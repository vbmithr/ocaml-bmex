open Core
open Async
open Bitmex_types

let src = Logs.Src.create "bmex.compta"
module Log = (val Logs.src_log src : Logs.LOG)
module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

module Cfg = struct
  type cfg = {
    key: string ;
    secret: string ;
    passphrase: string [@default ""];
    quote: (string * int) list [@default []];
  } [@@deriving sexp]

  type t = (string * cfg) list [@@deriving sexp]
end

let default_cfg = Filename.concat (Option.value_exn (Sys.getenv "HOME")) ".virtu"
let cfg =
  List.Assoc.find_exn ~equal:String.equal
    (Sexplib.Sexp.load_sexp_conv_exn default_cfg Cfg.t_of_sexp) "BMEXT"

(* let url = Uri.make ~scheme:"unix+kdb" ~path:"/tmp/kx.5042" () *)
let url = Uri.make ~scheme:"unix+kdb" ~host:"localhost" ~port:5042 ()

let side_of_string = function "buy" -> Fixtypes.Side.Buy | _ -> Sell
let string_of_side = function Fixtypes.Side.Buy -> "buy" | Sell -> "sell"

let ordType_of_string = function "limit" -> Fixtypes.OrdType.Limit | _ -> Market
let string_of_ordType = function Fixtypes.OrdType.Limit -> "limit" | _ -> "market"

let floats = Kx.(v float)
let timestamps = Kx.(v timestamp)
let syms = Kx.(v sym)
let sides = Kx.(conv (Array.map ~f:string_of_side) (Array.map ~f:side_of_string) (v sym))
let ordTypes = Kx.(conv (Array.map ~f:string_of_ordType) (Array.map ~f:ordType_of_string) (v sym))

let transfersw =
  let open Kx in
  t10 timestamps syms syms syms syms (v guid) (list (s char)) (list (s char)) floats floats

let kx_of_transfers transfers =
  let len = List.length transfers in
  let kinds = Array.create ~len "" in
  let times = Array.create ~len Ptime.epoch in
  let syms = Array.create ~len "" in
  let xchs = Array.create ~len "BMX" in
  let refids = Array.create ~len Uuidm.nil in
  let txids = Array.create ~len "" in
  let addrs = Array.create ~len "" in
  let statuses = Array.create ~len "" in
  let amounts = Array.create ~len Float.nan in
  let fees = Array.create ~len Float.nan in
  List.iteri transfers ~f:begin fun i ({ transactID; currency; transactType; amount; fee; transactStatus;
                                         address; tx; transactTime ; timestamp ; _ }:Transaction.t) ->
    let transactStatus =
      match String.split (Option.value_exn transactStatus) ~on:',' with
      | [status] -> status
      | _ -> "Completed" in
    kinds.(i) <- Option.value_exn transactType ;
    statuses.(i) <- transactStatus ;
    refids.(i) <- transactID ;
    times.(i) <- Option.(value ~default:Kx.np (first_some transactTime timestamp)) ;
    syms.(i) <- Option.value_exn currency ;
    txids.(i) <- Option.value_exn tx ;
    addrs.(i) <- Option.value_exn address ;
    amounts.(i) <- (Option.value_map amount ~default:Kx.nf ~f:(fun amount -> amount // 100_000_000));
    fees.(i) <- (Option.value_map fee ~default:Kx.nf ~f:(fun amount -> amount // 100_000_000)) ;
  end ;
  let open Kx in
  Kx_async.create (t3 (a sym) (a sym) transfersw)
    ("upd", "transfers", (times, syms, xchs, kinds, statuses, refids, txids, addrs, amounts, fees))

let retrieveTransfers w =
  let rec inner start =
    Bmex_rest.walletHistory ~testnet:true ~key:cfg.key ~secret:cfg.secret ~start ~count:10000 () >>= fun txs ->
    match List.length txs with
    | 0 -> Deferred.Or_error.return ()
    | len ->
      Log_async.app (fun m -> m "Found %d txs" len) >>= fun () ->
      Pipe.write w (kx_of_transfers txs) >>= fun () ->
      inner (start+len)
  in
  inner 0

let main () =
  Kx_async.with_connection url begin fun _ w ->
    retrieveTransfers w
  end >>= function
  | Error e -> Error.raise e
  | Ok _ -> Deferred.unit

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Command.async ~summary:"BMEX kdb+ compta" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param [] in
      fun () -> main ()
    ] end |>
  Command.run

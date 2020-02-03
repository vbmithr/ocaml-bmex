open Core
open Async
open Bitmex_types

let src = Logs.Src.create "bmex.compta"
module Log = (val Logs.src_log src : Logs.LOG)
module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

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

let key, secret =
  match String.split ~on:':' (Sys.getenv_exn "TOKEN_BMEXT") with
  | [key; secret] -> key, secret
  | _ -> assert false

let retrieveTransfers w =
  let rec inner start =
    Bmex_rest.walletHistory
      ~testnet:true ~key ~secret ~start ~count:10000 () >>= fun txs ->
    match List.length txs with
    | 0 -> Deferred.unit
    | len ->
      Log_async.app (fun m -> m "Found %d txs" len) >>= fun () ->
      Pipe.write w (kx_of_transfers txs) >>= fun () ->
      inner (start+len)
  in
  inner 0

let indexw =
  Kx.(t3 (v sym) (v timestamp) (v float))

let kx_of_index idxVals =
  let open Kx in
  let len = List.length idxVals in
  let syms = Array.create ~len "" in
  let origTss = Array.create ~len Ptime.epoch in
  let pxs = Array.create ~len Kx.nf in
  List.iteri idxVals ~f:begin fun i (t: Trade.t) ->
    syms.(i) <- t.symbol ;
    origTss.(i) <- t.timestamp ;
    Option.iter t.price ~f:(fun v -> pxs.(i) <- v) ;
  end ;
  Kx_async.create (t3 (a sym) (a sym) indexw)
    (".u.upd", "index", (syms, origTss, pxs))

let retrieveIndex w symbol =
  let rec inner startTime =
    Bmex_rest.trades ~reverse:true ~startTime
      ~testnet:false ~count:1000 symbol >>= function
    | [] -> Deferred.unit
    | (h :: _) as idxVals ->
      let len = List.length idxVals in
      Log_async.app begin fun m ->
        m "Found %d idx values (%a)" len Ptime.pp h.timestamp
      end >>= fun () ->
      Pipe.write w (kx_of_index (List.rev idxVals)) >>= fun () ->
      let nextTs = Caml.Option.get @@
          Ptime.add_span h.timestamp (Ptime.Span.of_int_s 1) in
      inner nextTs
  in
  let startTime =
    match Ptime.of_rfc3339 (Sys.getenv_exn "STARTTIME") with
    | Ok (a, _, _) -> a
    | _ -> assert false in
  inner startTime

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Command.group ~summary:"bmex army knife" [
    "compta",
    (Command.async ~summary:"Accounting experiment" begin
        let open Command.Let_syntax in
        [%map_open
          let () = Logs_async_reporter.set_level_via_param [] in
          fun () ->
            Kx_async.with_connection url (fun _ w -> retrieveTransfers w)
        ] end) ;
    "index",
    (Command.async ~summary:"Store historical indices in DB" begin
        let open Command.Let_syntax in
        [%map_open
          let () = Logs_async_reporter.set_level_via_param []
          and symbols = anon (sequence ("symbol" %: string)) in
          fun () ->
            let url = Uri.make
                ~userinfo:"tickerplant:pass"
                ~scheme:"kdb" ~host:"localhost" ~port:6000 () in
            Kx_async.with_connection url (fun _ w ->
            Deferred.List.iter symbols ~f:(fun symbol -> retrieveIndex w symbol))
        ] end) ;
  ] |> Command.run

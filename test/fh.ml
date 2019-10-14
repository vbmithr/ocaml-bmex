open Core
open Async

open Bmex_ws

let src = Logs.Src.create "bmex.fh"  ~doc:"BitMEX API - Toy feedhandler"
module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

let process_user_cmd w =
  let process s =
    match String.split s ~on:' ' with
    | "subscribe" :: subs ->
      let subs = List.map subs ~f:Request.Sub.of_string in
      Pipe.write w (Request.Subscribe subs)
    | "unsubscribe" :: subs ->
      let subs = List.map subs ~f:Request.Sub.of_string in
      Pipe.write w (Request.Unsubscribe subs)
    | h :: _ -> Log_async.err (fun m -> m "Unknown command %s" h)
    | [] -> Log_async.err (fun m -> m "Empty command")
  in
  let rec loop () = Reader.(read_line @@ Lazy.force stdin) >>= function
    | `Eof -> Deferred.unit
    | `Ok line -> process line >>= loop
  in
  loop ()

let side_encoding =
  let open Kx in
  conv
    (function Fixtypes.Side.Buy -> "bid" | Sell -> "ask")
    (function "bid" -> Buy | _ -> Sell)
    (a sym)

let longopt =
  let open Kx in
  conv
    (function Some i -> Int64.of_int i | None -> nj)
    (function | i when i = nj -> None | i -> Some (Int64.to_int_exn i))
    (a long)

let flopt =
  let open Kx in
  conv
    (function Some i -> i | None -> nf)
    (function | i when i = nf -> None | i -> Some i)
    (a float)

let row =
  let open Kx in
  conv
    (fun { Quote.symbol ; id ; side ; size; price } ->
       (symbol, id, side, size, price))
    (fun (symbol, id, side, size, price) ->
       { Quote.symbol ; id ; side ; size; price })
    (t5 (a sym) (a long) side_encoding longopt flopt)

let process_msgs kw msg =
  match msg with
  | Response.Update { table; action = _ ; data } -> begin
      match table, data with
      | OrderBookL2, Quotes qs ->
        Log_async.info (fun m -> m "%a" Response.pp msg) >>= fun () ->
        let open Kx in
        Pipe.write kw (Kx_async.create (t2 (a sym) (list row))  ("upd", qs))
      | _ -> Deferred.unit
    end
  | Error _ ->
    Log_async.err (fun m -> m "%a" Response.pp msg)
  | _ ->
    Log_async.info (fun m -> m "%a" Response.pp msg)

let main testnet symbol =
  Kx_async.Async.with_connection
    (Uri.make ~scheme:"kdb" ~host:"localhost" ~port:5042 ()) ~f:begin fun { w = kw; _ } ->
    Bmex_ws_async.with_connection ~testnet
      ~topics:[Request.Sub.create ~symbol Topic.OrderBookL2] begin fun r w ->
      Deferred.all_unit [
        process_user_cmd w ;
        Pipe.iter r ~f:(process_msgs kw)
      ]
    end
  end >>= function
  | Error e -> Log_async.err (fun m -> m "%a" Error.pp e)
  | Ok _ -> Deferred.unit

let () =
  Command.async ~summary:"BitMEX toy feed handler" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param []
      and testnet = flag "testnet" no_arg ~doc:" Use testnet"
      and sym = anon ("symbol" %: string) in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main testnet sym
    ] end |>
  Command.run

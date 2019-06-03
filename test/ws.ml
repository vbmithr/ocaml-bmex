open Core
open Async

open Bmex_ws
open Bmex_ws_async

let src = Logs.Src.create "bmex.ws-test"  ~doc:"BitMEX API - WS test application"
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

let main () =
  with_connection begin fun r w ->
    let log_incoming msg =
      Log_async.info (fun m -> m "%a" Response.pp msg) in
    Deferred.all_unit [
      process_user_cmd w ;
      Pipe.iter r ~f:log_incoming
    ]
  end

let () =
  Command.async ~summary:"BitMEX WS client" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param None in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main ()
    ] end |>
  Command.run

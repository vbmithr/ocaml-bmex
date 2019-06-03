open Core
open Async

open Bmex_ws
open Bmex_ws_async

let src = Logs.Src.create "bmex.ws-test"  ~doc:"BitMEX API - WS test application"
module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

let process_user_cmd _w =
  let process s =
    match String.split s ~on:' ' with
    (* | "unsubscribe" :: chanid :: _ ->
     *   let chanid = int_of_string chanid in
     *   Pipe.write w (Unsubscribe { chanid ; reqid = None })
     * | "ping" :: v :: _ ->
     *   Pipe.write w (Ping (int_of_string_opt v))
     * | "ping" :: _ ->
     *   Pipe.write w (Ping None)
     * | "trades" :: pair ->
     *   let pairs = List.map ~f:Pair.of_string_exn pair in
     *   Pipe.write w (Subscribe (trades pairs))
     * | "books" :: pair ->
     *   let pairs = List.map ~f:Pair.of_string_exn pair in
     *   Pipe.write w (Subscribe (book10 pairs)) *)
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
      Log_async.debug (fun m -> m "%a" Response.pp msg) in
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

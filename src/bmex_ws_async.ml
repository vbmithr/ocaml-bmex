open Core
open Async

open Bmex
open Bmex_ws

let src = Logs.Src.create "bmex.ws.async" ~doc:"BitMEX API - Websockets"
module Log = (val Logs.src_log src : Logs.LOG)
module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

let url_of_opts testnet md =
  Uri.with_path
    (if testnet then testnet_url else url)
    (if md then "realtimemd" else "realtime")

let connect
    ?(buf=Bi_outbuf.create 4096)
    ?(query_params=[])
    ?auth
    ?(testnet=false)
    ?(md=false)
    ?(topics=[]) () =
  let url = url_of_opts testnet md in
  let auth_params = match auth with
    | None -> []
    | Some (key, secret) ->
      Crypto.mk_query_params ~key ~secret ~verb:Get url in
  let query_params =
    match md, topics with
    | true, _ -> []
    | false, [] -> auth_params @ query_params
    | false, topics -> ["subscribe", List.map ~f:Request.Sub.to_string topics] @
                       auth_params @ query_params in
  let url = Uri.add_query_params url query_params in
  Deferred.Or_error.map (Fastws_async.EZ.connect url)
    ~f:begin fun { r; w; cleaned_up } ->
      let client_read = Pipe.map r ~f:begin fun msg ->
          Yojson_encoding.destruct_safe
            Response.encoding (Yojson.Safe.from_string ~buf msg)
        end in
      let ws_read, client_write = Pipe.create () in
      don't_wait_for
        (Pipe.closed client_write >>| fun () -> Pipe.close w) ;
      don't_wait_for @@
      Pipe.transfer ws_read w ~f:begin fun r ->
        let doc = Yojson.Safe.to_string ~buf
            (Yojson_encoding.construct Request.encoding r) in
        Log.debug (fun m -> m "-> %s" doc) ;
        doc
      end ;
      client_read, client_write, cleaned_up
    end

let connect_exn ?buf ?query_params ?auth ?testnet ?md ?topics () =
  connect ?buf ?query_params ?auth ?testnet ?md ?topics () >>= function
  | Error e -> Error.raise e
  | Ok a -> return a

let with_connection
    ?(buf=Bi_outbuf.create 4096)
    ?(query_params=[])
    ?auth
    ?(testnet=false)
    ?(md=false)
    ?(topics=[]) f =
  let url = url_of_opts testnet md in
  let auth_params = match auth with
    | None -> []
    | Some (key, secret) ->
      Crypto.mk_query_params ~key ~secret ~verb:Get url in
  let query_params =
    match md, topics with
    | true, _ -> []
    | false, [] -> auth_params @ query_params
    | false, topics -> ["subscribe", List.map ~f:Request.Sub.to_string topics] @
                       auth_params @ query_params in
  let url = Uri.add_query_params url query_params in
  Fastws_async.EZ.with_connection url ~f:begin fun r w ->
    let client_read = Pipe.map r ~f:begin fun msg ->
        Yojson_encoding.destruct_safe Response.encoding (Yojson.Safe.from_string ~buf msg)
      end in
    let ws_read, client_write = Pipe.create () in
    don't_wait_for @@
    Pipe.transfer ws_read w ~f:begin fun r ->
      let doc =
        Yojson.Safe.to_string ~buf (Yojson_encoding.construct Request.encoding r) in
      Log.debug (fun m -> m "-> %s" doc) ;
      doc
    end ;
    f client_read client_write
  end

let with_connection_exn
    ?buf ?query_params ?auth ?testnet ?md ?topics f =
  with_connection
    ?buf ?query_params ?auth ?testnet ?md ?topics f >>= function
  | Error e -> Error.raise e
  | Ok a -> return a

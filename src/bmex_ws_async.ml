open Core
open Async

open Bmex
open Bmex_ws

let src = Logs.Src.create "bmex.ws.async" ~doc:"BitMEX API - Websockets"
module Log = (val Logs.src_log src : Logs.LOG)
module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

module T = struct
  type t = {
    r: Response.t Pipe.Reader.t ;
    w: Request.t Pipe.Writer.t ;
  }

  let create r w = { r; w }

  module Address = Uri_sexp

  let is_closed { r; w } = Pipe.(is_closed r && is_closed w)
  let close { r; w } =
    Pipe.close w ;
    Pipe.close_read r ;
    Deferred.unit
  let close_finished { r; w } =
    Deferred.all_unit [Pipe.closed r;
                       Pipe.closed w]
end
include T

let is_md url = String.equal (Uri.path url) "realtimemd"

let connect
    ?(buf=Bi_outbuf.create 4096)
    ?(query_params=[])
    ?auth
    ?(topics=[]) url =
  let auth_params = match auth with
    | None -> []
    | Some (key, secret) ->
      Crypto.mk_query_params ~key ~secret ~verb:Get url in
  let query_params =
    match is_md url, topics with
    | true, _ -> []
    | false, [] -> auth_params @ query_params
    | false, topics -> ["subscribe", List.map ~f:Request.Sub.to_string topics] @
                       auth_params @ query_params in
  let url = Uri.add_query_params url query_params in
  Deferred.Or_error.map (Fastws_async.EZ.connect url)
    ~f:begin fun { r; w; _ } ->
      let client_read = Pipe.map r ~f:begin fun msg ->
          Yojson_encoding.destruct_safe
            Response.encoding (Yojson.Safe.from_string ~buf msg)
        end in
      let client_write = Pipe.create_writer begin fun ws_read ->
          Pipe.transfer ws_read w ~f:begin fun r ->
            let doc = Yojson.Safe.to_string ~buf
                (Yojson_encoding.construct Request.encoding r) in
            Log.debug (fun m -> m "-> %s" doc) ;
            doc
          end
        end in
      (Pipe.closed client_write >>> fun () -> Pipe.close w) ;
      create client_read client_write
    end

module Persistent = struct
  include Persistent_connection_kernel.Make(T)

  let create' ~server_name ?on_event ?retry_delay ?buf ?query_params ?auth ?topics =
    create ~server_name ?on_event ?retry_delay
      ~connect:(connect ?buf ?query_params ?auth ?topics)
end

let connect_exn ?buf ?query_params ?auth ?topics url =
  connect ?buf ?query_params ?auth ?topics url >>= function
  | Error e -> Error.raise e
  | Ok a -> return a

let with_connection
    ?(buf=Bi_outbuf.create 4096)
    ?(query_params=[])
    ?auth
    ?(topics=[]) ~f url =
  let auth_params = match auth with
    | None -> []
    | Some (key, secret) ->
      Crypto.mk_query_params ~key ~secret ~verb:Get url in
  let query_params =
    match is_md url, topics with
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
    ?buf ?query_params ?auth ?topics ~f url =
  with_connection
    ?buf ?query_params ?auth ?topics ~f url >>= function
  | Error e -> Error.raise e
  | Ok a -> return a

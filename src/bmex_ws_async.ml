open Bmex
open Bmex_ws

let of_string ?buf msg =
  Yojson_encoding.destruct_safe Response.encoding (Yojson.Safe.from_string ?buf msg)
let to_string ?buf msg =
  Yojson.Safe.to_string ?buf (Yojson_encoding.construct Request.encoding msg)

module Conn = (Fastws_async.MakePersistent (struct type r = Response.t type w = Request.t end))
module Persistent = Persistent_connection_kernel.Make(Conn)

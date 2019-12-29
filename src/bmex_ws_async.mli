open Bmex_ws

val of_string : ?buf:Bi_outbuf.t -> string -> Response.t
val to_string : ?buf:Bi_outbuf.t -> Request.t -> string

module Persistent : Persistent_connection_kernel.S
  with type address = Uri.t
   and type conn = (Response.t, Request.t) Fastws_async.t

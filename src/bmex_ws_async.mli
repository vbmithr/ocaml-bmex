open Async

open Bmex_ws

val connect :
  ?buf:Bi_outbuf.t ->
  ?query_params:(string * string list) list ->
  ?auth:string * string ->
  ?testnet:bool ->
  ?md:bool ->
  ?topics:Request.Sub.t list -> unit ->
  (Response.t Pipe.Reader.t *
   Request.t Pipe.Writer.t * unit Deferred.t) Deferred.Or_error.t

val connect_exn :
  ?buf:Bi_outbuf.t ->
  ?query_params:(string * string list) list ->
  ?auth:string * string ->
  ?testnet:bool ->
  ?md:bool ->
  ?topics:Request.Sub.t list -> unit ->
  (Response.t Pipe.Reader.t *
   Request.t Pipe.Writer.t * unit Deferred.t) Deferred.t

val with_connection :
  ?buf:Bi_outbuf.t ->
  ?query_params:(string * string list) list ->
  ?auth:string * string ->
  ?testnet:bool ->
  ?md:bool ->
  ?topics:Request.Sub.t list ->
  (Response.t Pipe.Reader.t -> Request.t Pipe.Writer.t  -> 'a Deferred.t) ->
  'a Deferred.Or_error.t

val with_connection_exn :
  ?buf:Bi_outbuf.t ->
  ?query_params:(string * string list) list ->
  ?auth:string * string ->
  ?testnet:bool ->
  ?md:bool ->
  ?topics:Request.Sub.t list ->
  (Response.t Pipe.Reader.t -> Request.t Pipe.Writer.t  -> 'a Deferred.t) ->
  'a Deferred.t

open Core
open Async

val position :
  ?buf:Bi_outbuf.t -> ?log:Log.t ->
  testnet:bool -> key:string -> secret:Cstruct.t ->
  unit -> Yojson.Safe.json Deferred.Or_error.t

val submit_order :
  ?buf:Bi_outbuf.t -> ?log:Log.t ->
  testnet:bool -> key:string -> secret:Cstruct.t ->
  Yojson.Safe.json list -> Yojson.Safe.json Deferred.Or_error.t

val update_order :
  ?buf:Bi_outbuf.t -> ?log:Log.t ->
  testnet:bool -> key:string -> secret:Cstruct.t ->
  Yojson.Safe.json list -> Yojson.Safe.json Deferred.Or_error.t

val cancel_order :
  ?buf:Bi_outbuf.t -> ?log:Log.t ->
  testnet:bool -> key:string -> secret:Cstruct.t ->
  Uuid.t -> Yojson.Safe.json Deferred.Or_error.t

val cancel_all_orders :
  ?buf:Bi_outbuf.t -> ?log:Log.t ->
  ?symbol:string -> ?filter:Yojson.Safe.json ->
  testnet:bool -> key:string -> secret:Cstruct.t ->
  unit -> Yojson.Safe.json Deferred.Or_error.t

val cancel_all_orders_after :
  ?buf:Bi_outbuf.t -> ?log:Log.t ->
  testnet:bool -> key:string -> secret:Cstruct.t ->
  int -> Yojson.Safe.json Deferred.Or_error.t

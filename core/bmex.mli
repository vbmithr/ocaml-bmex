open Async

val url : Uri.t
val testnet_url : Uri.t

module Crypto : sig
  type api = Rest | Ws
  type verb = Get | Post | Put | Delete

  val sign :
    ?log:Log.t ->
    ?data:string ->
    secret:Cstruct.t ->
    verb:verb ->
    endp:string ->
    api -> int * string

  val mk_query_params :
    ?log:Log.t ->
    ?data:string ->
    key:string ->
    secret:Cstruct.t ->
    api ->
    verb ->
    Uri.t -> (string * string list) list
end

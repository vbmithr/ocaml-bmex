open Core
open Async

val url : Uri.t
val testnet_url : Uri.t

val time_encoding : Time_ns.t Json_encoding.encoding

type verb = Get | Post | Put | Delete
val show_verb : verb -> string

module Side : sig
  type t = [`Buy | `Sell]

  val of_string : string -> t
  val to_string : t -> string
  val show : t -> string
  val pp : Format.formatter -> t -> unit
end

module OrderBook : sig
  module Deprecated : sig
    type t = {
      symbol: string ;
      level: int ;
      bidSize: int option ;
      bidPrice: float option ;
      askSize: int option ;
      askPrice: float option ;
      timestamp: Time_ns.t ;
    }
  end

  module L2 : sig
    type t = {
      symbol: string ;
      id: int ;
      side: Side.t ;
      size: int option ;
      price: float option ;
    }
  end
end

module Quote : sig
  type t = {
    timestamp: string;
    symbol: string;
    bidPrice: float option ;
    bidSize: int option ;
    askPrice: float option ;
    askSize: int option ;
  }

  val merge : t -> t -> t
end

module Trade : sig
  type t = {
    symbol: string;
    timestamp: Time_ns.t;
    price: float;
    size: int;
    side: Side.t;
  }
end

module Crypto : sig
  type api = Rest | Ws

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
    api:api ->
    verb:verb ->
    Uri.t -> (string * string list) list
end

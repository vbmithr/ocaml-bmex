open Core
open Async

val url : Uri.t
val testnet_url : Uri.t

val time_encoding : Time_ns.t Json_encoding.encoding

type verb = Get | Post | Put | Delete
val show_verb : verb -> string

module Side : sig
  type t = [`Buy | `Sell]

  val of_string : string -> t option
  val to_string : t -> string
  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val encoding : t option Json_encoding.encoding
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
      side: Side.t option ;
      size: int option ;
      price: float option ;
    }

    val encoding : t Json_encoding.encoding
  end
end

module Quote : sig
  type t = {
    timestamp: Time_ns.t ;
    symbol: string ;
    bidPrice: float option ;
    bidSize: int option ;
    askPrice: float option ;
    askSize: int option ;
  }

  val encoding : t Json_encoding.encoding
  val merge : t -> t -> t
end

module Trade : sig
  type t = {
    timestamp: Time_ns.t;
    symbol: string;
    side: Side.t option ;
    size: int;
    price: float;
  }

  val encoding : t Json_encoding.encoding
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

module OrderType : sig
  type t = [
    | `order_type_market
    | `order_type_limit
    | `order_type_stop
    | `order_type_stop_limit
    | `order_type_market_if_touched
  ]

  val to_string : t -> string
  val of_string : string -> t
end

module TimeInForce : sig
  type t = [
    | `tif_day
    | `tif_good_till_canceled
    | `tif_all_or_none
    | `tif_immediate_or_cancel
    | `tif_fill_or_kill
  ]

  val to_string : t -> string
  val of_string : string -> t
end

module ExecInst : sig
  type t = [
    | `MarkPrice
    | `LastPrice
    | `IndexPrice
  ]

  val to_string : t -> string
  val of_dtc :
    ?p1:float -> ?p2:float -> OrderType.t -> Yojson.Safe.json
end

val p1_p2_of_bitmex :
  ord_type:OrderType.t -> stopPx:float -> price:float ->
  float option * float option
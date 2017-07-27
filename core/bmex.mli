open Core
open Async

val url : Uri.t
val testnet_url : Uri.t

module Encoding : sig
  include module type of Json_encoding.Make(Json_repr.Yojson)

  val destruct_safe :
    't Json_encoding.encoding -> ?log:Log.t -> Yojson.Safe.json -> 't

  val any_to_yojson : Json_repr.any -> Yojson.Safe.json
  val yojson_to_any : Yojson.Safe.json -> Json_repr.any

  val time : Time_ns.t Json_encoding.encoding
  val uint : int Json_encoding.encoding
  val uuid : Uuid.t Json_encoding.encoding
end

type verb = Get | Post | Put | Delete
val show_verb : verb -> string

module Side : sig
  type t = [ `buy | `sell | `buy_sell_unset ]

  val of_string : string -> t
  val to_string : t -> string
  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val encoding : t Json_encoding.encoding
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

    val encoding : t Json_encoding.encoding
    val of_yojson : ?log:Log.t -> Yojson.Safe.json -> t
    val to_yojson : t -> Yojson.Safe.json
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
  val of_yojson : ?log:Log.t -> Yojson.Safe.json -> t
  val to_yojson : t -> Yojson.Safe.json
  val merge : t -> t -> t
end

module Trade : sig
  type t = {
    timestamp: Time_ns.t;
    symbol: string;
    side: Side.t ;
    size: int;
    price: float;
  }

  val encoding : t Json_encoding.encoding
  val of_yojson : ?log:Log.t -> Yojson.Safe.json -> t
  val to_yojson : t -> Yojson.Safe.json
end

module Crypto : sig
  type api = Rest | Ws

  val sign :
    ?log:Log.t ->
    ?data:string ->
    secret:string ->
    verb:verb ->
    endp:string ->
    api -> int * string

  val mk_query_params :
    ?log:Log.t ->
    ?data:string ->
    key:string ->
    secret:string ->
    api:api ->
    verb:verb ->
    Uri.t -> (string * string list) list
end

module OrderType : sig
  type t = [
    | `order_type_unset
    | `order_type_market
    | `order_type_limit
    | `order_type_stop
    | `order_type_stop_limit
    | `order_type_market_if_touched
  ]

  val to_string : t -> string
  val of_string : string -> t
  val encoding : t Json_encoding.encoding

  val to_p1_p2 :
    stopPx:float ->
    price:float ->
    t ->
    float option * float option

  val to_price_stopPx :
    ?p1:float -> ?p2:float -> t -> float option * float option
end

module TimeInForce : sig
  type t = [
    | `tif_unset
    | `tif_day
    | `tif_good_till_canceled
    | `tif_all_or_none
    | `tif_immediate_or_cancel
    | `tif_fill_or_kill
    | `tif_good_till_date_time
  ]

  val to_string : t -> string
  val of_string : string -> t
  val encoding : t Json_encoding.encoding
end

module ExecInst : sig
  type t =
    | ParticipateDoNotInitiate
    | AllOrNone
    | MarkPrice
    | LastPrice
    | IndexPrice
    | Close
    | ReduceOnly
    | Fixed

  val to_string : t -> string
  val of_string : string -> t
  val encoding : t Json_encoding.encoding
end

module ContingencyType : sig
  type t =
    | OCO (* OneCancelsTheOther *)
    | OTO (* OneTriggersTheOther *)
    | OUOA (* OneUpdatesTheOtherAbsolute *)
    | OUOP (* OneUpdatesTheOtherProportional *)

  val encoding : t Json_encoding.encoding
end

module PegPriceType : sig
  type t =
    | LastPeg
    | MidPricePeg
    | MarketPeg
    | PrimaryPeg
    | TrailingStopPeg

  val encoding : t Json_encoding.encoding
end

module OrdStatus : sig
  type t =
    | New
    | PartiallyFilled
    | Filled
    | DoneForDay
    | Canceled
    | PendingCancel
    | Stopped
    | Rejected
    | Suspended
    | PendingNew
    | Calculated
    | Expired
    | AcceptedForBidding
    | PendingReplace
  [@@deriving sexp]

  val show : t -> string
  val of_string : string -> t
end

module ExecType : sig
  type t =
    | New
    | Trade
    | Canceled
    | Replaced
    | Restated
    | Rejected
    | TriggeredOrActivatedBySystem
    | Funding
    | Settlement
  [@@deriving sexp]

  val show : t -> string
  val of_string : string -> t
end

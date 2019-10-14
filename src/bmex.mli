val url : Uri.t
val testnet_url : Uri.t

module Ptime : sig
  include module type of Ptime
    with type t = Ptime.t
     and type span = Ptime.span

  val t_of_sexp : Sexplib.Sexp.t -> Ptime.t
  val sexp_of_t : Ptime.t -> Sexplib.Sexp.t
  val encoding : t Json_encoding.encoding
end

module Uuidm : sig
  include module type of Uuidm
    with type t = Uuidm.t

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val encoding : t Json_encoding.encoding
end

module Yojson : sig
  module Safe : sig
    include module type of Yojson.Safe
      with type t = Yojson.Safe.t

    val t_of_sexp : Sexplib.Sexp.t -> t
    val sexp_of_t : t -> Sexplib.Sexp.t
  end
end

module Yojson_encoding : sig
  include module type of Json_encoding.Make(Json_repr.Yojson)

  val destruct_safe :
    't Json_encoding.encoding -> Yojson.Safe.t -> 't

  val any_to_yojson : Json_repr.any -> Yojson.Safe.t
  val yojson_to_any : Yojson.Safe.t -> Json_repr.any
end

type verb = Get | Post | Put | Delete

val pp_verb : Format.formatter -> verb -> unit
val show_verb : verb -> string

module Side : sig
  type t = [ `buy | `sell | `buy_sell_unset ]

  val of_string : string -> t
  val to_string : t -> string
  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val encoding : t Json_encoding.encoding
end

module Quote : sig
  type t = {
    timestamp: Ptime.t ;
    symbol: string ;
    bidPrice: float option ;
    bidSize: int option ;
    askPrice: float option ;
    askSize: int option ;
  }

  val encoding : t Json_encoding.encoding
    (* val merge : t -> t -> t *)
end

module Crypto : sig
  type api = Rest | Ws

  val sign :
    ?data:string ->
    secret:string ->
    verb:verb ->
    endp:string ->
    api -> int * string

  val mk_query_params :
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
    | `order_type_limit_if_touched
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
  type t = Fixtypes.TimeInForce.t [@@deriving sexp]

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
    | Unknown of string

  val to_string : t -> string
  val of_string : string -> t
  val encoding : t Json_encoding.encoding
end

module ContingencyType : sig
  type t = Fixtypes.ContingencyType.t
  val encoding : t Json_encoding.encoding
end

module PegPriceType : sig
  type t = Fixtypes.PegPriceType.t
  val encoding : t Json_encoding.encoding
end

module OrdStatus : sig
  type t = Fixtypes.OrdStatus.t [@@deriving sexp]

  val show : t -> string
  val of_string : string -> t
  val to_dtc : t ->
    [ `order_status_canceled
    | `order_status_filled
    | `order_status_open
    | `order_status_order_sent
    | `order_status_partially_filled
    | `order_status_pending_cancel
    | `order_status_pending_cancel_replace
    | `order_status_pending_child
    | `order_status_pending_open
    | `order_status_rejected
    | `order_status_unspecified ]
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
    | Suspended
    | Released
    | Insurance
    | Rebalance
    | Unknown of string
  [@@deriving sexp]

  val show : t -> string
  val of_string : string -> t
end

val side_encoding : Fixtypes.Side.t Json_encoding.encoding

module Trade : sig
  type t = {
    ts: Ptime.t ;
    symbol: string ;
    side: Fixtypes.Side.t ;
    size: int ;
    price: float ;
    tickDirection: Fixtypes.TickDirection.t ;
    trdMatchID: Uuidm.t ;
    grossValue: int64 ;
    homeNotional: float ;
    foreignNotional: float ;
  } [@@deriving sexp]

  val encoding : t Json_encoding.encoding
end

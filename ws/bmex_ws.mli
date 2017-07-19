open Core
open Async

module Topic : sig
  type t =
    (* private *)
    | PrivateNotifications
    | Account
    | Wallet
    | Affiliate
    | Margin
    | Position
    | Transact
    | Order
    | Execution
    (* public *)
    | Announcement
    | Connected
    | Chat
    | PublicNotifications
    | Instrument
    | Settlement
    | Funding
    | Insurance
    | Liquidation
    | OrderBookL2
    | OrderBook
    | OrderBook25
    | OrderBook10
    | Quote
    | Trade
    | QuoteBin1m
    | QuoteBin5m
    | QuoteBin1h
    | QuoteBin1d
    | TradeBin1m
    | TradeBin5m
    | TradeBin1h
    | TradeBin1d

  val of_string : string -> t
  val to_string : t -> string
  val show : t -> string
  val pp : Format.formatter -> t -> unit
end

module Request : sig
  module Sub : sig
    type t = {
      topic : Topic.t ;
      symbol : string option ;
    }

    val create : ?symbol:string -> Topic.t -> t
  end

  type t =
    | Subscribe of Sub.t list
    | Unsubscribe of Sub.t list
    | CancelAllAfter of int
    | AuthKey of {
        key : string ;
        nonce : int ;
        signature : string
      }

  val subscribe : Sub.t list -> t
  val unsubscribe : Sub.t list -> t
  val cancel_all_after : int -> t
  val authkey : key:string -> nonce:int -> signature:string -> t

  val encoding : t Json_encoding.encoding
  val to_yojson : t -> Yojson.Safe.json
  val of_yojson : Yojson.Safe.json -> t
end

module Response : sig
  module Welcome : sig
    type t = {
      version : string ;
      timestamp : Time_ns.t ;
    }
  end

  module Update : sig
    type action =
      | Partial
      | Update
      | Insert
      | Delete

    val action_of_string : string -> action
    val action_to_string : action -> string
    val show_action : action -> string
    val pp_action : Format.formatter -> action -> unit

    type t = {
      table : string ;
      action : action ;
      data : Yojson.Safe.json list ;
    }
  end

  module Response : sig
    type t = {
      success: bool option ;
      request : Request.t ;
      subscribe : Request.Sub.t option ;
    }
  end

  type t =
    | Welcome of Welcome.t
    | Error of string
    | Response of Response.t
    | Update of Update.t

  val encoding : t Json_encoding.encoding
  val to_yojson : t -> Yojson.Safe.json
  val of_yojson : Yojson.Safe.json -> t
end

module MD : sig
  type stream = {
    id : string ;
    topic : string
  }

  type t =
    | Message of { stream : stream ; payload : Yojson.Safe.json }
    | Subscribe of stream
    | Unsubscribe of stream

  val of_yojson : Yojson.Safe.json -> t
  val to_yojson : t -> Yojson.Safe.json

  val subscribe : id:string -> topic:string -> t
  val unsubscribe : id:string -> topic:string -> t
  val message : id:string -> topic:string -> payload:Yojson.Safe.json -> t
  val auth : id:string -> topic:string -> key:string -> secret:string -> t
end

val open_connection :
  ?buf:Bi_outbuf.t ->
  ?connected:unit Condition.t ->
  ?to_ws:Yojson.Safe.json Pipe.Reader.t ->
  ?query_params:(string * string list) list ->
  ?log:Log.t ->
  ?auth:string * string ->
  testnet:bool ->
  md:bool ->
  topics:string list ->
  unit -> Yojson.Safe.json Pipe.Reader.t


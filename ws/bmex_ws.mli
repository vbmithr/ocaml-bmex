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
  end
end

module Response : sig
  module Welcome : sig
    type t = {
      heartbeat : bool ;
      timeout : int ;
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

  type t =
    | Welcome of Welcome.t
    | Error of string
    | Response of Request.Sub.t
    | Update of Update.t

  val encoding : t Json_encoding.encoding
end

val open_connection :
  ?buf:Bi_outbuf.t ->
  ?connected:unit Condition.t ->
  ?to_ws:Yojson.Safe.json Pipe.Reader.t ->
  ?query_params:(string * string list) list ->
  ?log:Log.t ->
  ?auth:string * Cstruct.t ->
  testnet:bool ->
  md:bool ->
  topics:string list ->
  unit -> Yojson.Safe.json Pipe.Reader.t


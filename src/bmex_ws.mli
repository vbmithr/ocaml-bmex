open Bmex

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
    | OrderBookL2_25
    | OrderBookL2
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

module Quote : sig
  type t = {
    symbol: string ;
    id: int64 ;
    side: Fixtypes.Side.t ;
    size: int option ;
    price: float option ;
  } [@@deriving sexp]
end

module Request : sig
  module Sub : sig
    type t = {
      topic : Topic.t ;
      symbol : string option ;
    }

    val create : ?symbol:string -> Topic.t -> t
    val of_string : string -> t
    val to_string : t -> string
  end

  type t =
    | Subscribe of Sub.t list
    | Unsubscribe of Sub.t list
    | CancelAllAfter of int
    | AuthKeyExpires of {
        key : string ;
        expiresIn : string ;
        signature : string
      }

  val subscribe : Sub.t list -> t
  val unsubscribe : Sub.t list -> t
  val cancel_all_after : int -> t
  val authkey : key:string -> expiresIn:string -> signature:string -> t

  val encoding : t Json_encoding.encoding
  val to_string : ?buf:Bi_outbuf.t -> t -> string

end

module Response : sig
  module Welcome : sig
    type t = {
      version : string ;
      timestamp : Ptime.t ;
    } [@@deriving sexp]
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

    type data =
      | Quotes of Quote.t list
      | Trades of Trade.t list
      | Unknown of Yojson.Safe.t
    [@@deriving sexp]

    type t = {
      table : Topic.t ;
      action : action ;
      data : data ;
    } [@@deriving sexp]
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
  [@@deriving sexp]

  val pp : Format.formatter -> t -> unit
  val encoding : t Json_encoding.encoding
  val of_string : ?buf:Bi_outbuf.t -> string -> t

end

module MD : sig
  type stream = {
    id : string ;
    topic : string
  }

  type t =
    | Message of { stream : stream ; payload : Yojson.Safe.t }
    | Subscribe of stream
    | Unsubscribe of stream

  val of_yojson : Yojson.Safe.t -> t
  val to_yojson : t -> Yojson.Safe.t

  val subscribe : id:string -> topic:string -> t
  val unsubscribe : id:string -> topic:string -> t
  val message : id:string -> topic:string -> payload:Yojson.Safe.t -> t
  val auth : id:string -> topic:string -> key:string -> secret:string -> t
end

val mk_url :
  ?md:bool ->
  ?auth:string * string ->
  ?topics:Request.Sub.t list ->
  ?query_params:(string * string list) list -> Uri.t -> Uri.t

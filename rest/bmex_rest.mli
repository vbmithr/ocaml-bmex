open Core
open Async

open Bmex

module Execution : sig
  val trade_history :
    ?buf:Bi_outbuf.t ->
    ?log:Async.Log.t ->
    testnet:bool ->
    key:string ->
    secret:string ->
    ?startTime:Core.Time_ns.t ->
    ?endTime:Core.Time_ns.t ->
    ?start:Core.Int.t ->
    ?count:Core.Int.t ->
    ?symbol:string ->
    ?filter:Yojson.Safe.json ->
    ?reverse:Core.Bool.t ->
    unit ->
    (Cohttp.Response.t * Yojson.Safe.json list) Deferred.Or_error.t

  val all_trade_history :
    ?buf:Bi_outbuf.t ->
    ?log:Async.Log.t ->
    testnet:bool ->
    key:string ->
    secret:string ->
    ?symbol:string ->
    ?filter:Yojson.Safe.json ->
    unit ->
    (Cohttp.Response.t * Yojson.Safe.json list) Deferred.Or_error.t
end

module Instrument : sig
  val active_and_indices :
    ?buf:Bi_outbuf.t ->
    ?log:Log.t ->
    testnet:bool ->
    unit ->
    (Cohttp.Response.t * Yojson.Safe.json list) Deferred.Or_error.t
end

module Order : sig
  type t = {
    symbol : string ;
    orderQty : int ;
    displayQty : int option ;
    price : float option ;
    stopPx : float option ;
    clOrdID : string option ;
    contingencyType : (ContingencyType.t * string) option ;
    pegOffsetValue : float option ;
    pegPriceType : PegPriceType.t option ;
    ordType : OrderType.t ;
    timeInForce : TimeInForce.t ;
    execInst : ExecInst.t list ;
    text : string option ;
  }

  val create :
    ?displayQty:int ->
    ?price:float ->
    ?stopPx:float ->
    ?clOrdID:string ->
    ?contingencyType:(ContingencyType.t * string) ->
    ?pegOffsetValue:float ->
    ?pegPriceType:PegPriceType.t ->
    ?timeInForce:TimeInForce.t ->
    ?execInst:ExecInst.t list ->
    ?text:string ->
    symbol:string ->
    orderQty:int ->
    ordType:OrderType.t ->
    unit -> t

  val get_open_orders :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    testnet:bool ->
    key:string ->
    secret:string ->
    ?startTime:Core.Time_ns.t ->
    ?endTime:Core.Time_ns.t ->
    ?start:Core.Int.t ->
    ?count:Core.Int.t ->
    ?symbol:string ->
    ?filter:Yojson.Safe.json ->
    ?reverse:Core.Bool.t ->
    unit ->
    (Cohttp.Response.t * Yojson.Safe.json list) Deferred.Or_error.t

  val submit_bulk :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    testnet:bool ->
    key:string ->
    secret:string ->
    t list ->
    (Cohttp.Response.t * Yojson.Safe.json) Deferred.Or_error.t

  type amend = {
    orderID : Uuid.t option ;
    origClOrdID : string option ;
    clOrdID : string option ;
    orderQty : int option ;
    leavesQty : int option ;
    price : float option ;
    stopPx : float option ;
    pegOffsetValue : float option ;
    text : string option ;
  }

  val create_amend :
    ?origClOrdID:string ->
    ?clOrdID:string ->
    ?orderQty:int ->
    ?leavesQty:int ->
    ?price:float ->
    ?stopPx:float ->
    ?pegOffsetValue:float ->
    ?text:string ->
    ?orderID:Uuid.t ->
    unit -> amend

  val amend_bulk :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    testnet:bool -> key:string -> secret:string ->
    amend list ->
    (Cohttp.Response.t * Yojson.Safe.json) Deferred.Or_error.t

  val cancel :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    testnet:bool -> key:string -> secret:string ->
    ?orderIDs:Uuid.t list ->
    ?clOrdIDs:string list ->
    ?text:string -> unit ->
    (Cohttp.Response.t * Yojson.Safe.json) Deferred.Or_error.t

  val cancel_all :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    testnet:bool -> key:string -> secret:string ->
    ?symbol:string ->
    ?filter:Yojson.Safe.json ->
    ?text:string ->
    unit ->
    Cohttp.Response.t Deferred.Or_error.t

  val cancel_all_after :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    testnet:bool -> key:string -> secret:string ->
    Time_ns.Span.t ->
    Cohttp.Response.t Deferred.Or_error.t
end

module Position : sig
  val get :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    testnet:bool ->
    key:string ->
    secret:string ->
    ?filter:Yojson.Safe.json ->
    ?columns:string list ->
    ?count:Core.Int.t ->
    unit ->
    (Cohttp.Response.t * Yojson.Safe.json list) Deferred.Or_error.t
end

module Trade : sig
  val get :
    ?buf:Bi_outbuf.t ->
    ?log:Async.Log.t ->
    testnet:bool ->
    ?filter:Yojson.Safe.json ->
    ?columns:string list ->
    ?count:int ->
    ?start:int ->
    ?reverse:bool ->
    ?startTime:Time_ns.t ->
    ?endTime:Time_ns.t ->
    ?symbol:string ->
    unit ->
    (Cohttp.Response.t * Trade.t list) Deferred.Or_error.t
end

module ApiKey : sig
  module Permission : sig
    type t =
      | Perm of string
      | Dtc of string
  end

  type t = {
    id: string;
    secret: string;
    name: string;
    nonce: int;
    cidr: string;
    permissions: Permission.t list;
    enabled: bool;
    userId: int;
    created: Time_ns.t;
  }

  module Set : Set.S with type Elt.t = t

  val dtc :
    ?buf:Bi_outbuf.t ->
    ?log:Log.t ->
    ?username:string ->
    testnet:bool ->
    key:string ->
    secret:string ->
    unit ->
    (Cohttp.Response.t * t list) Deferred.Or_error.t
end


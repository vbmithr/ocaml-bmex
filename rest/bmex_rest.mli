open Core
open Async

open Bmex

module Execution : sig
  val trade_history :
    ?buf:Bi_outbuf.t ->
    ?log:Async.Log.t ->
    testnet:bool ->
    key:string ->
    secret:Cstruct.t ->
    ?startTime:Core.Time_ns.t ->
    ?endTime:Core.Time_ns.t ->
    ?start:Core.Int.t ->
    ?count:Core.Int.t ->
    ?symbol:string ->
    ?filter:Yojson.Safe.json ->
    ?reverse:Core.Bool.t ->
    unit ->
    (Cohttp.Response.t * Yojson.Safe.json) Deferred.Or_error.t
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
    qty : int ;
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
    ?execInst:ExecInst.t list ->
    ?text:string ->
    symbol:string ->
    qty:int ->
    ordType:OrderType.t ->
    timeInForce:TimeInForce.t ->
    unit -> t

  val submit_bulk :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    testnet:bool -> key:string -> secret:Cstruct.t ->
    t list ->
    (Cohttp.Response.t * Yojson.Safe.json) Deferred.Or_error.t

  type amend = {
    orderID : string ;
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
    orderID:string ->
    unit -> amend

  val amend_bulk :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    testnet:bool -> key:string -> secret:Cstruct.t ->
    amend list ->
    (Cohttp.Response.t * Yojson.Safe.json) Deferred.Or_error.t

  val cancel :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    testnet:bool -> key:string -> secret:Cstruct.t ->
    ?orderIDs:Uuid.t list ->
    ?clOrdIDs:string list ->
    ?text:string -> unit ->
    (Cohttp.Response.t * Yojson.Safe.json) Deferred.Or_error.t

  val cancel_all :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    testnet:bool -> key:string -> secret:Cstruct.t ->
    ?symbol:string ->
    ?filter:Yojson.Safe.json ->
    ?text:string ->
    unit ->
    (Cohttp.Response.t * Yojson.Safe.json) Deferred.Or_error.t

  val cancel_all_after :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    testnet:bool -> key:string -> secret:Cstruct.t ->
    Time_ns.Span.t ->
    (Cohttp.Response.t * Yojson.Safe.json) Deferred.Or_error.t
end

module Position : sig
  val get :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    testnet:bool ->
    key:string ->
    secret:Cstruct.t ->
    ?filter:Yojson.Safe.json ->
    ?columns:string list ->
    ?count:Core.Int.t ->
    unit ->
    (Cohttp.Response.t * Yojson.Safe.json) Deferred.Or_error.t
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


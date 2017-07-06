open Core
open Async

open Bmex

val position :
  ?buf:Bi_outbuf.t -> ?log:Log.t ->
  testnet:bool -> key:string -> secret:Cstruct.t ->
  unit -> Yojson.Safe.json Deferred.Or_error.t

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
    t list -> Yojson.Safe.json Deferred.Or_error.t

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
    amend list -> Yojson.Safe.json Deferred.Or_error.t

  val cancel :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    testnet:bool -> key:string -> secret:Cstruct.t ->
    ?orderIDs:Uuid.t list ->
    ?clOrdIDs:string list ->
    ?text:string -> unit ->
    Yojson.Safe.json Deferred.Or_error.t

  val cancel_all :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    testnet:bool -> key:string -> secret:Cstruct.t ->
    ?symbol:string ->
    ?filter:Yojson.Safe.json ->
    ?text:string ->
    unit -> Yojson.Safe.json Deferred.Or_error.t

  val cancel_all_after :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    testnet:bool -> key:string -> secret:Cstruct.t ->
    Time_ns.Span.t -> Yojson.Safe.json Deferred.Or_error.t
end

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
  unit -> Yojson.Safe.json Deferred.Or_error.t

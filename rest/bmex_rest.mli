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

  val update_bulk :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    testnet:bool -> key:string -> secret:Cstruct.t ->
    Yojson.Safe.json list -> Yojson.Safe.json Deferred.Or_error.t

  val cancel :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    testnet:bool -> key:string -> secret:Cstruct.t ->
    Uuid.t -> Yojson.Safe.json Deferred.Or_error.t

  val cancel_all :
    ?buf:Bi_outbuf.t -> ?log:Log.t ->
    ?symbol:string -> ?filter:Yojson.Safe.json ->
    testnet:bool -> key:string -> secret:Cstruct.t ->
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

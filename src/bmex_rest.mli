open Core
open Async

open Bmex
open Bitmex_types

val activeInstruments :
  ?buf:Bi_outbuf.t -> ?testnet:bool -> unit ->
  Instrument.t list Deferred.t

val activeAndIndices :
  ?buf:Bi_outbuf.t -> ?testnet:bool -> unit ->
  Instrument.t list Deferred.t

val trades :
  ?buf:Bi_outbuf.t ->
  ?testnet:bool ->
  ?filter:Yojson.Safe.t ->
  ?columns:string list ->
  ?count:int ->
  ?start:int ->
  ?reverse:bool ->
  ?startTime:Time_ns.t ->
  ?endTime:Time_ns.t ->
  string ->
  Trade.t list Deferred.t

val tradeHistory :
  ?buf:Bi_outbuf.t ->
  ?testnet:bool ->
  ?startTime:Time_ns.t ->
  ?endTime:Time_ns.t ->
  ?start:int ->
  ?count:int ->
  ?symbol:string ->
  ?filter:Yojson.Safe.t ->
  ?reverse:bool ->
  key:string ->
  secret:string ->
  unit ->
  Execution.t list Deferred.t
(** All balance affecting executions (trades, insurance,
    settlement). count must be [1;500]. *)

val executionHistory :
  ?buf:Bi_outbuf.t ->
  ?testnet:bool ->
  key:string ->
  secret:string ->
  symbol:string ->
  ts:Ptime.t ->
  unit ->
  Execution.t list Deferred.t
(** Execution history per symbol/day. Higher-level API compared to tradeHistory. *)

val wallet :
  ?buf:Bi_outbuf.t ->
  ?testnet:bool ->
  key:string ->
  secret:string ->
  unit ->
  Wallet.t Deferred.t
(** Get your current wallet (XBt) information. *)

val walletSummary :
  ?buf:Bi_outbuf.t ->
  ?testnet:bool ->
  key:string ->
  secret:string ->
  unit ->
  Wallet.t list Deferred.t
(** Get a split-out of wallet state (summarized history with
    deposits/withdrawals/RealisedPLNs). *)

val walletHistory :
  ?buf:Bi_outbuf.t ->
  ?testnet:bool ->
  ?start:int ->
  ?count:int ->
  key:string ->
  secret:string ->
  unit ->
  Transaction.t list Deferred.t
(** Get a detailed history of wallet movements (most precise). *)

val positions :
  ?buf:Bi_outbuf.t ->
  ?testnet:bool ->
  ?filter:Yojson.Safe.t ->
  ?columns:string list ->
  ?count:Core.Int.t ->
  key:string ->
  secret:string ->
  unit ->
  Position.t list Deferred.t

val openOrders :
  ?buf:Bi_outbuf.t ->
  ?testnet:bool ->
  ?startTime:Time_ns.t ->
  ?endTime:Time_ns.t ->
  ?start:int ->
  ?count:int ->
  ?symbol:string ->
  ?filter:Yojson.Safe.t ->
  ?reverse:Core.Bool.t ->
  key:string ->
  secret:string ->
  unit ->
  Order.t list Deferred.t

type order = {
  symbol : string ;
  orderQty : int ;
  displayQty : int option ;
  price : float option ;
  stopPx : float option ;
  clOrdID : Uuidm.t ;
  contingencyType : (ContingencyType.t * string) option ;
  pegOffsetValue : float option ;
  pegPriceType : PegPriceType.t option ;
  ordType : OrderType.t ;
  timeInForce : TimeInForce.t ;
  execInst : ExecInst.t list ;
  text : string option ;
}
(* OrderQty determines buy or sell. *)

val createOrder :
  ?displayQty:int ->
  ?price:float ->
  ?stopPx:float ->
  ?contingencyType:(ContingencyType.t * string) ->
  ?pegOffsetValue:float ->
  ?pegPriceType:PegPriceType.t ->
  ?timeInForce:TimeInForce.t ->
  ?execInst:ExecInst.t list ->
  ?text:string ->
  symbol:string ->
  orderQty:int ->
  ordType:OrderType.t ->
  Uuidm.t -> order

val submit :
  ?buf:Bi_outbuf.t ->
  ?testnet:bool ->
  key:string ->
  secret:string ->
  order list ->
  Order.t list Deferred.t

type amend = {
  orderID : Uuidm.t option ;
  origClOrdID : Uuidm.t option ;
  clOrdID : Uuidm.t option ;
  orderQty : int option ;
  leavesQty : int option ;
  price : float option ;
  stopPx : float option ;
  pegOffsetValue : float option ;
  text : string option ;
}

val createAmend :
  ?origClOrdID:Uuidm.t ->
  ?clOrdID:Uuidm.t ->
  ?orderQty:int ->
  ?leavesQty:int ->
  ?price:float ->
  ?stopPx:float ->
  ?pegOffsetValue:float ->
  ?text:string ->
  ?orderID:Uuidm.t ->
  unit -> amend

val amend :
  ?buf:Bi_outbuf.t ->
  ?testnet:bool -> key:string -> secret:string ->
  amend list ->
  Order.t list Deferred.t

val cancel :
  ?buf:Bi_outbuf.t ->
  ?testnet:bool ->
  ?orderIDs:Uuidm.t list ->
  ?clOrdIDs:Uuidm.t list ->
  ?text:string ->
  key:string -> secret:string -> unit ->
  Order.t list Deferred.t

val cancelAll :
  ?buf:Bi_outbuf.t ->
  ?testnet:bool ->
  ?symbol:string ->
  ?filter:Yojson.Safe.t ->
  ?text:string ->
  key:string -> secret:string ->
  unit ->
  Order.t list Deferred.t

type cancelAllAfter = {
  now: Ptime.t ;
  cancelTime: Ptime.t ;
}

val cancelAllAfter :
  ?buf:Bi_outbuf.t ->
  ?testnet:bool ->
  key:string -> secret:string ->
  Time_ns.Span.t ->
  cancelAllAfter Deferred.t

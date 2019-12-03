open Core
open Async

(* open Bmex *)
open Bitmex_types

val activeInstruments :
  ?buf:Bi_outbuf.t -> ?testnet:bool -> unit -> Instrument.t list Deferred.t

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

(* module Order : sig
 *   type t = {
 *     symbol : string ;
 *     orderQty : int ;
 *     displayQty : int option ;
 *     price : float option ;
 *     stopPx : float option ;
 *     clOrdID : string option ;
 *     contingencyType : (ContingencyType.t * string) option ;
 *     pegOffsetValue : float option ;
 *     pegPriceType : PegPriceType.t option ;
 *     ordType : OrderType.t ;
 *     timeInForce : TimeInForce.t ;
 *     execInst : ExecInst.t list ;
 *     text : string option ;
 *   }
 * 
 *   val create :
 *     ?displayQty:int ->
 *     ?price:float ->
 *     ?stopPx:float ->
 *     ?clOrdID:string ->
 *     ?contingencyType:(ContingencyType.t * string) ->
 *     ?pegOffsetValue:float ->
 *     ?pegPriceType:PegPriceType.t ->
 *     ?timeInForce:TimeInForce.t ->
 *     ?execInst:ExecInst.t list ->
 *     ?text:string ->
 *     symbol:string ->
 *     orderQty:int ->
 *     ordType:OrderType.t ->
 *     unit -> t
 * 
 *   val get_open_orders :
 *     ?extract_exn:bool ->
 *     ?buf:Bi_outbuf.t ->
 *     testnet:bool ->
 *     key:string ->
 *     secret:string ->
 *     ?startTime:Time_ns.t ->
 *     ?endTime:Time_ns.t ->
 *     ?start:int ->
 *     ?count:int ->
 *     ?symbol:string ->
 *     ?filter:Yojson.Safe.t ->
 *     ?reverse:Core.Bool.t ->
 *     unit ->
 *     (Cohttp.Response.t * Order.t list) Deferred.Or_error.t
 * 
 *   val submit_bulk :
 *     ?extract_exn:bool ->
 *     ?buf:Bi_outbuf.t ->
 *     testnet:bool ->
 *     key:string ->
 *     secret:string ->
 *     t list ->
 *     (Cohttp.Response.t * Order.t list) Deferred.Or_error.t
 * 
 *   type amend = {
 *     orderID : Uuidm.t option ;
 *     origClOrdID : string option ;
 *     clOrdID : string option ;
 *     orderQty : int option ;
 *     leavesQty : int option ;
 *     price : float option ;
 *     stopPx : float option ;
 *     pegOffsetValue : float option ;
 *     text : string option ;
 *   }
 * 
 *   val create_amend :
 *     ?origClOrdID:string ->
 *     ?clOrdID:string ->
 *     ?orderQty:int ->
 *     ?leavesQty:int ->
 *     ?price:float ->
 *     ?stopPx:float ->
 *     ?pegOffsetValue:float ->
 *     ?text:string ->
 *     ?orderID:Uuidm.t ->
 *     unit -> amend
 * 
 *   val amend_bulk :
 *     ?extract_exn:bool ->
 *     ?buf:Bi_outbuf.t ->
 *     testnet:bool -> key:string -> secret:string ->
 *     amend list ->
 *     (Cohttp.Response.t * Order.t list) Deferred.Or_error.t
 * 
 *   val cancel :
 *     ?extract_exn:bool ->
 *     ?buf:Bi_outbuf.t ->
 *     testnet:bool -> key:string -> secret:string ->
 *     ?orderIDs:Uuidm.t list ->
 *     ?clOrdIDs:string list ->
 *     ?text:string -> unit ->
 *     (Cohttp.Response.t * Order.t list) Deferred.Or_error.t
 * 
 *   val cancel_all :
 *     ?extract_exn:bool ->
 *     ?buf:Bi_outbuf.t ->
 *     testnet:bool -> key:string -> secret:string ->
 *     ?symbol:string ->
 *     ?filter:Yojson.Safe.t ->
 *     ?text:string ->
 *     unit ->
 *     Cohttp.Response.t Deferred.Or_error.t
 * 
 *   val cancel_all_after :
 *     ?extract_exn:bool ->
 *     ?buf:Bi_outbuf.t ->
 *     testnet:bool -> key:string -> secret:string ->
 *     Time_ns.Span.t ->
 *     Cohttp.Response.t Deferred.Or_error.t
 * end *)

(* module Position : sig
 *   val get :
 *     ?extract_exn:bool ->
 *     ?buf:Bi_outbuf.t ->
 *     testnet:bool ->
 *     key:string ->
 *     secret:string ->
 *     ?filter:Yojson.Safe.t ->
 *     ?columns:string list ->
 *     ?count:Core.Int.t ->
 *     unit ->
 *     (Cohttp.Response.t * Position.t list) Deferred.Or_error.t
 * end *)

(* module ApiKey : sig
 *   module Permission : sig
 *     type t =
 *       | Perm of string
 *       | Dtc of string
 *   end
 * 
 *   type t = {
 *     id: string;
 *     secret: string;
 *     name: string;
 *     nonce: int64;
 *     cidr: string;
 *     permissions: Permission.t list;
 *     enabled: bool;
 *     userId: int;
 *     created: Ptime.t;
 *   }
 * 
 *   module Set : Set.S with type Elt.t = t
 * 
 *   val dtc :
 *     ?extract_exn:bool ->
 *     ?buf:Bi_outbuf.t ->
 *     ?username:string ->
 *     testnet:bool ->
 *     key:string ->
 *     secret:string ->
 *     unit ->
 *     (Cohttp.Response.t * t list) Deferred.Or_error.t
 * end *)

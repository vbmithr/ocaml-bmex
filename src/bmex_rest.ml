open Core
open Async

(* let src = Logs.Src.create "bmex.rest" ~doc:"BitMEX API - REST" *)

open Bmex
open Bitmex_types

(* module Error = struct
 *   type t = {
 *     name: string;
 *     message: string;
 *   }
 * 
 *   let encoding =
 *     let open Json_encoding in
 *     conv
 *       (fun { name ; message } -> (name, message))
 *       (fun (name, message) -> { name ; message })
 *       (obj2
 *         (req "name" string)
 *         (req "message" string))
 * 
 *   let wrapped_encoding =
 *     let open Json_encoding in
 *     obj1 (req "error" encoding)
 * end *)

(* let mk_headers ?(data="") ?credentials ~verb uri =
 *   match credentials with
 *   | None -> None
 *   | Some (key, secret) ->
 *     let query_params =
 *       Crypto.mk_query_params ~data ~key ~secret ~api:Rest ~verb uri in
 *     ("content-type", "application/json") ::
 *     List.Assoc.map query_params ~f:List.hd_exn |>
 *     Cohttp.Header.of_list |>
 *     Option.some *)

(* let call
 *     ?extract_exn
 *     ?buf
 *     ?(span=Time_ns.Span.of_int_sec 1)
 *     ?(max_tries=3)
 *     ?(query=[])
 *     ?body
 *     ?credentials
 *     ~testnet
 *     ~verb
 *     path =
 *   let url = if testnet then testnet_url else url in
 *   let url = Uri.with_path url path in
 *   let url = Uri.with_query url query in
 *   let body_str =
 *     Option.map body ~f:(fun json -> Yojson.Safe.to_string ?buf json) in
 *   begin match body_str with
 *     | Some body_str ->
 *       Logs_async.debug ~src begin fun m ->
 *         m "%a %s -> %s" pp_verb verb path body_str
 *       end
 *     | _ -> Deferred.unit
 *   end >>= fun () ->
 *   let body = Option.map body_str ~f:Body.of_string in
 *   let headers = mk_headers ?data:body_str ?credentials ~verb url in
 *   let call () = match verb with
 *     | Get -> Client.get ?headers url
 *     | Post -> Client.post ?headers ~chunked:false ?body url
 *     | Put -> Client.put ?headers ~chunked:false ?body url
 *     | Delete -> Client.delete ?headers ~chunked:false ?body url in
 *   let rec inner_exn try_id =
 *     call () >>= fun (resp, body) ->
 *     Body.to_string body >>= fun body_str ->
 *     let status = Response.status resp in
 *     let status_code = C.Code.code_of_status status in
 *     if C.Code.is_success status_code then
 *       return (resp, Yojson.Safe.from_string ?buf body_str)
 *     else if C.Code.is_client_error status_code then begin
 *       let json = Yojson.Safe.(from_string ?buf body_str) in
 *       let { Error.name ; message } =
 *         Yojson_encoding.destruct_safe Error.wrapped_encoding json in
 *       failwithf "%s: %s" name message ()
 *     end
 *     else if C.Code.is_server_error status_code then begin
 *       let status_code_str = (C.Code.sexp_of_status_code status |> Sexplib.Sexp.to_string_hum) in
 *       Logs_async.err ~src begin fun m ->
 *         m "%a %s: %s" pp_verb verb path status_code_str
 *       end >>= fun () ->
 *       Clock_ns.after span >>= fun () ->
 *       if try_id >= max_tries then failwithf "%s %s: %s" (show_verb verb) path status_code_str ()
 *       else inner_exn @@ succ try_id
 *     end
 *     else failwithf "%s %s: Unexpected HTTP return status %s"
 *         (show_verb verb) path
 *         (C.Code.sexp_of_status_code status |> Sexplib.Sexp.to_string_hum) ()
 *   in
 *   Monitor.try_with_or_error ?extract_exn (fun () -> inner_exn 0) *)

(* module ApiKey = struct
 *   module Permission = struct
 *     type t =
 *       | Perm of string
 *       | Dtc of string
 *     [@@deriving sexp]
 * 
 *     let dtc_to_any username =
 *       Yojson_encoding.yojson_to_any
 *         (`List [`String "sierra-dtc"; `Assoc ["username", `String username]])
 * 
 *     let dtc_of_any any =
 *       match Yojson_encoding.any_to_yojson any with
 *       | `List [`String "sierra-dtc"; `Assoc ["username", `String username]] -> Dtc username
 *       | #Yojson.Safe.t -> invalid_arg "ApiKey.dtc_of_any"
 * 
 *     let encoding =
 *       let open Json_encoding in
 *       union [
 *         case string
 *           (function Perm s -> Some s | _ -> None)
 *           (fun s -> Perm s) ;
 *         case any_value
 *           (function Perm _ -> None | Dtc username -> Some (dtc_to_any username))
 *           (fun any -> dtc_of_any any) ;
 *       ]
 *   end
 * 
 *   module T = struct
 *     type t = {
 *       id: string;
 *       secret: string;
 *       name: string;
 *       nonce: int64;
 *       cidr: string;
 *       permissions: Permission.t list;
 *       enabled: bool;
 *       userId: int;
 *       created: Ptime.t;
 *     } [@@deriving sexp]
 * 
 *     let compare { id ; _ } { id = id2 ; _ } =
 *       String.compare id id2
 *   end
 * 
 *   include T
 *   module Set = Set.Make(T)
 * 
 *   let encoding =
 *     let open Json_encoding in
 *     conv
 *       (fun { id ; secret ; name ; nonce ; cidr ;
 *              permissions ; enabled ; userId ; created } ->
 *         (id, secret, name, nonce, cidr, permissions,
 *          enabled, userId, created))
 *       (fun (id, secret, name, nonce, cidr, permissions,
 *             enabled, userId, created) ->
 *         { id ; secret ; name ; nonce ; cidr ; permissions ;
 *           enabled ; userId ; created })
 *       (obj9
 *          (req "id" string)
 *          (req "secret" string)
 *          (req "name" string)
 *          (req "nonce" int53)
 *          (req "cidr" string)
 *          (req "permissions" (list Permission.encoding))
 *          (req "enabled" bool)
 *          (req "userId" int)
 *          (req "created" Ptime.encoding))
 * 
 *   let dtc ?extract_exn ?buf ?username ~testnet ~key ~secret () =
 *     let credentials = key, secret in
 *     let path = "/api/v1/apiKey/dtc/" ^
 *                match username with None -> "all" | Some _ -> "get" in
 *     let query = match username with None -> [] | Some u -> ["get", [u]] in
 *     call ?extract_exn ?buf ~credentials ~testnet ~query ~verb:Get path >>|
 *     Or_error.map ~f:begin fun (resp, json) ->
 *       resp, Yojson_encoding.destruct_safe (Json_encoding.list encoding) json
 *     end
 * end *)

module Instrument = struct
  let active ?buf ?(testnet=false) () =
    let url = if testnet then testnet_url else url in
    let url = Uri.with_path url "/api/v1/instrument/active" in
    Fastrest.simple_call_string ~meth:`GET url >>| fun (_resp, body) ->
    let instrs = Yojson.Safe.(Util.to_list (from_string ?buf body)) in
    List.map instrs ~f:Instrument.of_yojson
end

(* module Execution = struct
 *   let trade_history ?extract_exn ?buf ~testnet ~key ~secret
 *       ?startTime ?endTime ?start ?count ?symbol ?filter ?reverse () =
 *     let credentials = key, secret in
 *     let query = List.filter_opt [
 *         Option.map startTime ~f:(fun ts -> "startTime", [Time_ns.to_string ts]) ;
 *         Option.map endTime ~f:(fun ts -> "endTime", [Time_ns.to_string ts]) ;
 *         Option.map start ~f:(fun start -> "start", [Int.to_string start]) ;
 *         Option.map count ~f:(fun start -> "count", [Int.to_string start]) ;
 *         Option.map symbol ~f:(fun symbol -> "symbol", [symbol]) ;
 *         Option.map filter ~f:(fun filter -> "filter", [Yojson.Safe.to_string filter]) ;
 *         Option.map reverse ~f:(fun rev -> "reverse", [Bool.to_string rev]) ;
 *       ] in
 *     call ?extract_exn ?buf ~testnet ~credentials ~verb:Get ~query "/api/v1/execution/tradeHistory" >>|
 *     Or_error.map ~f:begin fun (resp, trades) ->
 *       resp, List.map (Yojson.Safe.Util.to_list trades) ~f:Execution.of_yojson
 *     end
 * 
 *   let all_trade_history ?extract_exn ?buf ~testnet ~key ~secret ?symbol ?filter () =
 *     let rec inner acc start =
 *       trade_history ?extract_exn ?buf ~testnet ~key ~secret ~start ~count:500 ?symbol ?filter () >>= function
 *       | Ok (resp, trades) ->
 *         let acc = acc @ trades in
 *         if List.length trades = 500 then
 *           inner acc (start + 500)
 *         else Deferred.Or_error.return (resp, acc)
 *       | Error err -> Deferred.Or_error.fail err
 *     in inner [] 0
 * end
 * 
 * module Order = struct
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
 *   let create ?displayQty ?price ?stopPx ?clOrdID ?contingencyType
 *       ?pegOffsetValue ?pegPriceType
 *       ?(timeInForce=Fixtypes.TimeInForce.GoodTillCancel) ?(execInst=[])
 *       ?text ~symbol ~orderQty ~ordType () =
 *     { symbol ; orderQty ; displayQty ; price ; stopPx ; clOrdID ; contingencyType ;
 *       pegOffsetValue ; pegPriceType ; ordType ; timeInForce ; execInst ; text }
 * 
 *   let encoding =
 *     let open Json_encoding in
 *     conv
 *       (fun { symbol ; orderQty ; displayQty ; price ; stopPx ; clOrdID ; contingencyType ;
 *              pegOffsetValue ; pegPriceType ; ordType ; timeInForce ; execInst ; text } ->
 *         let contingencyType, clOrdLinkID =
 *           match contingencyType with
 *           | None -> None, None
 *           | Some (contingencyType, clOrdLinkID) ->
 *             Some contingencyType, Some clOrdLinkID
 *         in
 *         let execInst =
 *           match execInst with
 *           | [] -> None
 *           | _ -> Some (String.concat ~sep:"," (List.map execInst ~f:ExecInst.to_string)) in
 *         (symbol, orderQty, displayQty, price, stopPx, clOrdID, clOrdLinkID,
 *          pegOffsetValue, pegPriceType, ordType),
 *         (timeInForce, execInst, contingencyType, text))
 *       (fun ((symbol, orderQty, displayQty, price, stopPx, clOrdID, clOrdLinkID,
 *              pegOffsetValue, pegPriceType, ordType),
 *             (timeInForce, execInst, contingencyType, text)) ->
 *         let contingencyType =
 *           match clOrdLinkID, contingencyType with
 *           | Some id, Some t -> Some (t, id)
 *           | _ -> None
 *         in
 *         let execInst =
 *           match execInst with
 *           | None -> []
 *           | Some execInst ->
 *             List.map (String.split ~on:',' execInst) ~f:ExecInst.of_string in
 *         { symbol ; orderQty ; displayQty ; price ; stopPx ; clOrdID ; contingencyType ;
 *           pegOffsetValue ; pegPriceType ; ordType ; timeInForce ; execInst ; text })
 *       (merge_objs
 *          (obj10
 *             (req "symbol" string)
 *             (req "orderQty" int)
 *             (opt "displayQty" int)
 *             (opt "price" float)
 *             (opt "stopPx" float)
 *             (opt "clOrdID" string)
 *             (opt "clOrdLinkID" string)
 *             (opt "pegOffsetValue" float)
 *             (opt "pegPriceType" PegPriceType.encoding)
 *             (req "ordType" OrderType.encoding))
 *          (obj4
 *             (dft "timeInForce" TimeInForce.encoding Fixtypes.TimeInForce.GoodTillCancel)
 *             (opt "execInst" string)
 *             (opt "contingencyType" ContingencyType.encoding)
 *             (opt "text" string)))
 * 
 *   let get_open_orders ?extract_exn ?buf ~testnet ~key ~secret
 *       ?startTime ?endTime ?start ?count ?symbol ?filter ?reverse () =
 *     let credentials = key, secret in
 *     let query = List.filter_opt [
 *         Option.map startTime ~f:(fun ts -> "startTime", [Time_ns.to_string ts]) ;
 *         Option.map endTime ~f:(fun ts -> "endTime", [Time_ns.to_string ts]) ;
 *         Option.map start ~f:(fun start -> "start", [Int.to_string start]) ;
 *         Option.map count ~f:(fun start -> "count", [Int.to_string start]) ;
 *         Option.map symbol ~f:(fun symbol -> "symbol", [symbol]) ;
 *         Option.map filter ~f:(fun filter -> "filter", [Yojson.Safe.to_string filter]) ;
 *         Option.map reverse ~f:(fun rev -> "reverse", [Bool.to_string rev]) ;
 *       ] in
 *     call ?extract_exn ?buf ~testnet ~credentials ~verb:Get ~query "/api/v1/order" >>|
 *     Or_error.map ~f:begin fun (resp, orders) ->
 *       resp, List.map (Yojson.Safe.Util.to_list orders) ~f:Order.of_yojson
 *     end
 * 
 *   let submit_bulk ?extract_exn ?buf ~testnet ~key ~secret orders =
 *     let credentials = key, secret in
 *     let orders = List.map orders ~f:(Yojson_encoding.construct encoding) in
 *     let body = `Assoc ["orders", `List orders] in
 *     call ?extract_exn ?buf ~testnet ~credentials ~body ~verb:Post "/api/v1/order/bulk" >>|
 *     Or_error.map ~f:begin fun (resp, orders) ->
 *       resp, List.map (Yojson.Safe.Util.to_list orders) ~f:Order.of_yojson
 *     end
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
 *   let create_amend
 *       ?origClOrdID ?clOrdID ?orderQty ?leavesQty
 *       ?price ?stopPx ?pegOffsetValue ?text ?orderID () =
 *     { orderID ; origClOrdID ; clOrdID ; orderQty ; leavesQty ; price ; stopPx ;
 *       pegOffsetValue ; text }
 * 
 *   let amend_encoding =
 *     let open Json_encoding in
 *     conv
 *       (fun { orderID ; origClOrdID ; clOrdID ; orderQty ; leavesQty ; price ; stopPx ;
 *              pegOffsetValue ; text } ->
 *         (orderID, origClOrdID, clOrdID, orderQty, leavesQty, price, stopPx,
 *          pegOffsetValue, text))
 *       (fun (orderID, origClOrdID, clOrdID, orderQty, leavesQty, price, stopPx,
 *             pegOffsetValue, text) ->
 *         { orderID ; origClOrdID ; clOrdID ; orderQty ; leavesQty ; price ; stopPx ;
 *           pegOffsetValue ; text })
 *       (obj9
 *          (opt "orderID" Uuidm.encoding)
 *          (opt "origClOrdID" string)
 *          (opt "clOrdID" string)
 *          (opt "orderQty" int)
 *          (opt "leavesQty" int)
 *          (opt "price" float)
 *          (opt "stopPx" float)
 *          (opt "pegOffsetValue" float)
 *          (opt "text" string))
 * 
 *   let amend_bulk ?extract_exn ?buf ~testnet ~key ~secret orders =
 *     let credentials = key, secret in
 *     let orders = List.map orders ~f:(Yojson_encoding.construct amend_encoding) in
 *     let body = `Assoc ["orders", `List orders] in
 *     call ?extract_exn ?buf ~testnet ~credentials ~body ~verb:Put "/api/v1/order/bulk" >>|
 *     Or_error.map ~f:begin fun (resp, orders) ->
 *       resp, List.map (Yojson.Safe.Util.to_list orders) ~f:Order.of_yojson
 *     end
 * 
 *   let cancel ?extract_exn ?buf  ~testnet ~key ~secret ?(orderIDs=[]) ?(clOrdIDs=[]) ?text () =
 *     let credentials = key, secret in
 *     let orderIDs = match orderIDs with
 *       | [] -> None
 *       | _ -> Some (String.concat ~sep:"," (List.map orderIDs ~f:Uuidm.to_string)) in
 *     let clOrdIDs = match clOrdIDs with
 *       | [] -> None
 *       | _ -> Some (String.concat ~sep:"," clOrdIDs) in
 *     let body = `Assoc (List.filter_opt [
 *         Option.map orderIDs ~f:(fun s -> "orderID", `String s) ;
 *         Option.map clOrdIDs ~f:(fun s -> "clOrdID", `String s) ;
 *         Option.map text ~f:(fun s -> "text", `String s) ;
 *       ]) in
 *     call ?extract_exn ?buf ~testnet ~credentials ~body ~verb:Delete "/api/v1/order" >>|
 *     Or_error.map ~f:begin fun (resp, orders) ->
 *       resp, List.map (Yojson.Safe.Util.to_list orders) ~f:Order.of_yojson
 *     end
 * 
 *   let cancel_all ?extract_exn ?buf ~testnet ~key ~secret ?symbol ?filter ?text () =
 *     let credentials = key, secret in
 *     let body = List.filter_opt [
 *         Option.map symbol ~f:(fun sym -> "symbol", `String sym);
 *         Option.map filter ~f:(fun json -> "filter", json);
 *         Option.map text ~f:(fun s -> "text", `String s);
 *       ] in
 *     let body = `Assoc body in
 *     call ?extract_exn ?buf ~testnet ~credentials ~body ~verb:Delete "/api/v1/order/all" >>|
 *     Or_error.map ~f:fst
 * 
 *   let cancel_all_after ?extract_exn ?buf ~testnet ~key ~secret timeout =
 *     let credentials = key, secret in
 *     let timeout = Time_ns.Span.to_int_ms timeout in
 *     let body = `Assoc ["timeout", `Int timeout] in
 *     call ?extract_exn ?buf ~testnet ~credentials ~body ~verb:Post "/api/v1/order/cancelAllAfter" >>|
 *     Or_error.map ~f:fst
 * end
 * 
 * module Position = struct
 *   let get ?extract_exn ?buf ~testnet ~key ~secret ?filter ?columns ?count () =
 *     let credentials = key, secret in
 *     let query = List.filter_opt [
 *         Option.map filter ~f:(fun filter -> "filter", [Yojson.Safe.to_string filter]) ;
 *         Option.map columns ~f:(fun cs -> "columns", cs) ;
 *         Option.map count ~f:(fun start -> "count", [Int.to_string start]) ;
 *       ] in
 *     call ?extract_exn ?buf ~testnet ~credentials ~verb:Get ~query "/api/v1/position" >>|
 *     Or_error.map ~f:begin fun (resp, positions) ->
 *       resp, List.map (Yojson.Safe.Util.to_list positions) ~f:Position.of_yojson
 *     end
 * end
 * 
 * module Trade = struct
 *   let get ?extract_exn ?buf ~testnet
 *       ?filter ?columns ?count ?start
 *       ?reverse ?startTime ?endTime ?symbol () =
 *     let query = List.filter_opt [
 *         Option.map symbol ~f:(fun symbol -> "symbol", [symbol]) ;
 *         Option.map filter ~f:(fun filter -> "filter", [Yojson.Safe.to_string filter]) ;
 *         Option.map columns ~f:(fun cs -> "columns", cs) ;
 *         Option.map count ~f:(fun start -> "count", [Int.to_string start]) ;
 *         Option.map start ~f:(fun start -> "start", [Int.to_string start]) ;
 *         Option.map reverse ~f:(fun rev -> "reverse", [Bool.to_string rev]) ;
 *         Option.map startTime ~f:(fun ts -> "startTime", [Time_ns.to_string ts]) ;
 *         Option.map endTime ~f:(fun ts -> "endTime", [Time_ns.to_string ts]) ;
 *       ] in
 *     call ?extract_exn ?buf ~testnet ~verb:Get ~query "/api/v1/trade" >>|
 *     Or_error.map ~f:begin fun (resp, json) ->
 *       resp, List.map (Yojson.Safe.Util.to_list json) ~f:Trade.of_yojson
 *     end
 * end *)

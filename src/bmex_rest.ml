open Core
open Async
open Httpaf

let src = Logs.Src.create "bmex.rest" ~doc:"BitMEX API - REST"
module Log = (val Logs_async.src_log src : Logs_async.LOG)

open Bmex
module BT = Bitmex_types

let err =
  let open Json_encoding in
  conv
    (fun _ -> assert false)
    (fun (name, msg) -> Error.createf "%s: %s" name msg)
    (obj1 (req "error" (obj2 (req "name" string) (req "message" string))))

let activeInstruments ?buf ?(testnet=false) () =
  let url = if testnet then testnet_url else url in
  let url = Uri.with_path url "/api/v1/instrument/active" in
  Fastrest.simple_call_string ~meth:`GET url >>| fun (_resp, body) ->
  let instrs = Yojson.Safe.(Util.to_list (from_string ?buf body)) in
  List.map instrs ~f:BT.Instrument.of_yojson

let activeAndIndices ?buf ?(testnet=false) () =
  let url = if testnet then testnet_url else url in
  let url = Uri.with_path url "/api/v1/instrument/activeAndIndices" in
  Fastrest.simple_call_string ~meth:`GET url >>| fun (_resp, body) ->
  let instrs = Yojson.Safe.(Util.to_list (from_string ?buf body)) in
  List.map instrs ~f:BT.Instrument.of_yojson

let trades ?buf ?(testnet=false)
    ?filter ?columns ?count ?start
    ?reverse ?startTime ?endTime symbol =
  let query = List.filter_opt [
      Some ("symbol", [symbol]) ;
      Option.map filter ~f:(fun filter -> "filter", [Yojson.Safe.to_string filter]) ;
      Option.map columns ~f:(fun cs -> "columns", cs) ;
      Option.map count ~f:(fun start -> "count", [Int.to_string start]) ;
      Option.map start ~f:(fun start -> "start", [Int.to_string start]) ;
      Option.map reverse ~f:(fun rev -> "reverse", [Bool.to_string rev]) ;
      Option.map startTime ~f:(fun ts -> "startTime", [Time_ns.to_string ts]) ;
      Option.map endTime ~f:(fun ts -> "endTime", [Time_ns.to_string ts]) ;
    ] in
  let url = if testnet then testnet_url else url in
  let url = Uri.with_path url "/api/v1/trade" in
  let url = Uri.with_query url query in
  Fastrest.simple_call_string ~meth:`GET url >>| fun (_resp, body) ->
  let instrs = Yojson.Safe.(Util.to_list (from_string ?buf body)) in
  List.map instrs ~f:BT.Trade.of_yojson

let tradeHistory ?buf ?(testnet=false)
    ?startTime ?endTime ?start ?count ?symbol ?filter ?reverse ~key ~secret () =
  Option.iter start ~f:(fun start -> if start < 0 then invalid_arg "tradeHistory: start < 0") ;
  Option.iter count ~f:(fun count -> if count < 1 || count > 500 then invalid_arg "tradeHistory: count < 1 || count > 500") ;
  let params = List.filter_opt [
      Option.map startTime ~f:(fun ts -> "startTime", [Time_ns.to_string ts]) ;
      Option.map endTime ~f:(fun ts -> "endTime", [Time_ns.to_string ts]) ;
      Option.map start ~f:(fun start -> "start", [Int.to_string start]) ;
      Option.map count ~f:(fun start -> "count", [Int.to_string start]) ;
      Option.map symbol ~f:(fun symbol -> "symbol", [symbol]) ;
      Option.map filter ~f:(fun filter -> "filter", [Yojson.Safe.to_string filter]) ;
      Option.map reverse ~f:(fun rev -> "reverse", [Bool.to_string rev]) ;
    ] in
  let url = if testnet then testnet_url else url in
  let url = Uri.with_path url "/api/v1/execution/tradeHistory" in
  let url = Uri.with_query url params in
  let auth_params = Crypto.mk_query_params ~key ~secret ~verb:Get url in
  let headers = Headers.(add_multi empty auth_params) in
  Fastrest.simple_call_string ~headers ~meth:`GET url >>| fun (_resp, body) ->
  let instrs = Yojson.Safe.(Util.to_list (from_string ?buf body)) in
  List.map instrs ~f:BT.Execution.of_yojson

let positions ?buf ?(testnet=false) ?filter ?columns ?count ~key ~secret () =
  let params = List.filter_opt [
      Option.map filter ~f:(fun filter -> "filter", [Yojson.Safe.to_string filter]) ;
      Option.map columns ~f:(fun cs -> "columns", cs) ;
      Option.map count ~f:(fun start -> "count", [Int.to_string start]) ;
    ] in
  let url = if testnet then testnet_url else url in
  let url = Uri.with_path url "/api/v1/position" in
  let url = Uri.with_query url params in
  let auth_params = Crypto.mk_query_params ~key ~secret ~verb:Get url in
  let headers = Headers.(add_multi empty auth_params) in
  Fastrest.simple_call_string ~headers ~meth:`GET url >>| fun (_resp, body) ->
  let instrs = Yojson.Safe.(Util.to_list (from_string ?buf body)) in
  List.map instrs ~f:BT.Position.of_yojson

let openOrders ?buf ?(testnet=false) ?startTime ?endTime
    ?start ?count ?symbol ?filter ?reverse ~key ~secret () =
  let params = List.filter_opt [
      Option.map startTime ~f:(fun ts -> "startTime", [Time_ns.to_string ts]) ;
      Option.map endTime ~f:(fun ts -> "endTime", [Time_ns.to_string ts]) ;
      Option.map start ~f:(fun start -> "start", [Int.to_string start]) ;
      Option.map count ~f:(fun start -> "count", [Int.to_string start]) ;
      Option.map symbol ~f:(fun symbol -> "symbol", [symbol]) ;
      Option.map filter ~f:(fun filter -> "filter", [Yojson.Safe.to_string filter]) ;
      Option.map reverse ~f:(fun rev -> "reverse", [Bool.to_string rev]) ;
    ] in
  let url = if testnet then testnet_url else url in
  let url = Uri.with_path url "/api/v1/order" in
  let url = Uri.with_query url params in
  let auth_params = Crypto.mk_query_params ~key ~secret ~verb:Get url in
  let headers = Headers.(add_multi empty auth_params) in
  Fastrest.simple_call_string ~headers ~meth:`GET url >>| fun (_resp, body) ->
  let instrs = Yojson.Safe.(Util.to_list (from_string ?buf body)) in
  List.map instrs ~f:BT.Order.of_yojson

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

let createOrder ?displayQty ?price ?stopPx ?contingencyType
    ?pegOffsetValue ?pegPriceType
    ?(timeInForce=Fixtypes.TimeInForce.GoodTillCancel) ?(execInst=[])
    ?text ~symbol ~orderQty ~ordType clOrdID =
  { symbol ; orderQty ; displayQty ; price ;
    stopPx ; clOrdID ; contingencyType ;
    pegOffsetValue ; pegPriceType ; ordType ;
    timeInForce ; execInst ; text }

let order =
  let open Json_encoding in
  conv
    (fun { symbol ; orderQty ; displayQty ; price ; stopPx ; clOrdID ; contingencyType ;
           pegOffsetValue ; pegPriceType ; ordType ; timeInForce ; execInst ; text } ->
      let contingencyType, clOrdLinkID =
        match contingencyType with
        | None -> None, None
        | Some (contingencyType, clOrdLinkID) ->
          Some contingencyType, Some clOrdLinkID
      in
      let execInst =
        match execInst with
        | [] -> None
        | _ -> Some (String.concat ~sep:"," (List.map execInst ~f:ExecInst.to_string)) in
      (symbol, orderQty, displayQty, price, stopPx, clOrdID, clOrdLinkID,
       pegOffsetValue, pegPriceType, ordType),
      (timeInForce, execInst, contingencyType, text))
    (fun ((symbol, orderQty, displayQty, price, stopPx, clOrdID, clOrdLinkID,
           pegOffsetValue, pegPriceType, ordType),
          (timeInForce, execInst, contingencyType, text)) ->
      let contingencyType =
        match clOrdLinkID, contingencyType with
        | Some id, Some t -> Some (t, id)
        | _ -> None
      in
      let execInst =
        match execInst with
        | None -> []
        | Some execInst ->
          List.map (String.split ~on:',' execInst) ~f:ExecInst.of_string in
      { symbol ; orderQty ; displayQty ; price ; stopPx ; clOrdID ; contingencyType ;
        pegOffsetValue ; pegPriceType ; ordType ; timeInForce ; execInst ; text })
    (merge_objs
       (obj10
          (req "symbol" string)
          (req "orderQty" int)
          (opt "displayQty" int)
          (opt "price" float)
          (opt "stopPx" float)
          (req "clOrdID" Uuidm.encoding)
          (opt "clOrdLinkID" string)
          (opt "pegOffsetValue" float)
          (opt "pegPriceType" PegPriceType.encoding)
          (req "ordType" OrderType.encoding))
       (obj4
          (dft "timeInForce" TimeInForce.encoding Fixtypes.TimeInForce.GoodTillCancel)
          (opt "execInst" string)
          (opt "contingencyType" ContingencyType.encoding)
          (opt "text" string)))

let json_base_headers =
  Headers.(add empty "content-type" "application/json")

let submit ?buf ?(testnet=false) ~key ~secret orders =
  let orders = List.map orders ~f:(Yojson_encoding.construct order) in
  let body = `Assoc ["orders", `List orders] in
  let body = Yojson.Safe.to_string ?buf body in
  let url = if testnet then testnet_url else url in
  let url = Uri.with_path url "/api/v1/order/bulk" in
  let auth_params =
    Crypto.mk_query_params ~key ~secret ~verb:Post ~data:body url in
  let headers = Headers.(add_multi json_base_headers auth_params) in
  Fastrest.simple_call_string
    ~body ~headers ~meth:`POST url >>= fun (_resp, body) ->
  let body_json = Yojson.Safe.from_string ?buf body in
  try
    let instrs = Yojson.Safe.Util.to_list body_json in
    return (List.map instrs ~f:BT.Order.of_yojson)
  with _ ->
    Error.raise (Yojson_encoding.destruct err body_json)

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

let createAmend
    ?origClOrdID ?clOrdID ?orderQty ?leavesQty
    ?price ?stopPx ?pegOffsetValue ?text ?orderID () =
  { orderID ; origClOrdID ; clOrdID ; orderQty ; leavesQty ; price ; stopPx ;
    pegOffsetValue ; text }

let amend =
  let open Json_encoding in
  conv
    (fun { orderID ; origClOrdID ; clOrdID ; orderQty ; leavesQty ; price ; stopPx ;
           pegOffsetValue ; text } ->
      (orderID, origClOrdID, clOrdID, orderQty, leavesQty, price, stopPx,
       pegOffsetValue, text))
    (fun (orderID, origClOrdID, clOrdID, orderQty, leavesQty, price, stopPx,
          pegOffsetValue, text) ->
      { orderID ; origClOrdID ; clOrdID ; orderQty ; leavesQty ; price ; stopPx ;
        pegOffsetValue ; text })
    (obj9
       (opt "orderID" Uuidm.encoding)
       (opt "origClOrdID" Uuidm.encoding)
       (opt "clOrdID" Uuidm.encoding)
       (opt "orderQty" int)
       (opt "leavesQty" int)
       (opt "price" float)
       (opt "stopPx" float)
       (opt "pegOffsetValue" float)
       (opt "text" string))

let amend ?buf ?(testnet=false) ~key ~secret orders =
  let orders = List.map orders ~f:(Yojson_encoding.construct amend) in
  let body = `Assoc ["orders", `List orders] in
  let body = Yojson.Safe.to_string ?buf body in
  let url = if testnet then testnet_url else url in
  let url = Uri.with_path url "/api/v1/order/bulk" in
  let auth_params =
    Crypto.mk_query_params ~key ~secret ~verb:Put ~data:body url in
  let headers = Headers.(add_multi json_base_headers auth_params) in
  Fastrest.simple_call_string
    ~body ~headers ~meth:`PUT url >>= fun (_resp, body) ->
  let body_json = Yojson.Safe.from_string ?buf body in
  try
    let instrs = Yojson.Safe.Util.to_list body_json in
    return (List.map instrs ~f:BT.Order.of_yojson)
  with _ ->
    Error.raise (Yojson_encoding.destruct err body_json)

let cancel
    ?buf ?(testnet=false)
    ?(orderIDs=[]) ?(clOrdIDs=[]) ?text ~key ~secret () =
  let orderIDs = match orderIDs with
    | [] -> None
    | _ -> Some (String.concat ~sep:"," (List.map orderIDs ~f:Uuidm.to_string)) in
  let clOrdIDs = match clOrdIDs with
    | [] -> None
    | _ -> Some (String.concat ~sep:"," (List.map clOrdIDs ~f:Uuidm.to_string)) in
  let body = `Assoc (List.filter_opt [
      Option.map orderIDs ~f:(fun s -> "orderID", `String s) ;
      Option.map clOrdIDs ~f:(fun s -> "clOrdID", `String s) ;
      Option.map text ~f:(fun s -> "text", `String s) ;
    ]) in
  let body = Yojson.Safe.to_string ?buf body in
  let url = if testnet then testnet_url else url in
  let url = Uri.with_path url "/api/v1/order" in
  let auth_params =
    Crypto.mk_query_params ~key ~secret ~verb:Delete ~data:body url in
  let headers = Headers.(add_multi json_base_headers auth_params) in
  Fastrest.simple_call_string
    ~body ~headers ~meth:`DELETE url >>= fun (_resp, body) ->
  let body_json = Yojson.Safe.from_string ?buf body in
  try
    let instrs = Yojson.Safe.Util.to_list body_json in
    return (List.map instrs ~f:BT.Order.of_yojson)
  with _ ->
    Error.raise (Yojson_encoding.destruct err body_json)

let cancelAll ?buf ?(testnet=false) ?symbol ?filter ?text ~key ~secret () =
  let body = List.filter_opt [
      Option.map symbol ~f:(fun sym -> "symbol", `String sym);
      Option.map filter ~f:(fun json -> "filter", json);
      Option.map text ~f:(fun s -> "text", `String s);
    ] in
  let body = Yojson.Safe.to_string ?buf (`Assoc body) in
  let url = if testnet then testnet_url else url in
  let url = Uri.with_path url "/api/v1/order/all" in
  let auth_params =
    Crypto.mk_query_params ~key ~secret ~verb:Delete ~data:body url in
  let headers = Headers.(add_multi json_base_headers auth_params) in
  Fastrest.simple_call_string
    ~body ~headers ~meth:`DELETE url >>= fun (_resp, body) ->
  let body_json = Yojson.Safe.from_string ?buf body in
  try
    let instrs = Yojson.Safe.Util.to_list body_json in
    return (List.map instrs ~f:BT.Order.of_yojson)
  with _ ->
    Error.raise (Yojson_encoding.destruct err body_json)

type cancelAllAfter = {
  now: Ptime.t ;
  cancelTime: Ptime.t ;
}

let caa_encoding =
  let open Json_encoding in
  conv
    (fun _ -> assert false)
    (fun (now, cancelTime) -> { now; cancelTime })
    (obj2 (req "now" Ptime.encoding) (req "cancelTime" Ptime.encoding))

let cancelAllAfter ?buf ?(testnet=false) ~key ~secret timeout =
  let timeout = Time_ns.Span.to_int_ms timeout in
  let body = ["timeout", `Int timeout] in
  let body = Yojson.Safe.to_string ?buf (`Assoc body) in
  let url = if testnet then testnet_url else url in
  let url = Uri.with_path url "/api/v1/order/cancelAllAfter" in
  let auth_params =
    Crypto.mk_query_params ~key ~secret ~verb:Post ~data:body url in
  let headers = Headers.(add_multi json_base_headers auth_params) in
  Fastrest.simple_call_string
    ~body ~headers ~meth:`POST url >>= fun (_resp, body) ->
  let body_json = Yojson.Safe.from_string ?buf body in
  try
    return (Yojson_encoding.destruct caa_encoding body_json)
  with _ ->
    Error.raise (Yojson_encoding.destruct err body_json)

let wallet ?buf ?(testnet=false) ~key ~secret ()  =
  let url = if testnet then testnet_url else url in
  let url = Uri.with_path url "/api/v1/user/wallet" in
  let auth_params =
    Crypto.mk_query_params ~key ~secret ~verb:Get url in
  let headers = Headers.(add_multi json_base_headers auth_params) in
  Fastrest.simple_call_string
    ~headers ~meth:`GET url >>= fun (_resp, body) ->
  let body_json = Yojson.Safe.from_string ?buf body in
  try
    return (BT.Wallet.of_yojson body_json)
  with _ ->
    Error.raise (Yojson_encoding.destruct err body_json)

let walletHistory ?buf ?(testnet=false) ?(start=0) ?(count=1000) ~key ~secret () =
  let params = [
    "start", [Int.to_string start] ;
    "count", [Int.to_string count] ;
  ] in
  let url = if testnet then testnet_url else url in
  let url = Uri.with_path url "/api/v1/user/walletHistory" in
  let url = Uri.with_query url params in
  let auth_params =
    Crypto.mk_query_params ~key ~secret ~verb:Get url in
  let headers = Headers.(add_multi json_base_headers auth_params) in
  Fastrest.simple_call_string
    ~headers ~meth:`GET url >>= fun (_resp, body) ->
  let body_json = Yojson.Safe.from_string ?buf body in
  try
    let txs = Yojson.Safe.Util.to_list body_json in
    return (List.map txs ~f:BT.Transaction.of_yojson)
  with _ ->
    Error.raise (Yojson_encoding.destruct err body_json)

let walletSummary ?buf ?(testnet=false) ~key ~secret ()  =
  let url = if testnet then testnet_url else url in
  let url = Uri.with_path url "/api/v1/user/walletSummary" in
  let auth_params =
    Crypto.mk_query_params ~key ~secret ~verb:Get url in
  let headers = Headers.(add_multi json_base_headers auth_params) in
  Fastrest.simple_call_string
    ~headers ~meth:`GET url >>= fun (_resp, body) ->
  let body_json = Yojson.Safe.from_string ?buf body in
  try
    let txs = Yojson.Safe.Util.to_list body_json in
    return (List.map txs ~f:BT.Wallet.of_yojson)
  with _ ->
    Error.raise (Yojson_encoding.destruct err body_json)

let executionHistory ?buf ?(testnet=false) ~key ~secret ~symbol ~ts ()  =
  let params = [
    "symbol", [symbol] ;
    "timestamp", [Ptime.to_rfc3339 ts] ;
  ] in
  let url = if testnet then testnet_url else url in
  let url = Uri.with_path url "/api/v1/user/executionHistory" in
  let url = Uri.with_query url params in
  let auth_params =
    Crypto.mk_query_params ~key ~secret ~verb:Get url in
  let headers = Headers.(add_multi json_base_headers auth_params) in
  Fastrest.simple_call_string
    ~headers ~meth:`GET url >>= fun (_resp, body) ->
  let body_json = Yojson.Safe.from_string ?buf body in
  try
    let execs = Yojson.Safe.Util.to_list body_json in
    return (List.map execs ~f:BT.Execution.of_yojson)
  with _ ->
    Error.raise (Yojson_encoding.destruct err body_json)


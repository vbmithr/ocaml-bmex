open Core
open Async

open Bmex

module Yojson_encoding = Json_encoding.Make(Json_repr.Yojson)
let any_to_yojson = Json_repr.(any_to_repr (module Yojson))
let yojson_to_any = Json_repr.(repr_to_any (module Yojson))

module C = Cohttp
open Cohttp_async

module Error = struct
  type t = {
    name: string;
    message: string;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { name ; message } -> (name, message))
      (fun (name, message) -> { name ; message })
      (obj2
        (req "name" string)
        (req "message" string))

  let wrapped_encoding =
    let open Json_encoding in
    obj1 (req "error" encoding)
end

let mk_headers ?log ?(data="") ~key ~secret ~verb uri =
  let query_params =
    Crypto.mk_query_params ?log ~data ~key ~secret ~api:Rest ~verb uri in
  Cohttp.Header.of_list @@
  ("content-type", "application/json") ::
  List.Assoc.map query_params ~f:List.hd_exn

let ssl_config = Conduit_async.Ssl.configure ~version:Tlsv1_2 ()

let call
    ?extract_exn
    ?buf
    ?log
    ?(span=Time_ns.Span.of_int_sec 1)
    ?(max_tries=3)
    ?(query=[])
    ?body
    ~key
    ~secret
    ~testnet
    ~verb
    path =
  let url = if testnet then testnet_url else url in
  let url = Uri.with_path url path in
  let url = Uri.with_query url query in
  let body_str =
    Option.map body ~f:(fun json -> Yojson.Safe.to_string ?buf json) in
  begin match log, body_str with
    | Some log, Some body_str ->
      Log.debug log "%s %s -> %s" (show_verb verb) path body_str
    | _ -> ()
  end ;
  let body = Option.map body_str ~f:Body.of_string in
  let headers = mk_headers ?log ?data:body_str ~key ~secret ~verb url in
  let call () = match verb with
    | Get -> Client.get ~ssl_config ~headers url
    | Post -> Client.post ~ssl_config ~headers ~chunked:false ?body url
    | Put -> Client.put ~ssl_config ~headers ~chunked:false ?body url
    | Delete -> Client.delete ~ssl_config ~headers ~chunked:false ?body url in
  let rec inner_exn try_id =
    call () >>= fun (resp, body) ->
    Body.to_string body >>= fun body_str ->
    let status = Response.status resp in
    let status_code = C.Code.code_of_status status in
    if C.Code.is_success status_code then
      return @@ Yojson.Safe.from_string ?buf body_str
    else if C.Code.is_client_error status_code then begin
      let json = Yojson.Safe.(from_string ?buf body_str) in
      let { Error.name ; message } =
        Yojson_encoding.destruct Error.wrapped_encoding json in
      failwithf "%s: %s" name message ()
    end
    else if C.Code.is_server_error status_code then begin
      let status_code_str = (C.Code.sexp_of_status_code status |> Sexplib.Sexp.to_string_hum) in
      Option.iter log ~f:(fun log -> Log.error log "%s %s: %s" (show_verb verb) path status_code_str);
      Clock_ns.after span >>= fun () ->
      if try_id >= max_tries then failwithf "%s %s: %s" (show_verb verb) path status_code_str ()
      else inner_exn @@ succ try_id
    end
    else failwithf "%s %s: Unexpected HTTP return status %s"
        (show_verb verb) path
        (C.Code.sexp_of_status_code status |> Sexplib.Sexp.to_string_hum) ()
  in
  Monitor.try_with_or_error ?extract_exn (fun () -> inner_exn 0)


module ApiKey = struct
  module Permission = struct
    type t =
      | Perm of string
      | Dtc of string

    let dtc_to_any username =
      yojson_to_any
        (`List [`String "sierra-dtc"; `Assoc ["username", `String username]])

    let dtc_of_any any =
      match any_to_yojson any with
      | `List [`String "sierra-dtc"; `Assoc ["username", `String username]] -> Dtc username
      | #Yojson.Safe.json -> invalid_arg "ApiKey.dtc_of_any"

    let encoding =
      let open Json_encoding in
      union [
        case string
          (function Perm s -> Some s | _ -> None)
          (fun s -> Perm s) ;
        case any_value
          (function Perm _ -> None | Dtc username -> Some (dtc_to_any username))
          (fun any -> dtc_of_any any) ;
      ]
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

  let encoding =
    let open Json_encoding in
    conv
      (fun { id ; secret ; name ; nonce ; cidr ;
             permissions ; enabled ; userId ; created } ->
        (id, secret, name, nonce, cidr, permissions,
         enabled, userId, created))
      (fun (id, secret, name, nonce, cidr, permissions,
            enabled, userId, created) ->
        { id ; secret ; name ; nonce ; cidr ; permissions ;
          enabled ; userId ; created })
      (obj9
         (req "id" string)
         (req "secret" string)
         (req "name" string)
         (req "nonce" int)
         (req "cidr" string)
         (req "permissions" (list Permission.encoding))
         (req "enabled" bool)
         (req "userId" int)
         (req "created" time_encoding))

  let dtc ?buf ?log ?username ~testnet ~key ~secret () =
    let path = "/api/v1/apiKey/dtc/" ^
               match username with None -> "all" | Some u -> "get" in
    let query = match username with None -> [] | Some u -> ["get", [u]] in
    call ?buf ?log ~key ~secret ~testnet ~query ~verb:Get path >>| function
    | Ok json -> Ok (Yojson_encoding.destruct encoding json)
    | Error err -> Error err
end

let position ?buf ?log ~testnet ~key ~secret () =
  call ?buf ?log ~testnet ~key ~secret ~verb:Get "/api/v1/position"

module Order = struct
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

  let create ?displayQty ?price ?stopPx ?clOrdID ?contingencyType
      ?pegOffsetValue ?pegPriceType ?(execInst=[]) ?text ~symbol ~qty ~ordType ~timeInForce () =
    { symbol ; qty ; displayQty ; price ; stopPx ; clOrdID ; contingencyType ;
      pegOffsetValue ; pegPriceType ; ordType ; timeInForce ; execInst ; text }

  let encoding =
    let open Json_encoding in
    conv
      (fun { symbol ; qty ; displayQty ; price ; stopPx ; clOrdID ; contingencyType ;
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
        (symbol, qty, displayQty, price, stopPx, clOrdID, clOrdLinkID,
         pegOffsetValue, pegPriceType, ordType),
        (timeInForce, execInst, contingencyType, text))
      (fun ((symbol, qty, displayQty, price, stopPx, clOrdID, clOrdLinkID,
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
        { symbol ; qty ; displayQty ; price ; stopPx ; clOrdID ; contingencyType ;
          pegOffsetValue ; pegPriceType ; ordType ; timeInForce ; execInst ; text })
      (merge_objs
         (obj10
            (req "symbol" string)
            (req "qty" int)
            (opt "displayQty" int)
            (opt "price" float)
            (opt "stopPx" float)
            (opt "clOrdID" string)
            (opt "clOrdLinkID" string)
            (opt "pegOffsetValue" float)
            (opt "pegPriceType" PegPriceType.encoding)
            (req "ordType" OrderType.encoding))
         (obj4
            (req "timeInForce" TimeInForce.encoding)
            (opt "execInst" string)
            (opt "contingencyType" ContingencyType.encoding)
            (opt "text" string)))

  let submit_bulk ?buf ?log ~testnet ~key ~secret orders =
    let orders = List.map orders ~f:(Yojson_encoding.construct encoding) in
    let body = `Assoc ["orders", `List orders] in
    call ?buf ?log ~testnet ~key ~secret ~body ~verb:Post "/api/v1/order/bulk"

  let update_bulk ?buf ?log ~testnet ~key ~secret orders =
    let body = `Assoc ["orders", `List orders] in
    call ?buf ?log ~testnet ~key ~secret ~body ~verb:Put "/api/v1/order/bulk"

  let cancel ?buf ?log ~testnet ~key ~secret orderID =
    let body = `Assoc ["orderID", `String Uuid.(to_string orderID)] in
    call ?buf ?log ~testnet ~key ~secret ~body ~verb:Delete "/api/v1/order"

  let cancel_all ?buf ?log ?symbol ?filter ~testnet ~key ~secret () =
    let body = List.filter_opt [
        Option.map filter ~f:(fun json -> "filter", json);
        Option.map symbol ~f:(fun sym -> "symbol", `String sym);
      ] in
    let body = `Assoc body in
    call ?buf ?log ~testnet ~key ~secret ~body ~verb:Delete "/api/v1/order/all"

  let cancel_all_after ?buf ?log ~testnet ~key ~secret timeout =
    let timeout = Time_ns.Span.to_int_ms timeout in
    let body = `Assoc ["timeout", `Int timeout] in
    call ?buf ?log ~testnet ~key ~secret ~body ~verb:Post "/api/v1/order/cancelAllAfter"
end

let trade_history ?buf ?log ~testnet ~key ~secret
    ?startTime ?endTime ?start ?count ?symbol ?filter ?reverse () =
  let query = List.filter_opt [
      Option.map startTime ~f:(fun ts -> "startTime", [Time_ns.to_string ts]) ;
      Option.map endTime ~f:(fun ts -> "endTime", [Time_ns.to_string ts]) ;
      Option.map start ~f:(fun start -> "endTime", [Int.to_string start]) ;
      Option.map count ~f:(fun start -> "count", [Int.to_string start]) ;
      Option.map symbol ~f:(fun symbol -> "symbol", [symbol]) ;
      Option.map filter ~f:(fun filter -> "filter", [Yojson.Safe.to_string filter]) ;
      Option.map reverse ~f:(fun rev -> "reverse", [Bool.to_string rev]) ;
    ] in
  call ?buf ?log ~testnet ~key ~secret ~verb:Get ~query "/api/v1/execution/tradeHistory"

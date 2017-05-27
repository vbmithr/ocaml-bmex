open Core
open Async

open Bmex

module Yojson_encoding = Json_encoding.Make(Json_repr.Yojson)
let any_to_yojson = Json_repr.(any_to_repr (module Yojson))
let yojson_to_any = Json_repr.(repr_to_any (module Yojson))

module Topic = struct
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
    | OrderBookL2
    | OrderBook
    | OrderBook25
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

  let to_string = function
    (* private *)
    | PrivateNotifications -> "privateNotifications"
    | Account -> "account"
    | Wallet -> "wallet"
    | Affiliate -> "affiliate"
    | Margin -> "margin"
    | Position -> "position"
    | Transact -> "transact"
    | Order -> "order"
    | Execution -> "execution"
    (* public *)
    | Announcement -> "announcement"
    | Connected -> "connected"
    | Chat -> "chat"
    | PublicNotifications -> "publicNotifications"
    | Instrument -> "instrument"
    | Settlement -> "settlement"
    | Funding -> "funding"
    | Insurance -> "insurance"
    | Liquidation -> "liquidation"
    | OrderBookL2 -> "orderBookL2"
    | OrderBook -> "orderBook"
    | OrderBook25 -> "orderBook25"
    | OrderBook10 -> "orderBook10"
    | Quote -> "quote"
    | Trade -> "trade"
    | QuoteBin1m -> "quoteBin1m"
    | QuoteBin5m -> "quoteBin5m"
    | QuoteBin1h -> "quoteBin1h"
    | QuoteBin1d -> "quoteBin1d"
    | TradeBin1m -> "tradeBin1m"
    | TradeBin5m -> "tradeBin5m"
    | TradeBin1h -> "tradeBin1h"
    | TradeBin1d -> "tradeBin1d"

  let of_string = function
    | "privateNotifications" -> PrivateNotifications
    | "account" -> Account
    | "wallet" -> Wallet
    | "affiliate" -> Affiliate
    | "margin" -> Margin
    | "position" -> Position
    | "transact" -> Transact
    | "order" -> Order
    | "execution" -> Execution
    | "announcement" -> Announcement
    | "connected" -> Connected
    | "chat" -> Chat
    | "publicNotifications" -> PublicNotifications
    | "instrument" -> Instrument
    | "settlement" -> Settlement
    | "funding" -> Funding
    | "insurance" -> Insurance
    | "liquidation" -> Liquidation
    | "orderBookL2" -> OrderBookL2
    | "orderBook" -> OrderBook
    | "orderBook25" -> OrderBook25
    | "orderBook10" -> OrderBook10
    | "quote" -> Quote
    | "trade" -> Trade
    | "quoteBin1m" -> QuoteBin1m
    | "quoteBin5m" -> QuoteBin5m
    | "quoteBin1h" -> QuoteBin1h
    | "quoteBin1d" -> QuoteBin1d
    | "tradeBin1m" -> TradeBin1m
    | "tradeBin5m" -> TradeBin5m
    | "tradeBin1h" -> TradeBin1h
    | "tradeBin1d" -> TradeBin1d
    | _ -> invalid_arg "Topic.of_string"
end

module Request = struct
  module Sub = struct
    type t = {
      topic : Topic.t ;
      symbol : string option ;
    }

    let to_string { topic ; symbol } =
      match symbol with
      | None -> (Topic.to_string topic)
      | Some symbol -> Topic.to_string topic ^ ":" ^ symbol

    let of_string str =
      match String.split str ~on:':' with
      | [topic] -> { topic = Topic.of_string topic ; symbol = None }
      | [topic ; symbol] -> { topic = Topic.of_string topic ; symbol = Some symbol }
      | _ -> invalid_arg "Request.Sub.of_string"

    let encoding = Json_encoding.(conv to_string of_string string)
  end

  type t =
    | Subscribe of Sub.t list
    | Unsubscribe of Sub.t list
    | CancelAllAfter of int
    | AuthKey of {
        key : string ;
        nonce : int ;
        signature : string
      }

  let encoding =
    let open Json_encoding in
    union [
      case
        (obj2
           (req "op" string)
           (req "args" (list Sub.encoding)))
        (function
          | Subscribe args -> Some ("subscribe", args)
          | Unsubscribe args -> Some ("unsubscribe", args)
          | _ -> None)
        (function
          | ("subscribe", args) -> Subscribe args
          | ("unsubscribe", args) -> Unsubscribe args
          | _ -> invalid_arg "Request.encoding") ;
      case
        (obj2
           (req "op" string)
           (req "args" int))
        (function
          | CancelAllAfter timeout -> Some ("cancelAllAfter", timeout)
          | _ -> None)
        (function
          | ("cancelAllAfter", timeout) -> CancelAllAfter timeout
          | _ -> invalid_arg "Request.encoding") ;
      case
        (obj2
           (req "op" string)
           (req "args" (list string)))
        (function
          | AuthKey { key ; nonce ; signature } ->
            Some ("authKey", [key ; Int.to_string nonce ; signature])
          | _ -> None)
        (fun _ -> assert false)
    ]
end

module Response = struct
  module Update = struct
    type action =
      | Partial
      | Update
      | Insert
      | Delete

    let action_encoding =
      let open Json_encoding in
      string_enum [
        "partial", Partial ;
        "update", Update ;
        "insert", Insert ;
        "delete", Delete ;
      ]

    type t = {
      table : string ;
      action : action ;
      data : Yojson.Safe.json ;
    }

    let encoding =
      let open Json_encoding in
      conv
        (fun { table ; action ; data } ->
           let data = yojson_to_any data in
           (), (table, action, data))
        (fun ((), (table, action, data)) ->
           let data = any_to_yojson data in
           { table ; action ; data })
        (merge_objs unit
           (obj3
              (req "table" string)
              (req "action" action_encoding)
              (req "data" any_value)))
  end

  module Welcome = struct
    type t = {
      heartbeat : bool ;
      timeout : int ;
      version : string ;
      timestamp : Time_ns.t ;
    }

    let encoding =
      let open Json_encoding in
      conv
        (fun { heartbeat ; timeout ; version ; timestamp } ->
           (), (heartbeat, timeout, version, Time_ns.to_string timestamp))
        (fun ((), (heartbeat, timeout, version, timestamp)) ->
           let timestamp = Time_ns.of_string timestamp in
           { heartbeat ; timeout ; version ; timestamp })
        (merge_objs unit
           (obj4
              (req "heartbeatEnabled" bool)
              (req "heartbeatTimeout" int)
              (req "version" string)
              (req "timestamp" string)))
  end

  let error_encoding =
    let open Json_encoding in
    conv (fun s -> (), s) (fun ((), s) -> s)
      (merge_objs unit (obj1 (req "error" string)))

  let response_encoding =
    let open Json_encoding in
    conv
      (fun sub -> (), (sub, true))
      (fun ((), (sub, res)) -> sub)
      (merge_objs unit
         (obj2
            (req "subscribe" Request.Sub.encoding)
            (req "success" bool)))

  type t =
    | Welcome of Welcome.t
    | Error of string
    | Response of Request.Sub.t
    | Update of Update.t

  let encoding =
    let open Json_encoding in
    union [
      case
        error_encoding
        (function Error s -> Some s | _ -> None)
        (fun s -> Error s) ;
      case
        Welcome.encoding
        (function Welcome w -> Some w | _ -> None)
        (fun w -> Welcome w) ;
      case
        response_encoding
        (function Response r -> Some r | _ -> None)
        (fun r -> Response r) ;
      case
        Update.encoding
        (function Update u -> Some u | _ -> None)
        (fun u -> Update u) ;
    ]
end

module MD = struct
  type stream = {
    id : string ;
    topic : string
  }

  type t =
    | Message of { stream : stream ; payload : Yojson.Safe.json }
    | Subscribe of stream
    | Unsubscribe of stream

  let of_yojson = function
    | `List (`Int typ :: `String id :: `String topic :: payload) -> begin
        match typ, payload with
        | 0, [payload] -> Message { stream = { id ; topic } ; payload }
        | 1, [] -> Subscribe { id ; topic }
        | 2, [] -> Subscribe { id ; topic }
        | _ -> invalid_arg "MD.of_yojson"
      end
    | #Yojson.Safe.json -> invalid_arg "MD.of_yojson"

  let to_yojson = function
    | Message { stream = { id ; topic } ; payload } ->
      `List [`Int 0 ; `String id ; `String topic ; payload]
    | Subscribe { id ; topic } ->
      `List [`Int 1 ; `String id ; `String topic]
    | Unsubscribe { id ; topic } ->
      `List [`Int 2 ; `String id ; `String topic]

  let subscribe ~id ~topic = Subscribe {id ; topic }
  let unsubscribe ~id ~topic = Unsubscribe { id ; topic }
  let message ~id ~topic ~payload = Message { stream = { id ; topic } ; payload }

  let auth ~id ~topic ~key ~secret =
    let nonce, signature = Crypto.sign ~secret ~verb:Get ~endp:"/realtime" Ws in
    let payload =
      Request.AuthKey { key ; nonce ; signature } |>
      Yojson_encoding.construct Request.encoding in
    message ~id ~topic ~payload

end

let uri_of_opts testnet md =
  Uri.with_path
    (if testnet then testnet_url else url)
    (if md then "realtimemd" else "realtime")

let open_connection
    ?connected
    ?(buf=Bi_outbuf.create 4096)
    ?to_ws
    ?(query_params=[])
    ?log
    ?auth
    ~testnet ~md ~topics () =
  let uri = uri_of_opts testnet md in
  let auth_params = match auth with
    | None -> []
    | Some (key, secret) -> Crypto.mk_query_params ?log ~key ~secret Ws Get uri
  in
  let uri = Uri.add_query_param uri ("heartbeat", ["true"]) in
  let uri = Uri.add_query_params uri @@
    if md then [] else
      ["subscribe", topics] @ auth_params @ query_params
  in
  let uri_str = Uri.to_string uri in
  let host = Option.value_exn ~message:"no host in uri" Uri.(host uri) in
  let port = Option.value_exn ~message:"no port inferred from scheme"
      Uri_services.(tcp_port_of_uri uri)
  in
  let scheme = Option.value_exn ~message:"no scheme in uri" Uri.(scheme uri) in
  let ws_w_mvar = Mvar.create () in
  let rec try_write msg =
    let ws_w_mvar_ro = Mvar.read_only ws_w_mvar in
    Mvar.value_available ws_w_mvar_ro >>= fun () ->
    let w = Mvar.peek_exn ws_w_mvar_ro in
    if Pipe.is_closed w then begin
      Mvar.take ws_w_mvar_ro >>= fun _ ->
      try_write msg
    end
    else Pipe.write w msg
  in
  Option.iter to_ws ~f:begin fun to_ws ->
    don't_wait_for @@
    Monitor.handle_errors begin fun () ->
      Pipe.iter ~continue_on_error:true to_ws ~f:begin fun msg_json ->
        let msg_str = Yojson.Safe.to_string msg_json in
        Option.iter log ~f:(fun log -> Log.debug log "-> %s" msg_str);
        try_write msg_str
      end
    end
      (fun exn -> Option.iter log ~f:(fun log ->
           Log.error log "%s" @@ Exn.to_string exn))
  end;
  let client_r, client_w = Pipe.create () in
  let cleanup r w ws_r ws_w =
    Option.iter log ~f:(fun log ->
        Log.debug log "[WS] post-disconnection cleanup") ;
    Pipe.close ws_w ;
    Pipe.close_read ws_r ;
    Deferred.all_unit [Reader.close r ; Writer.close w ] ;
  in
  let tcp_fun s r w =
    Option.iter log ~f:(fun log ->
        Log.info log "[WS] connecting to %s" uri_str);
    Socket.(setopt s Opt.nodelay true);
    (if scheme = "https" || scheme = "wss" then
       Conduit_async_ssl.ssl_connect r w else
       return (r, w)) >>= fun (ssl_r, ssl_w) ->
    let ws_r, ws_w =
      Websocket_async.client_ez ?log
        ~heartbeat:(Time_ns.Span.of_int_sec 25) uri s ssl_r ssl_w in
    don't_wait_for begin
      Deferred.all_unit
        [ Reader.close_finished r ; Writer.close_finished w ] >>= fun () ->
      cleanup ssl_r ssl_w ws_r ws_w
    end ;
    Mvar.set ws_w_mvar ws_w;
    Option.iter connected ~f:(fun c -> Mvar.set c ());
    Pipe.transfer ws_r client_w ~f:(Yojson.Safe.from_string ~buf)
  in
  let rec loop () = begin
    Monitor.try_with_or_error
      ~name:"with_connection"
      ~extract_exn:false
      begin fun () ->
        Tcp.(with_connection (to_host_and_port host port) tcp_fun)
      end >>| function
    | Ok () -> Option.iter log ~f:(fun log ->
        Log.error log "[WS] connection to %s terminated" uri_str);
    | Error err -> Option.iter log ~f:(fun log ->
        Log.error log "[WS] connection to %s raised %s"
          uri_str (Error.to_string_hum err))
  end >>= fun () ->
    if Pipe.is_closed client_r then Deferred.unit
    else Clock_ns.after @@ Time_ns.Span.of_int_sec 10 >>= loop
  in
  don't_wait_for @@ loop ();
  client_r

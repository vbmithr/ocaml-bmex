open Sexplib.Std
open Bmex

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
  [@@deriving sexp]

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

  let encoding =
    let open Json_encoding in
    string_enum [
      "privateNotifications", PrivateNotifications ;
      "account", Account ;
      "wallet", Wallet ;
      "affiliate", Affiliate ;
      "margin", Margin ;
      "position", Position ;
      "transact", Transact ;
      "order", Order ;
      "execution", Execution ;
      "announcement", Announcement ;
      "connected", Connected ;
      "chat", Chat ;
      "publicNotifications", PublicNotifications ;
      "instrument", Instrument ;
      "settlement", Settlement ;
      "funding", Funding ;
      "insurance", Insurance ;
      "liquidation", Liquidation ;
      "orderBookL2", OrderBookL2 ;
      "orderBook", OrderBook ;
      "orderBook25", OrderBook25 ;
      "orderBook10", OrderBook10 ;
      "quote", Quote ;
      "trade", Trade ;
      "quoteBin1m", QuoteBin1m ;
      "quoteBin5m", QuoteBin5m ;
      "quoteBin1h", QuoteBin1h ;
      "quoteBin1d", QuoteBin1d ;
      "tradeBin1m", TradeBin1m ;
      "tradeBin5m", TradeBin5m ;
      "tradeBin1h", TradeBin1h ;
      "tradeBin1d", TradeBin1d ;
    ]

  let show = to_string
  let pp ppf t = Format.fprintf ppf "%s" (to_string t)
end

module Request = struct
  module Sub = struct
    type t = {
      topic : Topic.t ;
      symbol : string option ;
    } [@@deriving sexp]

    let create ?symbol topic = { symbol ; topic }

    let to_string { topic ; symbol } =
      match symbol with
      | None -> (Topic.to_string topic)
      | Some symbol -> Topic.to_string topic ^ ":" ^ symbol

    let of_string str =
      match String.split_on_char ':' str with
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
  [@@deriving sexp]

  let subscribe subs = Subscribe subs
  let unsubscribe subs = Unsubscribe subs
  let cancel_all_after timeout = CancelAllAfter timeout
  let authkey ~key ~nonce ~signature = AuthKey { key ; nonce ; signature }

  let encoding =
    let open Json_encoding in
    union [
      case
        (obj2
           (req "op" string)
           (req "args" Sub.encoding))
        (fun _ -> None)
        (function
          | ("subscribe", arg) -> Subscribe [arg]
          | ("unsubscribe", arg) -> Unsubscribe [arg]
          | _ -> invalid_arg "Request.encoding") ;
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
           (req "args" any_value))
        (function
          | AuthKey { key ; nonce ; signature } ->
            let payload =
              Json_repr.(repr_to_any (module Yojson)
                           (`List [`String key ; `Int nonce ; `String signature])) in
            Some ("authKey", payload)
          | _ -> None)
        (function
          | ("authKey", payload) -> begin
              match Json_repr.(any_to_repr (module Yojson) payload) with
              | `List [`String key ; `Int nonce ; `String signature] ->
                AuthKey { key ; nonce ; signature }
              | _ -> invalid_arg "Request.encoding"
            end
          | _ -> invalid_arg "Request.encoding")
    ]
end

module Response = struct
  module Update = struct
    type action =
      | Partial
      | Update
      | Insert
      | Delete
    [@@deriving sexp]

    let action_of_string = function
      | "partial" -> Partial
      | "update" -> Update
      | "insert" -> Insert
      | "delete" -> Delete
      | _ -> invalid_arg "Bmex_ws.Response.Update.action_of_string"

    let action_to_string = function
      | Partial -> "partial"
      | Update -> "update"
      | Insert -> "insert"
      | Delete -> "delete"

    let show_action = action_to_string

    let pp_action ppf t = Format.fprintf ppf "%s" (action_to_string t)

    let action_encoding =
      let open Json_encoding in
      string_enum [
        "partial", Partial ;
        "update", Update ;
        "insert", Insert ;
        "delete", Delete ;
      ]

    type t = {
      table : Topic.t ;
      action : action ;
      data : Yojson.Safe.t list ;
    } [@@deriving sexp]

    let encoding =
      let open Json_encoding in
      conv
        (fun { table ; action ; data } ->
           let data = List.map Yojson_encoding.yojson_to_any data in
           (), (table, action, data))
        (fun ((), (table, action, data)) ->
           let data = List.map Yojson_encoding.any_to_yojson data in
           { table ; action ; data })
        (merge_objs unit
           (obj3
              (req "table" Topic.encoding)
              (req "action" action_encoding)
              (req "data" (list any_value))))
  end

  module Welcome = struct
    type t = {
      version : string ;
      timestamp : Ptime.t ;
    } [@@deriving sexp]

    let encoding =
      let open Json_encoding in
      conv
        (fun { version ; timestamp } -> (), (version, timestamp))
        (fun ((), (version, timestamp)) -> { version ; timestamp })
        (merge_objs unit
           (obj2
              (req "version" string)
              (req "timestamp" Ptime.encoding)))
  end

  let error_encoding =
    let open Json_encoding in
    conv (fun s -> (), s) (fun ((), s) -> s)
      (merge_objs unit (obj1 (req "error" string)))

  module Response = struct
    type t = {
      success: bool option ;
      request : Request.t ;
      subscribe : Request.Sub.t option ;
    } [@@deriving sexp]

    let encoding =
      let open Json_encoding in
      conv
        (fun { success ; request ; subscribe } ->
           (), (success, request, subscribe))
        (fun ((), (success, request, subscribe)) ->
           { success ; request ; subscribe })
        (merge_objs unit
           (obj3
              (opt "success" bool)
              (req "request" Request.encoding)
              (opt "subscribe" Request.Sub.encoding)))
  end

  type t =
    | Welcome of Welcome.t
    | Error of string
    | Response of Response.t
    | Update of Update.t
  [@@deriving sexp]

  let pp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

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
        Response.encoding
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
    | Message of { stream : stream ; payload : Yojson.Safe.t }
    | Subscribe of stream
    | Unsubscribe of stream

  let of_yojson = function
    | `List (`Int typ :: `String id :: `String topic :: payload) -> begin
        match typ, payload with
        | 0, [payload] -> Message { stream = { id ; topic } ; payload }
        | 1, [] -> Subscribe { id ; topic }
        | 2, [] -> Unsubscribe { id ; topic }
        | _ -> invalid_arg "MD.of_yojson"
      end
    | #Yojson.Safe.t -> invalid_arg "MD.of_yojson"

  let to_yojson = function
    | Message { stream = { id ; topic } ; payload } ->
      `List [`Int 0 ; `String id ; `String topic ; payload]
    | Subscribe { id ; topic } ->
      `List [`Int 1 ; `String id ; `String topic]
    | Unsubscribe { id ; topic } ->
      `List [`Int 2 ; `String id ; `String topic]

  let subscribe ~id ~topic = Subscribe { id ; topic }
  let unsubscribe ~id ~topic = Unsubscribe { id ; topic }
  let message ~id ~topic ~payload = Message { stream = { id ; topic } ; payload }

  let auth ~id ~topic ~key ~secret =
    let nonce, signature = Crypto.sign ~secret ~verb:Get ~endp:"/realtime" Ws in
    let payload =
      Request.AuthKey { key ; nonce ; signature } |>
      Yojson_encoding.construct Request.encoding in
    message ~id ~topic ~payload
end

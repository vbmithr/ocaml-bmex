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
    | OrderBookL2_25
    | OrderBookL2
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
    | "orderBookL2_25" -> OrderBookL2_25
    | "orderBookL2" -> OrderBookL2
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
    | OrderBookL2_25 -> "orderBookL2_25"
    | OrderBookL2 -> "orderBookL2"
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
      "orderBookL2_25", OrderBookL2_25 ;
      "orderBookL2", OrderBookL2 ;
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

module Quote = struct
  type t = {
    symbol: string ;
    id: int64 ;
    side: Fixtypes.Side.t ;
    size: int option ;
    price: float option ;
  } [@@deriving sexp]


  let encoding =
    let open Json_encoding in
    conv
      (fun { symbol ; id ; side ; size ; price } -> (symbol, id, side, size, price))
      (fun (symbol, id, side, size, price) -> { symbol ; id ; side ; size ; price })
      (obj5
         (req "symbol" string)
         (req "id" int53)
         (req "side" side_encoding)
         (opt "size" int)
         (opt "price" float))
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
    | AuthKeyExpires of {
        key : string ;
        expiresIn : string ;
        signature : string
      }
  [@@deriving sexp]

  let subscribe subs = Subscribe subs
  let unsubscribe subs = Unsubscribe subs
  let cancel_all_after timeout = CancelAllAfter timeout
  let authkey ~key ~expiresIn ~signature =
    AuthKeyExpires { key ; expiresIn ; signature }

  let encoding =
    let open Json_encoding in
    union [
      case (obj2 (req "op" string) (req "args" Sub.encoding))
        (fun _ -> None)
        (function
          | ("subscribe", arg) -> Subscribe [arg]
          | ("unsubscribe", arg) -> Unsubscribe [arg]
          | _ -> invalid_arg "Request.encoding") ;
      case
        (obj2 (req "op" string) (req "args" (list Sub.encoding)))
        (function
          | Subscribe args -> Some ("subscribe", args)
          | Unsubscribe args -> Some ("unsubscribe", args)
          | _ -> None)
        (function
          | ("subscribe", args) -> Subscribe args
          | ("unsubscribe", args) -> Unsubscribe args
          | _ -> invalid_arg "Request.encoding") ;
      case (obj2 (req "op" string) (req "args" int))
        (function
          | CancelAllAfter timeout -> Some ("cancelAllAfter", timeout)
          | _ -> None)
        (function
          | ("cancelAllAfter", timeout) -> CancelAllAfter timeout
          | _ -> invalid_arg "Request.encoding") ;
      case (obj2 (req "op" string) (req "args" any_value))
        (function
          | AuthKeyExpires { key ; expiresIn ; signature } ->
            let payload =
              Json_repr.(repr_to_any (module Yojson)
                           (`List [`String key ; `String expiresIn ; `String signature])) in
            Some ("authKeyExpires", payload)
          | _ -> None)
        (function
          | ("authKeyExpires", payload) -> begin
              match Json_repr.(any_to_repr (module Yojson) payload) with
              | `List [`String key ; `String expiresIn ; `String signature] ->
                AuthKeyExpires { key ; expiresIn ; signature }
              | _ -> invalid_arg "Request.encoding"
            end
          | _ -> invalid_arg "Request.encoding")
    ]

  let to_string ?buf msg =
    Yojson.Safe.to_string ?buf (Yojson_encoding.construct encoding msg)
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

    type data =
      | Quotes of Quote.t list
      | Trades of Trade.t list
      | Unknown of Yojson.Safe.t [@@deriving sexp]

    (* let yojson_to_repr = Json_repr.any_to_repr (module Json_repr.Yojson)
     * let repr_to_yojson = Json_repr.repr_to_any (module Json_repr.Yojson) *)

    (* let yojson_encoding =
     *   let open Json_encoding in
     *   conv repr_to_yojson yojson_to_repr any_value *)

    let data_encoding =
      let open Json_encoding in
      union [
        case (list Quote.encoding) (function Quotes qs -> Some qs | _ -> None ) (fun qs -> Quotes qs) ;
        case (list Trade.encoding) (function Trades ts -> Some ts | _ -> None ) (fun ts -> Trades ts) ;
        (* case yojson_encoding (function Unknown u -> Some u | _ -> None) (fun u -> Unknown u) ; *)
      ]

    type t = {
      table : Topic.t ;
      action : action ;
      data :  data ;
    } [@@deriving sexp]

    let encoding =
      let open Json_encoding in
      conv
        (fun { table ; action ; data } -> (), (table, action, data))
        (fun ((), (table, action, data)) -> { table ; action ; data })
        (merge_objs unit
           (obj3
              (req "table" Topic.encoding)
              (req "action" action_encoding)
              (req "data" data_encoding)))
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

  let of_string ?buf msg =
    Yojson_encoding.destruct_safe encoding (Yojson.Safe.from_string ?buf msg)
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
    let expiresIn, signature = Crypto.sign ~secret ~verb:Get "/realtime" in
    let payload =
      Request.AuthKeyExpires { key ; expiresIn ; signature } |>
      Yojson_encoding.construct Request.encoding in
    message ~id ~topic ~payload
end

let is_md url = String.equal (Uri.path url) "realtimemd"

let mk_auth_params url = function
  | None -> []
  | Some (key, secret) ->
    Crypto.mk_query_params ~key ~secret ~verb:Get url

let mk_query_params ~auth ~topics ~query_params =
  match is_md url, topics with
  | true, _ -> []
  | false, [] -> mk_auth_params url auth @ query_params
  | false, topics ->
    ["subscribe", List.map Request.Sub.to_string topics] @
    mk_auth_params url auth @ query_params

let mk_url ?(md=false) ?auth ?(topics=[]) ?(query_params=[]) url =
  let url = Uri.with_path url (if md then "realtimemd" else "realtime") in
  Uri.add_query_params url (mk_query_params ~auth ~topics ~query_params)

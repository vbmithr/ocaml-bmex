open Sexplib.Std

let src = Logs.Src.create "bmex" ~doc:"BitMEX API"
module Log = (val Logs.src_log src : Logs.LOG)

let url = Uri.make ~scheme:"https" ~host:"www.bitmex.com" ()
let testnet_url = Uri.make ~scheme:"https" ~host:"testnet.bitmex.com" ()
let ws url = Uri.with_path url "realtime"
let wsmd url = Uri.with_path url "realtimemd"

module Ptime = struct
  include Ptime

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_rfc3339 sexp_str with
    | Ok (t, _, _) -> t
    | _ -> invalid_arg "Ptime.t_of_sexp"

  let sexp_of_t t =
    sexp_of_string (to_rfc3339 t)

  let encoding =
    let open Ptime in
    let of_rfc3339_exn s = match of_rfc3339 s with
      | Ok (v, _, _) -> v
      | Error `RFC3339 (_, e) ->
        failwith (Format.asprintf "%a" pp_rfc3339_error e) in
    Json_encoding.(conv to_rfc3339 of_rfc3339_exn string)
end

module Uuidm = struct
  include Uuidm

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_string sexp_str with
    | None -> invalid_arg "Uuidm.t_of_sexp"
    | Some u -> u

  let sexp_of_t t =
    sexp_of_string (to_string t)

  let encoding =
    let open Json_encoding in
    conv
      (fun u -> to_string u)
      (fun s -> match of_string s with
         | None -> invalid_arg "Uuidm.encoding"
         | Some u -> u)
      string
end

module Yojson = struct
  module Safe = struct
    include Yojson.Safe

    let t_of_sexp sexp = from_string (string_of_sexp sexp)
    let sexp_of_t t = sexp_of_string (to_string t)
  end
end

module Yojson_encoding = struct
  include Json_encoding.Make(Json_repr.Yojson)

  let destruct_safe encoding value =
    try destruct encoding value with exn ->
      Log.err begin fun m ->
        m "@[<v 0>%a:@,%a@]"
          (Yojson.Safe.pretty_print ~std:false) value
          (Json_encoding.print_error ?print_unknown:None) exn
      end ;
      raise exn

  let any_to_yojson = Json_repr.(any_to_repr (module Yojson))
  let yojson_to_any = Json_repr.(repr_to_any (module Yojson))
end

type verb = Get | Post | Put | Delete

let string_of_verb = function
  | Get -> "GET"
  | Post -> "POST"
  | Put -> "PUT"
  | Delete -> "DELETE"

let show_verb = string_of_verb

let pp_verb ppf v =
  Format.pp_print_string ppf (show_verb v)

module Side = struct
  type t = [ `buy | `sell | `buy_sell_unset ]

  let of_string = function
    | "Buy" -> `buy
    | "Sell" -> `sell
    | _ -> `buy_sell_unset

  let to_string = function
    | `buy -> "Buy"
    | `sell -> "Sell"
    | `buy_sell_unset -> ""

  let show = to_string

  let pp ppf side =
    Format.fprintf ppf "%s" (to_string side)

  let encoding =
    let open Json_encoding in
    string_enum [
      "Buy", `buy ;
      "Sell", `sell ;
      "", `buy_sell_unset ;
    ]
end

module Quote = struct
  type t = {
    timestamp: Ptime.t ;
    symbol: string ;
    bidPrice: float option ;
    bidSize: int option ;
    askPrice: float option ;
    askSize: int option ;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { timestamp ; symbol ; bidPrice ; bidSize ; askPrice ; askSize } ->
         (timestamp, symbol, bidPrice, bidSize, askPrice, askSize))
      (fun (timestamp, symbol, bidPrice, bidSize, askPrice, askSize) ->
         { timestamp ; symbol ; bidPrice ; bidSize ; askPrice ; askSize })
      (obj6
         (req "timestamp" Ptime.encoding)
         (req "symbol" string)
         (req "bidPrice" (option float))
         (req "bidSize" (option int))
         (req "askPrice" (option float))
         (req "askSize" (option int)))

  (* let merge t t' =
   *   if t.symbol <> t'.symbol then invalid_arg "Quote.merge: symbols do not match";
   *   let merge_quote = Option.merge ~f:(fun _ q' -> q') in
   *   {
   *     timestamp = t'.timestamp ;
   *     symbol = t'.symbol ;
   *     bidPrice = merge_quote t.bidPrice t'.bidPrice ;
   *     bidSize = merge_quote t.bidSize t'.bidSize ;
   *     askPrice = merge_quote t.askPrice t'.askPrice ;
   *     askSize = merge_quote t.askSize t'.askSize ;
   *   } *)
end

module Crypto = struct
  let expire sec =
    Float.to_int (Ptime.to_float_s (Ptime_clock.now ())) + sec

  let sign ?(data="") ~secret ~verb endp =
    let expire_in = string_of_int (expire 30) in
    let prehash = (string_of_verb verb) ^ endp ^ expire_in ^ data in
    Log.debug (fun m -> m "prehash %s" prehash) ;
    let sign = Digestif.SHA256.(hmac_string ~key:secret prehash |> to_hex) in
    expire_in, sign

  let mk_query_params ?data ~key ~secret ~verb uri =
    let endp = Uri.path_and_query uri in
    let expire_in, signature = sign ?data ~secret ~verb endp in
    [ "api-expires", [expire_in] ;
      "api-key", [key];
      "api-signature", [signature];
    ]
end

module OrderType = struct
  type t = Fixtypes.OrdType.t [@@deriving sexp]

  let of_string = function
    | "Market" -> Fixtypes.OrdType.Market
    | "Limit" -> Limit
    | "Stop" -> Stop
    | "StopLimit" -> StopLimit
    | "LimitIfTouched" -> LimitIfTouched
    | "MarketIfTouched" -> MarketIfTouched
    | _ -> invalid_arg "OrdType.of_string"

  let encoding =
    let open Json_encoding in
    string_enum [
      "Market", Fixtypes.OrdType.Market ;
      "Limit", Limit ;
      "Stop", Stop ;
      "StopLimit", StopLimit ;
      "MarketIfTouched", MarketIfTouched ;
      "LimitIfTouched", LimitIfTouched ;
    ]
end

module TimeInForce = struct
  type t = Fixtypes.TimeInForce.t [@@deriving sexp]

  let of_string = function
    | "Day" -> Fixtypes.TimeInForce.Session
    | "GoodTillCancel" -> GoodTillCancel
    | "ImmediateOrCancel" -> ImmediateOrCancel
    | "FillOrKill" -> FillOrKill
    | _ -> invalid_arg "TimeInForce.of_string"

  let encoding =
    let open Json_encoding in
    string_enum [
      "Day", Fixtypes.TimeInForce.Session ;
      "GoodTillCancel", GoodTillCancel ;
      "ImmediateOrCancel", ImmediateOrCancel ;
      "FillOrKill", FillOrKill ;
    ]
end

module ExecInst = struct
  type t =
    | ParticipateDoNotInitiate
    | AllOrNone
    | MarkPrice
    | LastPrice
    | IndexPrice
    | Close
    | ReduceOnly
    | Fixed
    | Unknown of string

  let to_string = function
    | ParticipateDoNotInitiate -> "ParticipateDoNotInitiate"
    | AllOrNone -> "AllOrNone"
    | MarkPrice -> "MarkPrice"
    | LastPrice -> "LastPrice"
    | IndexPrice -> "IndexPrice"
    | Close -> "Close"
    | ReduceOnly -> "ReduceOnly"
    | Fixed -> "Fixed"
    | Unknown s -> s

  let of_string = function
    | "ParticipateDoNotInitiate" -> ParticipateDoNotInitiate
    | "AllOrNone" -> AllOrNone
    | "MarkPrice" -> MarkPrice
    | "LastPrice" -> LastPrice
    | "IndexPrice" -> IndexPrice
    | "Close" -> Close
    | "ReduceOnly" -> ReduceOnly
    | "Fixed" -> Fixed
    | s -> Unknown s

  let encoding =
    let open Json_encoding in
    string_enum [
      "ParticipateDoNotInitiate", ParticipateDoNotInitiate ;
      "AllOrNone", AllOrNone ;
      "MarkPrice", MarkPrice ;
      "LastPrice", LastPrice ;
      "IndexPrice", IndexPrice ;
      "Close", Close ;
      "ReduceOnly", ReduceOnly ;
      "Fixed", Fixed ;
    ]
end

module ContingencyType = struct
  type t = Fixtypes.ContingencyType.t

  let encoding =
    let open Json_encoding in
    string_enum [
      "OneCancelsTheOther", Fixtypes.ContingencyType.OCO ;
      "OneTriggersTheOther", OTO ;
      "OneUpdatesTheOtherAbsolute", OUOA ;
      "OneUpdatesTheOtherProportional", OUOP ;
    ]
end

module PegPriceType = struct
  type t = Fixtypes.PegPriceType.t

  let encoding =
    let open Json_encoding in
    string_enum [
      "LastPeg", Fixtypes.PegPriceType.LastPeg ;
      "MidPricePeg", MidPricePeg ;
      "PrimaryPeg", PrimaryPeg ;
      "TrailingStopPeg", TrailingStopPeg ;
    ]
end

module OrdStatus = struct
  type t = Fixtypes.OrdStatus.t [@@deriving sexp]

  let show t = Sexplib.Sexp.to_string (sexp_of_t t)

  let of_string = function
    | "New" -> Fixtypes.OrdStatus.New
    | "PartiallyFilled" -> PartiallyFilled
    | "Filled" -> Filled
    | "DoneForDay" -> DoneForDay
    | "Canceled" -> Canceled
    | "PendingCancel" -> PendingCancel
    | "Stopped" -> Stopped
    | "Rejected" -> Rejected
    | "Suspended" -> Suspended
    | "PendingNew" -> PendingNew
    | "Calculated" -> Calculated
    | "Expired" -> Expired
    | "AcceptedForBidding" -> AcceptedForBidding
    | "PendingReplace" -> PendingReplace
    | s -> failwith ("OrdStatus.of_string: " ^ s)

  let to_dtc = function
    | Fixtypes.OrdStatus.New -> `order_status_open
    | PartiallyFilled -> `order_status_partially_filled
    | Filled -> `order_status_filled
    | Canceled -> `order_status_canceled
    | PendingCancel -> `order_status_pending_cancel
    | Rejected -> `order_status_rejected
    | PendingNew -> `order_status_pending_open
    | _ -> `order_status_unspecified
end

module ExecType = struct
  type t =
    | New
    | Trade
    | Canceled
    | Replaced
    | Restated
    | Rejected
    | TriggeredOrActivatedBySystem
    | Funding
    | Settlement
    | Suspended
    | Released
    | Insurance
    | Rebalance
    | Unknown of string
  [@@deriving sexp]

  let show t = Sexplib.Sexp.to_string (sexp_of_t t)

  let of_string = function
    | "New" -> New
    | "Trade" -> Trade
    | "Canceled" -> Canceled
    | "Replaced" -> Replaced
    | "Restated" -> Restated
    | "TriggeredOrActivatedBySystem" -> TriggeredOrActivatedBySystem
    | "Funding" -> Funding
    | "Settlement" -> Settlement
    | "Suspended" -> Suspended
    | "Released" -> Released
    | "Insurance" -> Insurance
    | "Rebalance" -> Rebalance
    | s -> Unknown s
end

let side_encoding =
  let open Json_encoding in
  string_enum [
    "Buy", Fixtypes.Side.Buy ;
    "Sell", Sell ;
  ]

let tickDirection_encoding =
  let open Json_encoding in
  string_enum [
    "ZeroMinusTick", Fixtypes.TickDirection.ZeroMinusTick ;
    "MinusTick", MinusTick ;
    "ZeroPlusTick", ZeroPlusTick ;
    "PlusTick", PlusTick ;
  ]

module Trade = struct
  type t = {
    ts: Ptime.t ;
    symbol: string ;
    side: Fixtypes.Side.t ;
    size: int ;
    price: float ;
    tickDirection: Fixtypes.TickDirection.t ;
    trdMatchID: Uuidm.t ;
    grossValue: int64 ;
    homeNotional: float ;
    foreignNotional: float ;
  } [@@deriving sexp]

  let encoding =
    let open Json_encoding in
    conv
      (fun { ts ; symbol ; side ; size ; price ; tickDirection ;
             trdMatchID ; grossValue ; homeNotional ; foreignNotional } ->
        (ts, symbol, side, size, price, tickDirection,
         trdMatchID, grossValue, homeNotional, foreignNotional))
      (fun (ts, symbol, side, size, price, tickDirection,
            trdMatchID, grossValue, homeNotional, foreignNotional) ->
        { ts ; symbol ; side ; size ; price ; tickDirection ;
          trdMatchID ; grossValue ; homeNotional ; foreignNotional })
      (obj10
         (req "timestamp" Ptime.encoding)
         (req "symbol" string)
         (req "side" side_encoding)
         (req "size" int)
         (req "price" float)
         (req "tickDirection" tickDirection_encoding)
         (req "trdMatchID" Uuidm.encoding)
         (req "grossValue" int53)
         (req "homeNotional" float)
         (req "foreignNotional" float))
end

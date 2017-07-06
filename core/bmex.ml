open Core
open Async
module Yojson_encoding = Json_encoding.Make(Json_repr.Yojson)

let url = Uri.of_string "https://www.bitmex.com"
let testnet_url = Uri.of_string "https://testnet.bitmex.com"

let time_encoding =
  Json_encoding.(Time_ns.(conv to_string of_string string))

let uint_encoding =
  Json_encoding.ranged_int ~minimum:0 ~maximum:Int.max_value "uint"

type verb = Get | Post | Put | Delete
let string_of_verb = function
  | Get -> "GET"
  | Post -> "POST"
  | Put -> "PUT"
  | Delete -> "DELETE"
let show_verb = string_of_verb

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

module OrderBook = struct
  module Deprecated = struct
    type t = {
      symbol: string ;
      level: int ;
      bidSize: int option ;
      bidPrice: float option ;
      askSize: int option ;
      askPrice: float option ;
      timestamp: Time_ns.t ;
    }
  end

  module L2 = struct
    type t = {
      symbol: string ;
      id: int ;
      side: Side.t ;
      size: int option ;
      price: float option ;
    }

    let encoding =
      let open Json_encoding in
      conv
        (fun { symbol ; id ; side ; size ; price } ->
           (symbol, id, side, size, price))
        (fun (symbol, id, side, size, price) ->
           { symbol ; id ; side ; size ; price })
        (obj5
           (req "symbol" string)
           (req "id" uint_encoding)
           (req "side" Side.encoding)
           (opt "size" int)
           (opt "price" float))

    let of_yojson = Yojson_encoding.destruct encoding
    let to_yojson = Yojson_encoding.construct encoding
  end
end

module Quote = struct
  type t = {
    timestamp: Time_ns.t ;
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
         (req "timestamp" time_encoding)
         (req "symbol" string)
         (req "bidPrice" (option float))
         (req "bidSize" (option int))
         (req "askPrice" (option float))
         (req "askSize" (option int)))

  let of_yojson = Yojson_encoding.destruct encoding
  let to_yojson = Yojson_encoding.construct encoding

  let merge t t' =
    if t.symbol <> t'.symbol then invalid_arg "Quote.merge: symbols do not match";
    let merge_quote = Option.merge ~f:(fun _ q' -> q') in
    {
      timestamp = t'.timestamp ;
      symbol = t'.symbol ;
      bidPrice = merge_quote t.bidPrice t'.bidPrice ;
      bidSize = merge_quote t.bidSize t'.bidSize ;
      askPrice = merge_quote t.askPrice t'.askPrice ;
      askSize = merge_quote t.askSize t'.askSize ;
    }
end

module Trade = struct
  type t = {
    timestamp: Time_ns.t;
    symbol: string;
    side: Side.t ;
    size: int;
    price: float;
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun { timestamp ; symbol ; side ; size ; price } ->
         (), (timestamp, symbol, side, size, price))
      (fun ((), (timestamp, symbol, side, size, price)) ->
         { timestamp ; symbol ; side ; size ; price })
      (merge_objs unit
         (obj5
            (req "timestamp" time_encoding)
            (req "symbol" string)
            (req "side" Side.encoding)
            (req "size" int)
            (req "price" float)))

  let of_yojson = Yojson_encoding.destruct encoding
  let to_yojson = Yojson_encoding.construct encoding
end

module Crypto = struct
  type api = Rest | Ws

  let gen_nonce = function
  | Rest -> Time_ns.(now () |> to_int_ns_since_epoch) / 1_000_000_000 + 5
  | Ws -> Time_ns.(now () |> to_int_ns_since_epoch) / 1_000

  let sign ?log ?(data="") ~secret ~verb ~endp kind =
    let verb_str = string_of_verb verb in
    let nonce = gen_nonce kind in
    let nonce_str = Int.to_string nonce in
    Option.iter log ~f:(fun log -> Log.debug log "sign %s" nonce_str);
    let prehash = Cstruct.of_string (verb_str ^ endp ^ nonce_str ^ data) in
    let `Hex sign =
      Hex.of_cstruct Nocrypto.Hash.SHA256.(hmac ~key:secret prehash) in
    nonce, sign

  let mk_query_params ?log ?(data="") ~key ~secret ~api ~verb uri =
    let endp = Uri.path_and_query uri in
    let nonce, signature = sign ?log ~secret ~verb ~endp ~data api in
    [ (match api with Rest -> "api-expires" | Ws -> "api-nonce"), [Int.to_string nonce];
      "api-key", [key];
      "api-signature", [signature];
    ]
end

module OrderType = struct
  type t = [
    | `order_type_unset
    | `order_type_market
    | `order_type_limit
    | `order_type_stop
    | `order_type_stop_limit
    | `order_type_market_if_touched
  ]

  let to_string = function
    | `order_type_market -> "Market"
    | `order_type_limit -> "Limit"
    | `order_type_stop -> "Stop"
    | `order_type_stop_limit -> "StopLimit"
    | `order_type_market_if_touched -> "MarketIfTouched"
    | `order_type_unset -> ""

  let of_string = function
    | "Market" -> `order_type_market
    | "Limit" -> `order_type_limit
    | "Stop" -> `order_type_stop
    | "StopLimit" -> `order_type_stop_limit
    | "MarketIfTouched" -> `order_type_market_if_touched
    | s -> invalid_argf "ord_type_of_string: %s" s ()

  let encoding =
    let open Json_encoding in
    string_enum [
      "Market", `order_type_market ;
      "Limit", `order_type_limit ;
      "Stop", `order_type_stop ;
      "StopLimit", `order_type_stop_limit ;
      "MarketIfTouched", `order_type_market_if_touched ;
    ]

  let to_p1_p2 ~stopPx ~price = function
    | `order_type_unset
    | `order_type_market -> None, None
    | `order_type_limit -> Some price, None
    | `order_type_stop -> Some stopPx, None
    | `order_type_stop_limit -> Some stopPx, Some price
    | `order_type_market_if_touched -> Some stopPx, None

  let to_price_stopPx ?p1 ?p2 ordType =
    match ordType, p1, p2 with
    | `order_type_unset, _, _
    | `order_type_market, _, _ -> None, None
    | `order_type_limit, Some p, _ -> Some p, None
    | `order_type_stop, Some p, _
    | `order_type_market_if_touched, Some p, _ -> None, Some p
    | `order_type_stop_limit, Some stopPx, Some limitPx -> Some limitPx, Some stopPx
    | _ -> invalid_arg "price_fields_of_dtc"
end

module TimeInForce = struct
  type t = [
    | `tif_unset
    | `tif_day
    | `tif_good_till_canceled
    | `tif_all_or_none
    | `tif_immediate_or_cancel
    | `tif_fill_or_kill
    | `tif_good_till_date_time
  ]

  let to_string = function
    | `tif_unset
    | `tif_good_till_date_time
    | `tif_day -> "Day"
    | `tif_good_till_canceled
    | `tif_all_or_none -> "GoodTillCancel"
    | `tif_immediate_or_cancel -> "ImmediateOrCancel"
    | `tif_fill_or_kill -> "FillOrKill"

  let of_string = function
    | "Day" -> `tif_day
    | "GoodTillCancel" -> `tif_good_till_canceled
    | "ImmediateOrCancel" -> `tif_immediate_or_cancel
    | "FillOrKill" -> `tif_fill_or_kill
    | s -> invalid_argf "tif_of_string: %s" s ()

  let encoding =
    let open Json_encoding in
    string_enum [
      "Day", `tif_day ;
      "GoodTillCancel", `tif_good_till_canceled ;
      "ImmediateOrCancel", `tif_immediate_or_cancel ;
      "FillOrKill", `tif_fill_or_kill ;
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

  let to_string = function
    | ParticipateDoNotInitiate -> "ParticipateDoNotInitiate"
    | AllOrNone -> "AllOrNone"
    | MarkPrice -> "MarkPrice"
    | LastPrice -> "LastPrice"
    | IndexPrice -> "IndexPrice"
    | Close -> "Close"
    | ReduceOnly -> "ReduceOnly"
    | Fixed -> "Fixed"

  let of_string = function
    | "ParticipateDoNotInitiate" -> ParticipateDoNotInitiate
    | "AllOrNone" -> AllOrNone
    | "MarkPrice" -> MarkPrice
    | "LastPrice" -> LastPrice
    | "IndexPrice" -> IndexPrice
    | "Close" -> Close
    | "ReduceOnly" -> ReduceOnly
    | "Fixed" -> Fixed
    | _ -> invalid_arg "ExecInst.of_string"

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
  type t =
    | OCO (* OneCancelsTheOther *)
    | OTO (* OneTriggersTheOther *)
    | OUOA (* OneUpdatesTheOtherAbsolute *)
    | OUOP (* OneUpdatesTheOtherProportional *)

  let encoding =
    let open Json_encoding in
    string_enum [
      "OneCancelsTheOther", OCO ;
      "OneTriggersTheOther", OTO ;
      "OneUpdatesTheOtherAbsolute", OUOA ;
      "OneUpdatesTheOtherProportional", OUOP ;
    ]
end

module PegPriceType = struct
  type t =
    | LastPeg
    | MidPricePeg
    | MarketPeg
    | PrimaryPeg
    | TrailingStopPeg

  let encoding =
    let open Json_encoding in
    string_enum [
      "LastPeg", LastPeg ;
      "MidPricePeg", MidPricePeg ;
      "PrimaryPeg", PrimaryPeg ;
      "TrailingStopPeg", TrailingStopPeg ;
    ]
end


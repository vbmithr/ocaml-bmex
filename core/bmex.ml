open Core
open Async

let url = Uri.of_string "https://www.bitmex.com"
let testnet_url = Uri.of_string "https://testnet.bitmex.com"

let time_encoding =
  Json_encoding.(Time_ns.(conv to_string of_string string))

type verb = Get | Post | Put | Delete
let string_of_verb = function
  | Get -> "GET"
  | Post -> "POST"
  | Put -> "PUT"
  | Delete -> "DELETE"
let show_verb = string_of_verb

module Side = struct
  type t = [`Buy | `Sell]

  let of_string = function
    | "Buy" -> `Buy
    | "Sell" -> `Sell
    | _ -> invalid_arg "Side.of_string"

  let to_string = function
    | `Buy -> "Buy"
    | `Sell -> "Sell"

  let show = to_string

  let pp ppf side =
    Format.fprintf ppf "%s" (to_string side)
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
  end
end

module Quote = struct
  type t = {
    timestamp: string;
    symbol: string;
    bidPrice: float option ;
    bidSize: int option ;
    askPrice: float option ;
    askSize: int option ;
  }

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
    symbol: string;
    timestamp: Time_ns.t;
    price: float;
    size: int;
    side: Side.t;
  }
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

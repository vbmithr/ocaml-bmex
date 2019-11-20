open Core
open Async
open Bmex_rest

module Cfg = struct
  type cfg = {
    key: string ;
    secret: string ;
    passphrase: string [@default ""];
    quote: (string * int) list [@default []];
  } [@@deriving sexp]

  type t = (string * cfg) list [@@deriving sexp]
end

let default_cfg = Filename.concat (Option.value_exn (Sys.getenv "HOME")) ".virtu"
let cfg =
  List.Assoc.find_exn ~equal:String.equal
    (Sexplib.Sexp.load_sexp_conv_exn default_cfg Cfg.t_of_sexp) "BMEXT"

let wrap_request
    ?(timeout=Time.Span.of_int_sec 5)
    ?(speed=`Quick) n service =
  Alcotest_async.test_case ~timeout n speed begin fun () ->
    (Fastrest.request service) |>
    Deferred.Or_error.ignore |>
    Deferred.Or_error.ok_exn
  end

let wrap_request_light
    ?(timeout=Time.Span.of_int_sec 5)
    ?(speed=`Quick) n f =
  Alcotest_async.test_case ~timeout n speed begin fun () ->
    f () |>
    Deferred.ignore
  end

let raise_or_ignore f =
  f () >>= function
  | Error e -> Error.raise e
  | Ok _v -> Deferred.unit

let orderCycle () =
  let clOrdID = Uuidm.create `V4 in
  let price = 6000. in
  let o = createOrder
      ~price ~symbol:"XBTUSD"
      ~orderQty:1 ~ordType:Limit clOrdID in
  submit ~testnet:true
    ~key:cfg.Cfg.key ~secret:cfg.Cfg.secret [o] >>=? fun ords ->
  let bmxO = List.hd_exn ords in
  let orderID =
    Option.value_exn (Uuidm.of_string (Uuid.to_string bmxO.orderID)) in
  let amds = [createAmend ~orderID ~price:(price +. 100.) ()] in
  amend ~testnet:true ~key:cfg.Cfg.key ~secret:cfg.Cfg.secret amds >>=? fun _ ->
  cancel ~testnet:true ~key:cfg.Cfg.key ~secret:cfg.Cfg.secret ~clOrdIDs:[clOrdID] ()

let rest = [
  Alcotest_async.test_case "instruments" `Quick (fun () ->
      Deferred.ignore (activeInstruments ())) ;
  Alcotest_async.test_case "trades" `Quick (fun () ->
      Deferred.ignore (trades "XBTUSD")) ;
  Alcotest_async.test_case "tradeHistory" `Quick (fun () ->
      Deferred.ignore (tradeHistory ~testnet:true ~key:cfg.Cfg.key ~secret:cfg.Cfg.secret ())) ;
  Alcotest_async.test_case "positions" `Quick (fun () ->
      Deferred.ignore (positions ~testnet:true ~key:cfg.Cfg.key ~secret:cfg.Cfg.secret ())) ;
  Alcotest_async.test_case "openOrders" `Quick (fun () ->
      Deferred.ignore (openOrders ~testnet:true ~key:cfg.Cfg.key ~secret:cfg.Cfg.secret ())) ;
  Alcotest_async.test_case "orderCycle" `Quick (fun () -> raise_or_ignore orderCycle) ;
  Alcotest_async.test_case "deleteAll" `Quick (fun () ->
      Deferred.ignore (cancelAll ~testnet:true ~key:cfg.Cfg.key ~secret:cfg.Cfg.secret ())) ;
  Alcotest_async.test_case "cancelAllAfter" `Quick (fun () ->
      Deferred.ignore (cancelAllAfter ~testnet:true ~key:cfg.Cfg.key ~secret:cfg.Cfg.secret (Time_ns.Span.of_int_sec 10))) ;
]

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level (Some Debug) ;
  Alcotest.run "bmex" [
    "rest", rest ;
  ]

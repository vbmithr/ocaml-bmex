open Core
open Async
open Bmex_rest
open Alcotest_async

let wrap_request
    ?(timeout=Time.Span.of_int_sec 5)
    ?(speed=`Quick) n service =
  test_case ~timeout n speed begin fun () ->
    (Fastrest.request service) |>
    Deferred.Or_error.ignore_m |>
    Deferred.Or_error.ok_exn
  end

let wrap_request_light
    ?(timeout=Time.Span.of_int_sec 5)
    ?(speed=`Quick) n f =
  test_case ~timeout n speed begin fun () ->
    f () |>
    Deferred.ignore_m
  end

let key, secret =
  match String.split ~on:':' (Sys.getenv_exn "TOKEN_BMEXT") with
  | [key; secret] -> key, secret
  | _ -> assert false

let orderCycle () =
  let clOrdID = Uuidm.create `V4 in
  let price = 6000. in
  let o = createOrder
      ~price ~symbol:"XBTUSD"
      ~orderQty:1 ~ordType:Limit clOrdID in
  submit ~testnet:true ~key ~secret [o] >>= fun ords ->
  let bmxO = List.hd_exn ords in
  let amds = [createAmend ~orderID:bmxO.orderID ~price:(price +. 100.) ()] in
  amend ~testnet:true ~key ~secret amds >>= fun _ ->
  cancel ~testnet:true ~key ~secret ~clOrdIDs:[clOrdID] ()

let date =
  Option.value_exn (Ptime.of_date_time ((2019,08,23), ((0,0,0),0)))

let raise_on_error f = Deferred.ignore_m (f ())

let rest = [
  test_case "instruments" `Quick (fun () ->
      raise_on_error (fun () -> activeInstruments ())) ;
  test_case "instrumentsAndIndices" `Quick (fun () ->
      raise_on_error (fun () -> activeAndIndices ())) ;
  test_case "trades" `Quick (fun () ->
      raise_on_error (fun () -> trades "XBTUSD")) ;
  test_case "tradeHistory" `Quick (fun () ->
      raise_on_error (tradeHistory ~testnet:true ~key ~secret)) ;
  test_case "walletHistory" `Quick (fun () ->
      raise_on_error (walletHistory ~testnet:true ~key ~secret)) ;
  test_case "walletSummary" `Quick (fun () ->
      raise_on_error (walletSummary ~testnet:true ~key ~secret)) ;
  test_case "wallet" `Quick (fun () ->
      raise_on_error (wallet ~testnet:true ~key ~secret)) ;
  test_case "executionHistory" `Quick (fun () ->
      raise_on_error (executionHistory ~testnet:true ~key ~secret ~symbol:"XBTUSD" ~ts:date)) ;
  test_case "positions" `Quick (fun () ->
      raise_on_error (positions ~testnet:true ~key ~secret)) ;
  test_case "openOrders" `Quick (fun () ->
      raise_on_error (openOrders ~testnet:true ~key ~secret)) ;
  test_case "orderCycle" `Quick (fun () -> raise_on_error orderCycle) ;
  test_case "deleteAll" `Quick (fun () ->
      raise_on_error (cancelAll ~testnet:true ~key ~secret)) ;
  test_case "cancelAllAfter" `Quick (fun () ->
      raise_on_error (fun () -> cancelAllAfter ~testnet:true ~key ~secret (Time_ns.Span.of_int_sec 10))) ;
]

let main () =
  run "bmex" [
    "rest", rest ;
  ]

let () =
  don't_wait_for (main ()) ;
  never_returns (Scheduler.go ())

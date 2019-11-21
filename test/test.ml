open Core
open Async

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level (Some Info)

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

let rest = [
  Alcotest_async.test_case "instruments" `Quick
    (fun () -> Deferred.ignore (Bmex_rest.Instrument.active ())) ;
]

let () =
  Alcotest.run ~and_exit:false "bmex" [
    "rest", rest ;
  ]

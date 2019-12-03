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
    (Sexplib.Sexp.load_sexp_conv_exn default_cfg Cfg.t_of_sexp) "BMEX"

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
    (fun () -> Deferred.ignore (activeInstruments ())) ;
  Alcotest_async.test_case "trades" `Quick
    (fun () -> Deferred.ignore (trades "XBTUSD")) ;
  (* Alcotest_async.test_case "tradeHistory" `Quick
   *   (fun () -> Deferred.ignore (tradeHistory ~key:cfg.Cfg.key ~secret:cfg.Cfg.secret ())) ; *)
]

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level (Some Debug) ;
  Alcotest.run "bmex" [
    "rest", rest ;
  ]

(**pp -syntax camlp5o -package pa_ppx_stream_seq,bos *)
open OUnit2
open Pa_ppx_testutils

let test ctxt =
  ()

let suite = "Test pa_ppx_stream_seq" >::: [
      "simple"   >:: test
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()


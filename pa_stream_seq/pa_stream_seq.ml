(**pp -syntax camlp5o -package pa_ppx_regexp *)
(* camlp5o *)
(* pa_string.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base
open Pa_ppx_utils
open Pa_passthru
open Ppxutil

let install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            expr = extfun ef.expr with [
    <:expr:< [%stream_parser $str:_$ ] >> as z ->
    fun arg fallback ->
      Some z
  ] } in

  Pa_passthru.(install { name = "pa_stream_seq"; ef =  ef ; pass = None ; before = [] ; after = [] })
;;

install();;

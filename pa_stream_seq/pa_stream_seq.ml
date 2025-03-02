(**pp -syntax camlp5o -package pa_ppx_regexp *)
(* camlp5o *)
(* pa_string.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base
open Pa_ppx_utils
open Pa_passthru
open Ppxutil

let do_parse_expr str = (Grammar.Entry.parse Pcaml.expr_eoi) (Stream.of_string str) ;;

let eval_anti entry loc typ str : Ploc.t * MLast.expr =
  let loc =
    let sh =
      if typ = "" then String.length "$"
      else
        String.length "$" + String.length typ + String.length ":"
    in
    let len = String.length str in
    Ploc.sub loc sh len
  in
  let r =
    try
      Ploc.call_with Plexer.force_antiquot_loc false
        do_parse_expr str
    with
    Ploc.Exc(loc1, exc) ->
        let shift = Ploc.first_pos loc in
        let loc =
          Ploc.make_loc (Ploc.file_name loc)
            (Ploc.line_nb loc + Ploc.line_nb loc1 - 1)
            (if Ploc.line_nb loc1 = 1 then Ploc.bol_pos loc
             else shift + Ploc.bol_pos loc1)
            (shift + Ploc.first_pos loc1,
             shift + Ploc.last_pos loc1) ""
          in
          raise (Ploc.Exc(loc, exc))
  in
  (loc, r)

let reloc_to_subloc ~enclosed subloc =
  Ploc.(sub enclosed (first_pos subloc) (last_pos subloc))

let parse_expr loc str =
  let (_,e) = eval_anti Pcaml.expr_eoi loc "" str in
  let shift = 0 in
  Reloc.expr (fun subloc -> reloc_to_subloc ~enclosed:loc subloc) shift e

let stream_parser_expr loc s =
  parse_expr loc s

let rewrite_stream_parser arg = function
  <:expr< [%stream_parser $exp:e$ ] >> ->
   let (loc, s) = match e with <:expr< $locstr:(loc,s)$ >> -> (loc, Pcaml.unvala s) in
   stream_parser_expr loc (s |> Scanf.unescaped)

| _ -> assert false

let stream_expr_expr loc s =
  parse_expr loc s

let rewrite_stream_expr arg = function
  <:expr< [%stream_expr $exp:e$ ] >> ->
   let (loc, s) = match e with <:expr< $locstr:(loc,s)$ >> -> (loc, Pcaml.unvala s) in
   stream_expr_expr loc (s |> Scanf.unescaped)

| _ -> assert false

let install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            expr = extfun ef.expr with [
    <:expr:< [%stream_parser $str:_$] >> as z ->
    fun arg fallback ->
    Some (rewrite_stream_parser arg z)
  | <:expr:< [%stream_expr $str:_$] >> as z ->
    fun arg fallback ->
    Some (rewrite_stream_expr arg z)
  ] } in

  Pa_passthru.(install { name = "pa_stream_seq"; ef =  ef ; pass = None ; before = [] ; after = [] })
;;

install();;

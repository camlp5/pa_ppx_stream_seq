(**pp -syntax camlp5o -package pa_ppx_regexp,camlp5.extend,camlp5 *)
(* camlp5o *)
(* pa_string.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base
open Pa_ppx_utils
open Pa_passthru
open Ppxutil
open Pcaml

let stream_expr_eoi = Grammar.Entry.create gram "stream_expr_eoi";;
let stream_parser_eoi = Grammar.Entry.create gram "stream_parser_eoi";;
let stream_match_eoi = Grammar.Entry.create gram "stream_match_eoi";;

EXTEND
  GLOBAL: stream_expr stream_expr_eoi stream_parser stream_parser_eoi stream_match stream_match_eoi;

  stream_expr_eoi:
    [ [ x = stream_expr; EOI -> x ] ]
  ;
  stream_match_eoi:
    [ [ x = stream_match; EOI -> x ] ]
  ;
  stream_parser_eoi:
    [ [ x = stream_parser; EOI -> x ] ]
  ;
END;;

let do_parse_stream_parser str = (Grammar.Entry.parse stream_parser_eoi) (Stream.of_string str) ;;
let do_parse_match_expr str = (Grammar.Entry.parse stream_match_eoi) (Stream.of_string str) ;;

let eval_anti pafun loc typ str =
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
        pafun str
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

module StreamExpr = struct
let do_parse str = (Grammar.Entry.parse stream_expr_eoi) (Stream.of_string str) ;;

let reloc floc shift x = x

let parse loc str =
  let (_,e) = eval_anti do_parse loc "" str in
  let shift = 0 in
  reloc (fun subloc -> reloc_to_subloc ~enclosed:loc subloc) shift e

let to_expr loc str =
  let (loc, sel) = parse loc str in
  Exparser.cstream loc sel

let rewrite arg = function
  <:expr< [%stream_expr $exp:e$ ] >> ->
   let (loc, s) = match e with <:expr< $locstr:(loc,s)$ >> -> (loc, Pcaml.unvala s) in
   to_expr loc (s |> Scanf.unescaped)

| _ -> assert false
end

module StreamParser = struct
let do_parse str = (Grammar.Entry.parse stream_parser_eoi) (Stream.of_string str) ;;

let reloc floc shift x = x

let parse loc str =
  let (_,e) = eval_anti do_parse loc "" str in
  let shift = 0 in
  reloc (fun subloc -> reloc_to_subloc ~enclosed:loc subloc) shift e

let to_expr loc str =
  let (loc, (po, pcl)) = parse loc str in
  Exparser.cparser loc (po, pcl)

let rewrite arg = function
  <:expr< [%stream_parser $exp:e$ ] >> ->
   let (loc, s) = match e with <:expr< $locstr:(loc,s)$ >> -> (loc, Pcaml.unvala s) in
   to_expr loc (s |> Scanf.unescaped)

| _ -> assert false
end

let install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            expr = extfun ef.expr with [
    <:expr:< [%stream_expr $str:_$] >> as z ->
    fun arg fallback ->
    Some (StreamExpr.rewrite arg z)

  | <:expr:< [%stream_parser $str:_$] >> as z ->
    fun arg fallback ->
    Some (StreamParser.rewrite arg z)

  ] } in

  Pa_passthru.(install { name = "pa_stream_seq"; ef =  ef ; pass = None ; before = [] ; after = [] })
;;

install();;

(* Cross-check the compact payload decoder against the independently
 * typechecked and extracted Rocq test tuples.  This executable is an audit
 * gate; production campaigns do not link the static Tests modules. *)

open Fragment

let rec list_eq equal left right =
  match left, right with
  | [], [] -> true
  | x :: xs, y :: ys -> equal x y && list_eq equal xs ys
  | _ -> false

let option_eq equal left right =
  match left, right with
  | None, None -> true
  | Some x, Some y -> equal x y
  | _ -> false

let pair_eq equal_left equal_right (left_a, left_b) (right_a, right_b) =
  equal_left left_a right_a && equal_right left_b right_b

let int_eq (left : int) right = left = right
let bool_eq (left : bool) right = left = right
let string_eq (left : string) right = left = right
let integer_eq = Big_int_Z.eq_big_int
let cstr_eq = list_eq integer_eq

let float_eq left right =
  match Float64.classify left, Float64.classify right with
  | Float64.NaN, Float64.NaN -> true
  | _ -> Float64.to_hex_string left = Float64.to_hex_string right

let lexval_eq left right =
  match left, right with
  | LVStr left, LVStr right -> cstr_eq left right
  | LVMath left, LVMath right -> integer_eq left right
  | LVNumber left, LVNumber right -> float_eq left right
  | LVBigInt left, LVBigInt right -> integer_eq left right
  | LVUndef, LVUndef -> true
  | _ -> false

let rec ast_eq left right =
  match left, right with
  | ASyn
      (left_name, left_args, left_rhs, left_sub, left_children,
       left_child_names, left_source, left_parse_source),
    ASyn
      (right_name, right_args, right_rhs, right_sub, right_children,
       right_child_names, right_source, right_parse_source) ->
      left_name = right_name
      && list_eq bool_eq left_args right_args
      && left_rhs = right_rhs
      && left_sub = right_sub
      && list_eq (option_eq ast_eq) left_children right_children
      && list_eq string_eq left_child_names right_child_names
      && cstr_eq left_source right_source
      && cstr_eq left_parse_source right_parse_source
  | ALex
      (left_name, left_lexeme, left_source, left_parse_source, left_sdos),
    ALex
      (right_name, right_lexeme, right_source, right_parse_source, right_sdos) ->
      left_name = right_name
      && left_lexeme = right_lexeme
      && cstr_eq left_source right_source
      && cstr_eq left_parse_source right_parse_source
      && list_eq
           (pair_eq string_eq lexval_eq)
           left_sdos right_sdos
  | _ -> false

let ast_origin_eq left right =
  match left, right with
  | AstExported left, AstExported right -> left = right
  | AstRuntime left, AstRuntime right -> left = right
  | _ -> false

let rec value_eq left right =
  match left, right with
  | VMath left, VMath right -> integer_eq left right
  | VBool left, VBool right -> left = right
  | VStr left, VStr right -> cstr_eq left right
  | VUndef, VUndef
  | VNull, VNull -> true
  | VEnum left, VEnum right -> left = right
  | VAddr left, VAddr right -> left = right
  | VClo (left_fn, left_captured), VClo (right_fn, right_captured) ->
      left_fn = right_fn
      && list_eq
           (pair_eq string_eq value_eq)
           left_captured right_captured
  | VCont (left_fn, left_captured, left_stack),
    VCont (right_fn, right_captured, right_stack) ->
      left_fn = right_fn
      && list_eq
           (pair_eq string_eq value_eq)
           left_captured right_captured
      && option_eq int_eq left_stack right_stack
  | VAst (left_origin, left_ast, left_path),
    VAst (right_origin, right_ast, right_path) ->
      ast_origin_eq left_origin right_origin
      && ast_eq left_ast right_ast
      && list_eq int_eq left_path right_path
  | VNumber left, VNumber right -> float_eq left right
  | VBigInt left, VBigInt right -> integer_eq left right
  | VInfinity left, VInfinity right -> left = right
  | VCodeUnit left, VCodeUnit right -> integer_eq left right
  | VGrammarSymbol (left_name, left_params),
    VGrammarSymbol (right_name, right_params) ->
      left_name = right_name && list_eq bool_eq left_params right_params
  | _ -> false

let host_query_eq left right =
  match left, right with
  | HQParseText (left_text, left_rule, left_params),
    HQParseText (right_text, right_rule, right_params) ->
      cstr_eq left_text right_text
      && left_rule = right_rule
      && list_eq bool_eq left_params right_params
  | HQToStr (left_value, left_radix),
    HQToStr (right_value, right_radix) ->
      value_eq left_value right_value && integer_eq left_radix right_radix
  | HQStrToNumber left, HQStrToNumber right -> cstr_eq left right
  | HQNumberPow (left_base, left_exponent),
    HQNumberPow (right_base, right_exponent) ->
      float_eq left_base right_base && float_eq left_exponent right_exponent
  | HQDoubleToLongChecked left, HQDoubleToLongChecked right ->
      float_eq left right
  | HQStrToBigInt left, HQStrToBigInt right -> cstr_eq left right
  | HQMathOp (left_op, left_args), HQMathOp (right_op, right_args) ->
      left_op = right_op && list_eq integer_eq left_args right_args
  | HQMathToNumber left, HQMathToNumber right -> integer_eq left right
  | HQNumberMathOp (left_op, left_lhs, left_rhs),
    HQNumberMathOp (right_op, right_lhs, right_rhs) ->
      left_op = right_op
      && float_eq left_lhs right_lhs
      && float_eq left_rhs right_rhs
  | HQNumberSin left, HQNumberSin right -> float_eq left right
  | HQNumberMathCompare (left_op, left_direction, left_number, left_integer),
    HQNumberMathCompare
      (right_op, right_direction, right_number, right_integer) ->
      left_op = right_op
      && left_direction = right_direction
      && float_eq left_number right_number
      && integer_eq left_integer right_integer
  | HQNumberToMath left, HQNumberToMath right -> float_eq left right
  | _ -> false

let host_entry_eq left right =
  host_query_eq left.hc_query right.hc_query
  && value_eq left.hc_result right.hc_result

let test_input_eq left right =
  let ((((left_name, left_source), left_ast), left_hosts),
       (left_result, left_prints)) = left
  and ((((right_name, right_source), right_ast), right_hosts),
       (right_result, right_prints)) = right in
  left_name = right_name
  && cstr_eq left_source right_source
  && ast_eq left_ast right_ast
  && list_eq host_entry_eq left_hosts right_hosts
  && value_eq left_result right_result
  && list_eq value_eq left_prints right_prints

let payload_dir () =
  if Array.length Sys.argv >= 2 then Sys.argv.(1)
  else "validation/payload"

let check_mop_tags () =
  let expected =
    [ MExpm1; MLog10; MLog2; MCos; MCbrt; MExp; MCosh; MSinh; MTanh;
      MAcos; MAcosh; MAsinh; MAtanh; MAsin; MAtan2; MAtan; MLog1p;
      MLog; MSin; MSqrt; MTan ]
  in
  if Array.to_list Payload_codec.mop_tags <> expected then begin
    Printf.eprintf "mathematical operator payload-tag table mismatch\n%!";
    exit 1
  end;
  Printf.printf "mathematical operator payload tags: 21 / 21 exact\n%!"

let check_math_to_number_query_equality () =
  let positive = Big_int_Z.big_int_of_string "9007199254740993" in
  let negative = Big_int_Z.minus_big_int positive in
  if not (host_query_eq (HQMathToNumber positive) (HQMathToNumber positive))
     || host_query_eq (HQMathToNumber positive) (HQMathToNumber negative)
  then begin
    Printf.eprintf "Math-to-Number host-query equality mismatch\n%!";
    exit 1
  end;
  Printf.printf "Math-to-Number host-query equality: exact\n%!"

let check_number_math_query_equality () =
  let pos_zero = Float64.of_float 0.0
  and neg_zero = Float64.of_float (-0.0)
  and one = Float64.of_float 1.0 in
  if not
       (host_query_eq
          (HQNumberMathOp (NMAdd, pos_zero, one))
          (HQNumberMathOp (NMAdd, pos_zero, one)))
     || host_query_eq
          (HQNumberMathOp (NMAdd, pos_zero, one))
          (HQNumberMathOp (NMMul, pos_zero, one))
     || host_query_eq
          (HQNumberMathOp (NMAdd, pos_zero, one))
          (HQNumberMathOp (NMAdd, neg_zero, one))
  then begin
    Printf.eprintf "Number/Math host-query equality mismatch\n%!";
    exit 1
  end;
  Printf.printf "Number/Math host-query equality: exact\n%!"

let check_number_sin_query_equality () =
  let pos_zero = Float64.of_float 0.0
  and neg_zero = Float64.of_float (-0.0) in
  if not (host_query_eq (HQNumberSin pos_zero) (HQNumberSin pos_zero))
     || host_query_eq (HQNumberSin pos_zero) (HQNumberSin neg_zero)
  then begin
    Printf.eprintf "Number/sin host-query equality mismatch\n%!";
    exit 1
  end;
  Printf.printf "Number/sin host-query equality: exact\n%!"

let check_number_math_compare_query_equality () =
  let pos_zero = Float64.of_float 0.0
  and neg_zero = Float64.of_float (-0.0)
  and integer = Big_int_Z.big_int_of_int 0 in
  let base =
    HQNumberMathCompare (NMCLt, NMCNumberLeft, pos_zero, integer) in
  if not (host_query_eq base base)
     || host_query_eq base
          (HQNumberMathCompare (NMCEqual, NMCNumberLeft, pos_zero, integer))
     || host_query_eq base
          (HQNumberMathCompare (NMCLt, NMCNumberRight, pos_zero, integer))
     || host_query_eq base
          (HQNumberMathCompare (NMCLt, NMCNumberLeft, neg_zero, integer))
  then begin
    Printf.eprintf "Number/Math comparison host-query equality mismatch\n%!";
    exit 1
  end;
  Printf.printf "Number/Math comparison host-query equality: exact\n%!"

let check_number_to_math_query_equality () =
  let pos_zero = Float64.of_float 0.0
  and neg_zero = Float64.of_float (-0.0) in
  if not (host_query_eq (HQNumberToMath pos_zero) (HQNumberToMath pos_zero))
     || host_query_eq (HQNumberToMath pos_zero) (HQNumberToMath neg_zero)
  then begin
    Printf.eprintf "Number-to-Math host-query equality mismatch\n%!";
    exit 1
  end;
  Printf.printf "Number-to-Math host-query equality: exact\n%!"

let () =
  check_mop_tags ();
  check_math_to_number_query_equality ();
  check_number_math_query_equality ();
  check_number_sin_query_equality ();
  check_number_math_compare_query_equality ();
  check_number_to_math_query_equality ();
  let passed = ref 0 in
  List.iteri
    (fun index extracted ->
      let path =
        Filename.concat (payload_dir ()) (Printf.sprintf "T%03d.fvt" index)
      in
      try
        let decoded = Payload_codec.load path in
        if test_input_eq extracted decoded.test_input then incr passed
        else begin
          Printf.eprintf
            "T%03d tuple mismatch after compact-payload round trip: %s\n%!"
            index path;
          exit 1
        end
      with
      | Payload_codec.Decode_error message ->
          Printf.eprintf "T%03d decode error: %s\n%!" index message;
          exit 1)
    Tests.tests;
  let total = List.length Tests.tests in
  Printf.printf "compact payload round trip: %d / %d exact\n%!" !passed total;
  if !passed <> total then exit 1

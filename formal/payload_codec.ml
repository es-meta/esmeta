(* Decoder for FVPayload version 7.
 *
 * This module constructs only data in the Rocq-extracted Fragment types.
 * The executable semantics still comes exclusively from ITreeCore. *)

open Fragment

exception Decode_error of string

type test_input =
  ((((string * cstr) * ast) * host_cache_entry list)
   * (coq_val * coq_val list))

type payload = {
  global_index : int;
  test_input : test_input;
}

type decoder = {
  data : bytes;
  mutable offset : int;
}

let magic = "ESFVIT07"
let max_payload_bytes = 512 * 1024 * 1024
let max_collection_length = 16 * 1024 * 1024
let max_ast_depth = 100_000

let fail decoder format =
  Printf.ksprintf
    (fun message ->
      raise
        (Decode_error
           (Printf.sprintf "byte %d: %s" decoder.offset message)))
    format

let remaining decoder = Bytes.length decoder.data - decoder.offset

let require decoder count =
  if count < 0 || remaining decoder < count then
    fail decoder "truncated payload (need %d byte(s), have %d)"
      count (remaining decoder)

let read_u8 decoder =
  require decoder 1;
  let value = Char.code (Bytes.get decoder.data decoder.offset) in
  decoder.offset <- decoder.offset + 1;
  value

let read_u16 decoder =
  let high = read_u8 decoder
  and low = read_u8 decoder in
  (high lsl 8) lor low

let read_u32_nat decoder =
  let a = Int64.of_int (read_u8 decoder)
  and b = Int64.of_int (read_u8 decoder)
  and c = Int64.of_int (read_u8 decoder)
  and d = Int64.of_int (read_u8 decoder) in
  let value =
    Int64.logor
      (Int64.shift_left a 24)
      (Int64.logor
         (Int64.shift_left b 16)
         (Int64.logor (Int64.shift_left c 8) d))
  in
  if Int64.compare value 2_147_483_647L > 0 then
    fail decoder "natural is too large: %Ld" value;
  Int64.to_int value

let read_length decoder kind =
  let length = read_u32_nat decoder in
  if length > max_collection_length then
    fail decoder "%s length %d exceeds limit %d"
      kind length max_collection_length;
  length

let read_bytes decoder kind =
  let length = read_length decoder kind in
  require decoder length;
  let value = Bytes.sub_string decoder.data decoder.offset length in
  decoder.offset <- decoder.offset + length;
  value

let invalid_utf8 decoder kind index =
  fail decoder "%s contains invalid UTF-8 at relative byte %d" kind index

let validate_utf8 decoder kind value =
  let length = String.length value in
  let byte index = Char.code value.[index] in
  let continuation index =
    index < length && let value = byte index in 0x80 <= value && value <= 0xbf
  in
  let rec loop index =
    if index = length then ()
    else
      let first = byte index in
      if first <= 0x7f then loop (index + 1)
      else if 0xc2 <= first && first <= 0xdf then
        if continuation (index + 1) then loop (index + 2)
        else invalid_utf8 decoder kind index
      else if first = 0xe0 then
        if index + 2 < length
           && 0xa0 <= byte (index + 1) && byte (index + 1) <= 0xbf
           && continuation (index + 2)
        then loop (index + 3)
        else invalid_utf8 decoder kind index
      else if (0xe1 <= first && first <= 0xec)
           || (0xee <= first && first <= 0xef)
      then
        if continuation (index + 1) && continuation (index + 2)
        then loop (index + 3)
        else invalid_utf8 decoder kind index
      else if first = 0xed then
        if index + 2 < length
           && 0x80 <= byte (index + 1) && byte (index + 1) <= 0x9f
           && continuation (index + 2)
        then loop (index + 3)
        else invalid_utf8 decoder kind index
      else if first = 0xf0 then
        if index + 3 < length
           && 0x90 <= byte (index + 1) && byte (index + 1) <= 0xbf
           && continuation (index + 2) && continuation (index + 3)
        then loop (index + 4)
        else invalid_utf8 decoder kind index
      else if 0xf1 <= first && first <= 0xf3 then
        if continuation (index + 1)
           && continuation (index + 2)
           && continuation (index + 3)
        then loop (index + 4)
        else invalid_utf8 decoder kind index
      else if first = 0xf4 then
        if index + 3 < length
           && 0x80 <= byte (index + 1) && byte (index + 1) <= 0x8f
           && continuation (index + 2) && continuation (index + 3)
        then loop (index + 4)
        else invalid_utf8 decoder kind index
      else invalid_utf8 decoder kind index
  in
  loop 0

let read_utf8 decoder kind =
  let value = read_bytes decoder kind in
  validate_utf8 decoder kind value;
  value

let read_bool decoder =
  match read_u8 decoder with
  | 0 -> false
  | 1 -> true
  | tag -> fail decoder "invalid Boolean tag %d" tag

let read_list decoder kind read =
  let length = read_length decoder kind in
  List.init length (fun _ -> read decoder)

let read_cstr decoder =
  read_list decoder "UTF-16 string" (fun decoder ->
    Big_int_Z.big_int_of_int (read_u16 decoder))

let read_integer decoder =
  let source = read_utf8 decoder "integer" in
  try Big_int_Z.big_int_of_string source
  with _ -> fail decoder "invalid decimal integer %S" source

let read_i64 decoder =
  let result = ref 0L in
  for _ = 1 to 8 do
    result :=
      Int64.logor
        (Int64.shift_left !result 8)
        (Int64.of_int (read_u8 decoder))
  done;
  !result

let read_float64 decoder =
  match read_u8 decoder with
  | 0 -> Float64.of_float (Int64.float_of_bits (read_i64 decoder))
  | 1 -> PrimFloat.nan
  | 2 -> PrimFloat.infinity
  | 3 -> PrimFloat.neg_infinity
  | tag -> fail decoder "invalid float64 tag %d" tag

let mop_tags =
  [| MExpm1; MLog10; MLog2; MCos; MCbrt; MExp; MCosh; MSinh; MTanh;
     MAcos; MAcosh; MAsinh; MAtanh; MAsin; MAtan2; MAtan; MLog1p;
     MLog; MSin; MSqrt; MTan |]

let read_mop decoder =
  let tag = read_u8 decoder in
  if tag < Array.length mop_tags then mop_tags.(tag)
  else fail decoder "invalid mathematical operator tag %d" tag

let number_math_op_tags = [| NMAdd; NMMul; NMDiv; NMPow |]

let read_number_math_op decoder =
  let tag = read_u8 decoder in
  if tag < Array.length number_math_op_tags then number_math_op_tags.(tag)
  else fail decoder "invalid Number/Math operator tag %d" tag

let number_math_compare_op_tags = [| NMCLt; NMCEqual |]

let read_number_math_compare_op decoder =
  let tag = read_u8 decoder in
  if tag < Array.length number_math_compare_op_tags
  then number_math_compare_op_tags.(tag)
  else fail decoder "invalid Number/Math comparison operator tag %d" tag

let number_math_compare_direction_tags =
  [| NMCNumberLeft; NMCNumberRight |]

let read_number_math_compare_direction decoder =
  let tag = read_u8 decoder in
  if tag < Array.length number_math_compare_direction_tags
  then number_math_compare_direction_tags.(tag)
  else fail decoder "invalid Number/Math comparison direction tag %d" tag

let rec read_ast depth decoder =
  if depth > max_ast_depth then
    fail decoder "AST depth exceeds limit %d" max_ast_depth;
  match read_u8 decoder with
  | 0 ->
      let name = read_utf8 decoder "syntactic AST name" in
      let args = read_list decoder "syntactic arguments" (fun d -> read_bool d) in
      let rhs_index = read_u32_nat decoder in
      let sub_index = read_u32_nat decoder in
      let children =
        read_list decoder "syntactic children" (fun decoder ->
          match read_u8 decoder with
          | 0 -> None
          | 1 -> Some (read_ast (depth + 1) decoder)
          | tag -> fail decoder "invalid AST option tag %d" tag)
      in
      let child_names =
        read_list decoder "syntactic child names" (fun decoder ->
          read_utf8 decoder "syntactic child name")
      in
      let source = read_cstr decoder in
      let parse_source = read_cstr decoder in
      ASyn
        (name, args, rhs_index, sub_index, children, child_names,
         source, parse_source)
  | 1 ->
      let name = read_utf8 decoder "lexical AST name" in
      let lexeme = read_utf8 decoder "lexical AST lexeme" in
      let source = read_cstr decoder in
      let parse_source = read_cstr decoder in
      let sdos =
        read_list decoder "lexical SDO table" (fun decoder ->
          let method_name = read_utf8 decoder "lexical SDO name" in
          let value =
            match read_u8 decoder with
            | 0 -> LVStr (read_cstr decoder)
            | 1 -> LVMath (read_integer decoder)
            | 2 -> LVNumber (read_float64 decoder)
            | 3 -> LVBigInt (read_integer decoder)
            | 4 -> LVUndef
            | tag -> fail decoder "invalid lexical value tag %d" tag
          in
          method_name, value)
      in
      ALex (name, lexeme, source, parse_source, sdos)
  | tag -> fail decoder "invalid AST tag %d" tag

let read_ast_origin decoder =
  match read_u8 decoder with
  | 0 -> AstExported (read_u32_nat decoder)
  | 1 -> AstRuntime (read_u32_nat decoder)
  | tag -> fail decoder "invalid AST origin tag %d" tag

let read_cont_stack decoder =
  match read_u8 decoder with
  | 0 -> None
  | 1 -> Some (read_u32_nat decoder)
  | tag -> fail decoder "invalid continuation stack tag %d" tag

let rec read_value decoder =
  match read_u8 decoder with
  | 0 -> VMath (read_integer decoder)
  | 1 -> VBool (read_bool decoder)
  | 2 -> VStr (read_cstr decoder)
  | 3 -> VUndef
  | 4 -> VNull
  | 5 -> VEnum (read_utf8 decoder "enum")
  | 6 -> VAddr (read_u32_nat decoder)
  | 7 ->
      let fn = read_utf8 decoder "closure function" in
      let captured =
        read_list decoder "closure captures" (fun decoder ->
          let name = read_utf8 decoder "closure capture name" in
          name, read_value decoder)
      in
      VClo (fn, captured)
  | 8 ->
      let fn = read_utf8 decoder "continuation function" in
      let captured =
        read_list decoder "continuation captures" (fun decoder ->
          let name = read_utf8 decoder "continuation capture name" in
          name, read_value decoder)
      in
      VCont (fn, captured, read_cont_stack decoder)
  | 9 ->
      let origin = read_ast_origin decoder in
      let root = read_ast 0 decoder in
      let path =
        read_list decoder "AST cursor path" (fun decoder ->
          read_u32_nat decoder)
      in
      VAst (origin, root, path)
  | 10 -> VNumber (read_float64 decoder)
  | 11 -> VBigInt (read_integer decoder)
  | 12 -> VInfinity (read_bool decoder)
  | 13 -> VCodeUnit (Big_int_Z.big_int_of_int (read_u16 decoder))
  | 14 ->
      let name = read_utf8 decoder "grammar symbol" in
      let params =
        read_list decoder "grammar symbol parameters" (fun d -> read_bool d)
      in
      VGrammarSymbol (name, params)
  | tag -> fail decoder "invalid value tag %d" tag

let read_host_entry decoder =
  let query =
    match read_u8 decoder with
    | 0 ->
        let text = read_cstr decoder in
        let rule = read_utf8 decoder "parse rule" in
        let params =
          read_list decoder "parse parameters" (fun d -> read_bool d)
        in
        HQParseText (text, rule, params)
    | 1 ->
        let input = read_value decoder in
        let radix = read_integer decoder in
        HQToStr (input, radix)
    | 2 -> HQStrToNumber (read_cstr decoder)
    | 3 ->
        let left = read_float64 decoder in
        let right = read_float64 decoder in
        HQNumberPow (left, right)
    | 4 -> HQDoubleToLongChecked (read_float64 decoder)
    | 5 -> HQStrToBigInt (read_cstr decoder)
    | 6 ->
        let op = read_mop decoder in
        let args = read_list decoder "mathematical arguments" read_integer in
        HQMathOp (op, args)
    | 7 -> HQMathToNumber (read_integer decoder)
    | 8 ->
        let op = read_number_math_op decoder in
        let left = read_float64 decoder in
        let right = read_float64 decoder in
        HQNumberMathOp (op, left, right)
    | 9 -> HQNumberSin (read_float64 decoder)
    | 10 ->
        let op = read_number_math_compare_op decoder in
        let direction = read_number_math_compare_direction decoder in
        let number = read_float64 decoder in
        let integer = read_integer decoder in
        HQNumberMathCompare (op, direction, number, integer)
    | 11 -> HQNumberToMath (read_float64 decoder)
    | tag -> fail decoder "invalid host query tag %d" tag
  in
  let result = read_value decoder in
  { hc_query = query; hc_result = result }

let decode data =
  let decoder = { data; offset = 0 } in
  require decoder (String.length magic);
  let actual_magic =
    Bytes.sub_string decoder.data decoder.offset (String.length magic)
  in
  decoder.offset <- decoder.offset + String.length magic;
  if actual_magic <> magic then
    fail decoder "invalid payload magic %S" actual_magic;
  let global_index = read_u32_nat decoder in
  let name = read_utf8 decoder "test name" in
  let source = read_cstr decoder in
  let root = read_ast 0 decoder in
  let hosts =
    read_list decoder "host cache" read_host_entry
  in
  let expected_result = read_value decoder in
  let expected_prints =
    read_list decoder "expected prints" read_value
  in
  if remaining decoder <> 0 then
    fail decoder "trailing data (%d byte(s))" (remaining decoder);
  {
    global_index;
    test_input =
      ((((name, source), root), hosts),
       (expected_result, expected_prints));
  }

let load path =
  let channel =
    try open_in_bin path
    with Sys_error message -> raise (Decode_error message)
  in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let length = in_channel_length channel in
      if length > max_payload_bytes then
        raise
          (Decode_error
             (Printf.sprintf "payload is %d bytes; limit is %d"
                length max_payload_bytes));
      let data = Bytes.create length in
      really_input channel data 0 length;
      decode data)

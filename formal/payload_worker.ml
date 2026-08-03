(* Persistent, core-only Test262 worker.
 *
 * Protocol (ASCII, one request/response per line):
 *   READY 1
 *   RUN <fuel> <local-index> <global-index> <diagnostics:0|1>
 *       <hex-expected-name> <hex-payload-path>
 *   RESULT <hex-verdict-line>
 *   ERROR <hex-message>
 *   QUIT
 *   BYE
 *
 * Hex fields keep paths and Test262 names unambiguous without adding a JSON
 * dependency to the trusted runner boundary. *)

open ITreeCore
open Itree_test_runtime

let hex_digit value =
  if value < 10 then Char.chr (Char.code '0' + value)
  else Char.chr (Char.code 'a' + value - 10)

let hex_of_string value =
  let result = Bytes.create (String.length value * 2) in
  String.iteri
    (fun index character ->
      let code = Char.code character in
      Bytes.set result (index * 2) (hex_digit (code lsr 4));
      Bytes.set result (index * 2 + 1) (hex_digit (code land 0x0f)))
    value;
  Bytes.unsafe_to_string result

let nibble = function
  | '0' .. '9' as value -> Char.code value - Char.code '0'
  | 'a' .. 'f' as value -> Char.code value - Char.code 'a' + 10
  | 'A' .. 'F' as value -> Char.code value - Char.code 'A' + 10
  | value -> invalid_arg (Printf.sprintf "invalid hex digit %C" value)

let string_of_hex field value =
  let length = String.length value in
  if length mod 2 <> 0 then
    invalid_arg (field ^ " has odd-length hex encoding");
  String.init (length / 2) (fun index ->
    let high = nibble value.[index * 2]
    and low = nibble value.[index * 2 + 1] in
    Char.chr ((high lsl 4) lor low))

let parse_nonnegative field source =
  match int_of_string_opt source with
  | Some value when value >= 0 -> value
  | _ -> invalid_arg (field ^ " must be a nonnegative integer")

let parse_positive field source =
  match int_of_string_opt source with
  | Some value when value > 0 -> value
  | _ -> invalid_arg (field ^ " must be a positive integer")

let test_name
    ((((name, _source), _root), _hosts), (_result, _prints)) =
  name

let respond kind message =
  Printf.printf "%s %s\n%!" kind (hex_of_string message)

let execute fields =
  match fields with
  | ["RUN"; fuel_source; local_source; global_source;
     diagnostics_source; expected_name_hex; payload_path_hex] ->
      let fuel = parse_positive "fuel" fuel_source
      and local_index = parse_nonnegative "local index" local_source
      and expected_global = parse_nonnegative "global index" global_source
      and expected_name = string_of_hex "expected name" expected_name_hex
      and payload_path = string_of_hex "payload path" payload_path_hex in
      let diagnostics =
        match diagnostics_source with
        | "0" -> false
        | "1" -> true
        | _ -> invalid_arg "diagnostics must be 0 or 1"
      in
      let payload = Payload_codec.load payload_path in
      if payload.Payload_codec.global_index <> expected_global then
        failwith
          (Printf.sprintf
             "payload global index mismatch: expected %d, decoded %d"
             expected_global payload.Payload_codec.global_index);
      let actual_name = test_name payload.Payload_codec.test_input in
      if actual_name <> expected_name then
        failwith
          (Printf.sprintf
             "payload test name mismatch: expected %S, decoded %S"
             expected_name actual_name);
      let test = make_test_tree payload.Payload_codec.test_input in
      let elapsed, verdict =
        evaluate_test
          ~fuel ~diagnostics ~trace_func:None test
      in
      respond "RESULT"
        (format_verdict
           ~index:local_index ~name:actual_name ~elapsed verdict)
  | _ -> invalid_arg "malformed RUN request"

let () =
  Printf.printf "READY 1\n%!";
  let rec loop () =
    match input_line stdin with
    | exception End_of_file -> ()
    | "QUIT" ->
        Printf.printf "BYE\n%!"
    | line ->
        let fields =
          String.split_on_char ' ' line
          |> List.filter (fun field -> field <> "")
        in
        (try execute fields with
         | Payload_codec.Decode_error message ->
             respond "ERROR" ("decode error: " ^ message)
         | exn ->
             respond "ERROR" (Printexc.to_string exn));
        loop ()
  in
  loop ()

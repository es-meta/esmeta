(* Shared Test262 execution and verdict policy for the modular shard driver
 * and the persistent compact-payload worker. *)

open Fragment
open Domain
open Events
open ITreeDefinition
open ITreeCore

type outcome =
  | Completed of coq_val * coq_val list * int
  | Unsupported_effect of string * string list * string option * int
  | Out_of_fuel of int
  | Crashed of string * int

type verdict =
  | Pass of int
  | Result_differs of int * string * string
  | Prints_differ of int * int * int
  | Ast_aliases_differ of int
  | Unsupported of int * string
  | Fuel_exhausted of int
  | Crash of int * string

type category =
  | Matched
  | Mismatched
  | Unsupported_category
  | Out_of_fuel_category
  | Crashed_category

let pop_call expected = function
  | actual :: rest when actual = expected -> rest
  | calls -> calls

let observe_safely tree =
  try Result.Ok (observe tree)
  with exn -> Result.Error (Printexc.to_string exn)

let continue_safely continue value =
  try Result.Ok (continue value)
  with exn -> Result.Error (Printexc.to_string exn)

let has_prefix prefix value =
  let prefix_len = Stdlib.String.length prefix
  and value_len = Stdlib.String.length value in
  value_len >= prefix_len
    && Stdlib.String.sub value 0 prefix_len = prefix

let rec run trace_markers fuel steps prints calls last_inst tree =
  if fuel = 0 then Out_of_fuel steps
  else
    match observe_safely tree with
    | Result.Error why -> Crashed (why, steps)
    | Result.Ok (RetF value) -> Completed (value, List.rev prints, steps)
    | Result.Ok (TauF next) ->
        run trace_markers (fuel - 1) (steps + 1) prints calls last_inst next
    | Result.Ok (VisF (event, continue)) ->
        begin match event with
        | IO (name, arg) when name = "esmeta.print" ->
            let value : coq_val = Obj.obj arg in
            begin match continue_safely continue (Obj.repr ()) with
            | Result.Error why -> Crashed (why, steps)
            | Result.Ok next ->
              begin match value with
              | VEnum marker
                when trace_markers
                  && has_prefix "$ESMetaFV.trace.inst:" marker ->
                  run trace_markers (fuel - 1) (steps + 1) prints calls
                    (Some marker) next
              | _ ->
                  run trace_markers (fuel - 1) (steps + 1)
                    (value :: prints) calls last_inst next
              end
            end
        | IO (name, arg) when name = "esmeta.trace.enter" ->
            let fn : string = Obj.obj arg in
            begin match continue_safely continue (Obj.repr ()) with
            | Result.Error why -> Crashed (why, steps)
            | Result.Ok next ->
              run trace_markers (fuel - 1) (steps + 1)
                prints (fn :: calls) last_inst next
            end
        | IO (name, arg) when name = "esmeta.trace.exit" ->
            let fn : string = Obj.obj arg in
            begin match continue_safely continue (Obj.repr ()) with
            | Result.Error why -> Crashed (why, steps)
            | Result.Ok next ->
              run trace_markers (fuel - 1) (steps + 1)
                prints (pop_call fn calls) last_inst next
            end
        | IO (name, _) -> Crashed ("unhandled IO " ^ name, steps)
        | Take -> Unsupported_effect ("Take/UB", calls, last_inst, steps)
        | Choose -> Unsupported_effect ("Choose", calls, last_inst, steps)
        end

let diagnostic_note =
  "Exec diagnostic is validation-only; use the ITree call path and marker"

let rec take count values =
  if count = 0 then []
  else match values with
    | [] -> []
    | value :: rest -> value :: take (count - 1) rest

let call_path calls =
  match take 12 calls with
  | [] -> "<outside function>"
  | names -> Stdlib.String.concat " <- " names

let value_tag = function
  | VMath _ -> "VMath"
  | VBool _ -> "VBool"
  | VStr _ -> "VStr"
  | VUndef -> "VUndef"
  | VNull -> "VNull"
  | VEnum _ -> "VEnum"
  | VAddr _ -> "VAddr"
  | VClo _ -> "VClo"
  | VCont _ -> "VCont"
  | VAst _ -> "VAst"
  | VNumber _ -> "VNumber"
  | VBigInt _ -> "VBigInt"
  | VInfinity _ -> "VInfinity"
  | VCodeUnit _ -> "VCodeUnit"
  | VGrammarSymbol _ -> "VGrammarSymbol"

let evaluate_test ~fuel ~diagnostics ~trace_func test =
  match test.tt_expected_result with
  | expected when (match expected with VUndef -> false | _ -> true) ->
      0.0, Crash (0, "invalid-oracle-" ^ value_tag expected)
  | _ ->
      let started = Unix.gettimeofday () in
      let tree =
        match trace_func with
        | Some target -> test.tt_trace_func target
        | None -> test.tt_tree
      in
      let result =
        run
          (match trace_func with Some _ -> true | None -> false)
          fuel 0 [] [] None tree
      in
      let elapsed = Unix.gettimeofday () -. started in
      let verdict =
        match result with
        | Completed (actual_result, actual_prints, steps)
          when observable_outcome_eqb
            (actual_result, actual_prints)
            (test.tt_expected_result, test.tt_expected_prints) ->
            Pass steps
        | Completed (actual_result, _, steps)
          when not (val_eqb actual_result test.tt_expected_result) ->
            Result_differs
              (steps, value_tag actual_result,
               value_tag test.tt_expected_result)
        | Completed (_, actual_prints, steps)
          when not (vals_eqb actual_prints test.tt_expected_prints) ->
            Prints_differ
              (steps, List.length actual_prints,
               List.length test.tt_expected_prints)
        | Completed (_, _, steps) -> Ast_aliases_differ steps
        | Unsupported_effect (effect, calls, last_inst, steps) ->
            let trace = call_path calls in
            let inst =
              match last_inst with
              | Some marker -> marker
              | None -> "<no instruction marker>"
            in
            let why =
              if diagnostics
              then
                effect ^ "; " ^ trace ^ "; " ^ inst ^ "; "
                  ^ diagnostic_note
              else effect ^ "; " ^ trace ^ "; " ^ inst
            in
            Unsupported (steps, why)
        | Out_of_fuel steps -> Fuel_exhausted steps
        | Crashed (why, steps) -> Crash (steps, why)
      in
      elapsed, verdict

let category = function
  | Pass _ -> Matched
  | Result_differs _
  | Prints_differ _
  | Ast_aliases_differ _ -> Mismatched
  | Unsupported _ -> Unsupported_category
  | Fuel_exhausted _ -> Out_of_fuel_category
  | Crash _ -> Crashed_category

let reason = function
  | Pass _ -> None
  | Result_differs _ -> Some "result differs"
  | Prints_differ _ -> Some "prints differ"
  | Ast_aliases_differ _ -> Some "AST aliases differ"
  | Unsupported (_, why) -> Some why
  | Fuel_exhausted _ -> Some "out of fuel"
  | Crash (_, why) -> Some ("crash: " ^ why)

let format_verdict ~index ~name ~elapsed = function
  | Pass steps ->
      Printf.sprintf "%-6s %8.3fs %10d  %-18s %s"
        (Printf.sprintf "T%03d" index) elapsed steps "PASS" name
  | Result_differs (steps, actual, expected) ->
      Printf.sprintf "%-6s %8.3fs %10d  RESULT-DIFFERS     %s (%s vs %s)"
        (Printf.sprintf "T%03d" index) elapsed steps name actual expected
  | Prints_differ (steps, actual, expected) ->
      Printf.sprintf "%-6s %8.3fs %10d  PRINTS-DIFFER      %s (%d vs %d)"
        (Printf.sprintf "T%03d" index) elapsed steps name actual expected
  | Ast_aliases_differ steps ->
      Printf.sprintf "%-6s %8.3fs %10d  AST-ALIASES-DIFFER %s"
        (Printf.sprintf "T%03d" index) elapsed steps name
  | Unsupported (steps, why) ->
      Printf.sprintf "%-6s %8.3fs %10d  UNSUPPORTED-EFFECT  %-42s %s"
        (Printf.sprintf "T%03d" index) elapsed steps why name
  | Fuel_exhausted steps ->
      Printf.sprintf "%-6s %8.3fs %10d  OUT-OF-FUEL        %s"
        (Printf.sprintf "T%03d" index) elapsed steps name
  | Crash (steps, why) ->
      Printf.sprintf "%-6s %8.3fs %10d  CRASH %-12s %s"
        (Printf.sprintf "T%03d" index) elapsed steps why name

(* Standalone logger for the generated ECMA-262 IsCallable IR function.
 * This executable does not load or run a Test262 program. *)

open Fragment
open Domain
open Events
open ITreeDefinition
open SpecAlgorithmITree

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

let ensure_log_dir () =
  if not (Sys.file_exists "logs") then
    try Unix.mkdir "logs" 0o755
    with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let line_limit () =
  if Array.length Sys.argv < 2 then 120
  else
    match int_of_string_opt Sys.argv.(1) with
    | Some limit when limit > 0 -> limit
    | _ ->
        prerr_endline
          "usage: ./fvitree-spec-algorithm [positive-line-limit] [report-path]";
        exit 2

let report_path () =
  if Array.length Sys.argv >= 3 then Sys.argv.(2)
  else "logs/itree-spec-IsCallable.log"

let io_description name arg =
  if name = "esmeta.trace.enter" then
    let fn : string = Obj.obj arg in
    "Vis IO(esmeta.trace.enter, " ^ fn ^ ")"
  else if name = "esmeta.trace.exit" then
    let fn : string = Obj.obj arg in
    "Vis IO(esmeta.trace.exit, " ^ fn ^ ")"
  else if name = "esmeta.print" then
    let value : coq_val = Obj.obj arg in
    match value with
    | VEnum marker when has_prefix "$ESMetaFV.trace.inst:" marker ->
        "Vis IR(" ^ marker ^ ")"
    | _ -> "Vis IO(esmeta.print, " ^ value_tag value ^ ")"
  else "Vis IO(" ^ name ^ ", <opaque>)"

let dump_itree_shape emit_line limit tree =
  let emitted = ref 0 in
  let emit_node step description =
    if !emitted < limit then begin
      emit_line (Printf.sprintf "%10d  %s" step description);
      incr emitted
    end
  in
  let flush_taus step count =
    if count > 0 then
      emit_node (step - count) (Printf.sprintf "Tau x %d" count)
  in
  let rec loop step pending_taus tree =
    if !emitted >= limit then
      emit_line
        (Printf.sprintf
           "... truncated after %d shape lines at execution step %d ..."
           limit step)
    else if step >= 1_000_000 then begin
      flush_taus step pending_taus;
      emit_node step "Stopped(step limit 1000000)"
    end else
      match observe_safely tree with
      | Result.Error why ->
          flush_taus step pending_taus;
          emit_node step ("Crash(" ^ why ^ ")")
      | Result.Ok (RetF value) ->
          flush_taus step pending_taus;
          emit_node step ("Ret(" ^ value_tag value ^ ")")
      | Result.Ok (TauF next) ->
          loop (step + 1) (pending_taus + 1) next
      | Result.Ok (VisF (event, continue)) ->
          flush_taus step pending_taus;
          if !emitted >= limit then
            emit_line
              (Printf.sprintf
                 "... truncated after %d shape lines at execution step %d ..."
                 limit step)
          else
            begin match event with
            | IO (name, arg) ->
                emit_node step (io_description name arg);
                begin match continue_safely continue (Obj.repr ()) with
                | Result.Error why ->
                    emit_node (step + 1) ("Crash(" ^ why ^ ")")
                | Result.Ok next -> loop (step + 1) 0 next
                end
            | Take -> emit_node step "Vis Take (UB)"
            | Choose -> emit_node step "Vis Choose"
            end
  in
  loop 0 0 tree

let () =
  ensure_log_dir ();
  let limit = line_limit () in
  let path = report_path () in
  let report = open_out path in
  let emit format =
    Printf.ksprintf
      (fun line ->
        print_endline line;
        output_string report line;
        output_char report '\n';
        flush report)
      format
  in
  Fun.protect
    ~finally:(fun () -> close_out_noerr report)
    (fun () ->
      emit "ESMetaFV ECMA-262 algorithm ITree dump";
      emit "algorithm: IsCallable(argument)";
      emit "generated IR: validation/Spec.v::spec_funcs[name=IsCallable]";
      emit "sample input: argument = VUndef";
      emit "Test262 / RunJobs: not used";
      emit
        "pipeline: Spec.spec_funcs[IsCallable] -> denote_fbody -> spec call/state interpreters -> itree coreE";
      emit
        "legend: Tau = internal computation; Vis IR = generated IR instruction marker; Ret = algorithm result";
      emit "";
      dump_itree_shape (fun line -> emit "%s" line) limit is_callable_itree;
      emit "";
      emit "report: %s" path)

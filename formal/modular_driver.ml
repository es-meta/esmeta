(* Modular Test262 runner.  It shares the production verdict policy while
 * loading its trees from a generated shard. *)

open Fragment
open Events
open ITreeDefinition
open ITreeCore
open Itree_test_runtime

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
          "Vis IO(esmeta.print, " ^ marker ^ ")"
      | _ -> "Vis IO(esmeta.print, " ^ value_tag value ^ ")"
    else "Vis IO(" ^ name ^ ", <opaque>)"
  in
  let rec loop step pending_taus tree =
    if !emitted >= limit then
      emit_line
        (Printf.sprintf
           "... truncated after %d shape lines at execution step %d ..."
           limit step)
    else
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
            | Take -> emit_node step "Vis Take (UB / no Test262 handler)"
            | Choose -> emit_node step "Vis Choose (no Test262 handler)"
            end
  in
  loop 0 0 tree

let ensure_log_dir () =
  if not (Sys.file_exists "logs") then
    try Unix.mkdir "logs" 0o755
    with Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let parse_fuel () =
  if Array.length Sys.argv < 2 then 100_000_000
  else match int_of_string_opt Sys.argv.(1) with
    | Some fuel when fuel > 0 -> fuel
    | _ ->
        prerr_endline
          "usage: ./fvitree-modular [positive-fuel] [report-path] \
           [--diagnose] [--only INDEX] [--trace-func NAME] \
           [--dump-itree-shape LIMIT]";
        exit 2

let report_path () =
  if Array.length Sys.argv >= 3 then Sys.argv.(2)
  else "logs/itree-t262-modular-report.txt"

let diagnostics_enabled () =
  Array.exists (fun arg -> arg = "--diagnose") Sys.argv

let selected_index () =
  let rec find index =
    if index + 1 >= Array.length Sys.argv then None
    else if Sys.argv.(index) = "--only" then
      int_of_string_opt Sys.argv.(index + 1)
    else find (index + 1)
  in find 1

let traced_function () =
  let rec find index =
    if index + 1 >= Array.length Sys.argv then None
    else if Sys.argv.(index) = "--trace-func" then Some Sys.argv.(index + 1)
    else find (index + 1)
  in find 1

let itree_shape_limit () =
  let rec find index =
    if index + 1 >= Array.length Sys.argv then None
    else if Sys.argv.(index) = "--dump-itree-shape" then
      match int_of_string_opt Sys.argv.(index + 1) with
      | Some limit when limit > 0 -> Some limit
      | _ ->
          prerr_endline "--dump-itree-shape requires a positive limit";
          exit 2
    else find (index + 1)
  in find 1

let () =
  ensure_log_dir ();
  let fuel = parse_fuel () in
  let path = report_path () in
  let diagnostics = diagnostics_enabled () in
  let selected = selected_index () in
  let trace_func = traced_function () in
  let indexed_tests =
    Tests.tests
    |> List.mapi (fun index test -> index, test)
    |> List.filter (fun (index, _) ->
         match selected with None -> true | Some wanted -> index = wanted)
    |> List.map (fun (index, test) -> index, make_test_tree test)
  in
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
  (match itree_shape_limit () with
   | Some limit ->
       Fun.protect
         ~finally:(fun () -> close_out_noerr report)
         (fun () ->
           emit "ESMetaFV closed ITree shape dump";
           emit
             "pipeline: Test262 JS -> ESMeta AST/test tuple -> script_prog -> \
              exec_itree_trace_func -> call/state interpreters -> itree coreE";
           emit
             "legend: Tau = internal computation; Vis = observable core event; \
              Ret = final value";
           emit "";
           List.iter
             (fun (index, test) ->
               emit "program: T%03d  %s" index test.tt_name;
               (match trace_func with
                | Some target ->
                    emit "instruction markers enabled for IR function: %s"
                      target
                | None ->
                    emit
                      "instruction markers disabled (use --trace-func NAME)");
               emit "";
               let tree = match trace_func with
                 | Some target -> test.tt_trace_func target
                 | None -> test.tt_tree
               in
               dump_itree_shape (fun line -> emit "%s" line) limit tree)
             indexed_tests);
       exit 0
   | None -> ());
  let matched = ref 0
  and mismatched = ref 0
  and unsupported = ref 0
  and out_of_fuel = ref 0
  and crashed = ref 0 in
  let reasons : (string, int) Hashtbl.t = Hashtbl.create 16 in
  let bump reason =
    let count = match Hashtbl.find_opt reasons reason with
      | Some count -> count | None -> 0
    in Hashtbl.replace reasons reason (count + 1)
  in
  let record verdict =
    (match category verdict with
     | Matched -> incr matched
     | Mismatched -> incr mismatched
     | Unsupported_category -> incr unsupported
     | Out_of_fuel_category -> incr out_of_fuel
     | Crashed_category -> incr crashed);
    match reason verdict with
    | Some why -> bump why
    | None -> ()
  in
  Fun.protect
    ~finally:(fun () -> close_out_noerr report)
    (fun () ->
      emit "ITree Test262 differential run: %d test(s), fuel=%d, diagnostics=%b"
        (List.length indexed_tests) fuel diagnostics;
      (match trace_func with
       | Some target -> emit "instruction trace target: %s" target
       | None -> ());
      emit "";
      List.iter
        (fun (index, test) ->
          let elapsed, verdict =
            evaluate_test ~fuel ~diagnostics ~trace_func test
          in
          record verdict;
          emit "%s"
            (format_verdict
               ~index ~name:test.tt_name ~elapsed verdict))
        indexed_tests;
      let total =
        !matched + !mismatched + !unsupported + !out_of_fuel + !crashed
      in
      emit "";
      emit "matched       %d / %d" !matched total;
      emit "mismatched    %d / %d" !mismatched total;
      emit "unsupported   %d / %d" !unsupported total;
      emit "out of fuel   %d / %d" !out_of_fuel total;
      emit "crashed       %d / %d" !crashed total;
      if Hashtbl.length reasons > 0 then begin
        emit "";
        emit "reasons:";
        Hashtbl.to_seq reasons
        |> List.of_seq
        |> List.sort (fun (_, left) (_, right) -> Stdlib.compare right left)
        |> List.iter (fun (reason, count) ->
             emit "  %4d  %s" count reason)
      end;
      emit "";
      emit "report: %s" path;
      emit
        "ESMeta failures and values outside the model are excluded by FVInitState.")

(* Differential Test262 driver.

   Runs every payload twice — once through the generic denoter over the IR
   data, once through the generated direct ITree maps — and checks both
   against the observable ESMeta produced for that test.  The ITree walk
   mirrors direct_itree_tier_a_driver.ml.

   Three verdicts per test, kept separate on purpose:
     generic vs ESMeta  — a model defect
     direct  vs ESMeta  — a model or direct-backend defect
     direct  vs generic — a direct-backend defect specifically *)

open Events
open ITreeDefinition
open ITreeCore
open DirectTest262

type outcome =
  | Done of Fragment.coq_val * Fragment.coq_val list
  | Stuck of string
  | Out_of_fuel

let rec run fuel prints tree =
  if fuel = 0 then Out_of_fuel
  else
    match observe tree with
    | RetF value -> Done (value, List.rev prints)
    | TauF next -> run (fuel - 1) prints next
    | VisF (event, continue) ->
        begin match Obj.magic event with
        | Take -> Stuck "undefined behavior"
        | Choose -> Stuck "nondeterministic behavior"
        | IO (name, arg) when name = "esmeta.print" ->
            let value : Fragment.coq_val = Obj.obj arg in
            run (fuel - 1) (value :: prints) (continue (Obj.repr ()))
        | IO (name, _) -> Stuck ("unhandled IO: " ^ name)
        end

let describe = function
  | Done _ -> "ok"
  | Stuck why -> "stuck (" ^ why ^ ")"
  | Out_of_fuel -> "out of fuel"

let fuel =
  if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 50_000_000

let () =
  let generic = generic_t262_trees and direct = direct_t262_trees in
  if List.length generic <> List.length direct then begin
    prerr_endline "FAIL: tree list length mismatch";
    exit 1
  end;
  let failures = ref 0 in
  List.iter2
    (fun g d ->
      let name = g.tt_name in
      let expected = (g.tt_expected_result, g.tt_expected_prints) in
      (* Extracted ITrees are lazy and memoise once forced, so a tree is only
         ever timed on its first walk.  That is also how a full run uses it. *)
      let t0 = Unix.gettimeofday () in
      let gr = run fuel [] g.tt_tree in
      let t1 = Unix.gettimeofday () in
      let dr = run fuel [] d.tt_tree in
      let t2 = Unix.gettimeofday () in
      Printf.printf "TIME generic=%.3fs direct=%.3fs %s\n%!" (t1 -. t0)
        (t2 -. t1) name;
      match gr, dr with
      | Done (gv, gp), Done (dv, dp) ->
          let generic_ok = observable_outcome_eqb (gv, gp) expected in
          let direct_ok = observable_outcome_eqb (dv, dp) expected in
          let agree = observable_outcome_eqb (dv, dp) (gv, gp) in
          if generic_ok && direct_ok && agree then
            Printf.printf "PASS %s\n%!" name
          else begin
            incr failures;
            Printf.printf
              "FAIL %s (generic-vs-esmeta=%b direct-vs-esmeta=%b direct-vs-generic=%b)\n%!"
              name generic_ok direct_ok agree
          end
      | _ ->
          incr failures;
          Printf.printf "FAIL %s (generic=%s direct=%s)\n%!" name
            (describe gr) (describe dr))
    generic direct;
  Printf.printf "\n%d test(s), %d failure(s)\n" (List.length generic) !failures;
  if !failures > 0 then exit 1

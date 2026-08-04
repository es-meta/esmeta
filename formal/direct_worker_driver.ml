(* Direct-backend Test262 runner over decoded payloads.
 *
 * Nothing here is baked in at build time: every payload named on the command
 * line is decoded at run time and handed to the extracted
 * direct_make_test_tree, so running more tests never needs another build.
 *
 * Each payload carries the observable ESMeta produced for its test, so a
 * verdict needs no second backend: observable_outcome_eqb compares the direct
 * run against that oracle.  Agreement between the two backends is established
 * separately by direct_t262_driver.ml. *)

open Events
open ITreeDefinition
open ITreeCore
open DirectITreeCore

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

let test_name (input : Payload_codec.test_input) =
  let ((((name, _), _), _), _) = input in
  name

let fuel = try int_of_string (Sys.getenv "ITREE_FUEL") with _ -> 100_000_000

let () =
  let paths = List.tl (Array.to_list Sys.argv) in
  if paths = [] then begin
    prerr_endline "usage: direct-worker PAYLOAD.fvt ...";
    exit 2
  end;
  let matched = ref 0 and mismatched = ref 0 in
  let stuck = ref 0 and fuelled_out = ref 0 and failed = ref 0 in
  let total = ref 0.0 in
  List.iter
    (fun path ->
      match Payload_codec.load path with
      | exception Payload_codec.Decode_error message ->
          incr failed;
          Printf.printf "DECODE-ERROR %s (%s)\n%!" path message
      | payload ->
          let input = payload.Payload_codec.test_input in
          let name = test_name input in
          let (_, expected) = input in
          let tree = (direct_make_test_tree input).tt_tree in
          let start = Unix.gettimeofday () in
          let result = run fuel [] tree in
          let elapsed = Unix.gettimeofday () -. start in
          total := !total +. elapsed;
          (match result with
           | Done (value, prints) ->
               if observable_outcome_eqb (value, prints) expected then begin
                 incr matched;
                 Printf.printf "PASS %.3fs %s\n%!" elapsed name
               end else begin
                 incr mismatched;
                 Printf.printf "MISMATCH %.3fs %s\n%!" elapsed name
               end
           | Stuck why ->
               incr stuck;
               Printf.printf "STUCK %.3fs %s (%s)\n%!" elapsed name why
           | Out_of_fuel ->
               incr fuelled_out;
               Printf.printf "FUEL %.3fs %s\n%!" elapsed name))
    paths;
  let count = List.length paths in
  Printf.printf
    "\n%d payload(s): matched %d, mismatched %d, stuck %d, out of fuel %d, \
     undecodable %d\n"
    count !matched !mismatched !stuck !fuelled_out !failed;
  Printf.printf "total %.3fs, mean %.3fs/test\n" !total
    (!total /. float_of_int (max 1 count));
  if !mismatched > 0 || !failed > 0 then exit 1

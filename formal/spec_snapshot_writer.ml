(* Serialize the extracted, Test262-independent specification data once so a
   native worker does not need to compile the multi-megabyte Spec.ml module. *)

(* Rocq [Require Export] does not transfer declaration ownership during
   extraction.  These immutable values remain in their defining facade
   modules even though [Spec] imports them to build [script_prog]. *)
open SpecFuncs
open SpecGlobals
open SpecHeap

let snapshot_magic = "ESMETA_FV_SPEC_SNAPSHOT_V1"

let () =
  if Array.length Sys.argv <> 2 then begin
    Printf.eprintf "usage: %s OUTPUT\n" Sys.argv.(0);
    exit 2
  end;
  let output = open_out_bin Sys.argv.(1) in
  Fun.protect
    ~finally:(fun () -> close_out output)
    (fun () ->
      Marshal.to_channel output
        (snapshot_magic, spec_funcs, base_globals, init_heap) []);
  Printf.printf "wrote %s\n%!" Sys.argv.(1)

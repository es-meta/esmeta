open Direct_itree_tier_a

type run_result =
  | Done of tier_a_observation * val0 list
  | Stuck of string
  | Out_of_fuel

let rec run fuel prints tree =
  if fuel = 0 then Out_of_fuel
  else
    match observe tree with
    | RetF value -> Done (value, List.rev prints)
    | TauF next -> run (fuel - 1) prints next
    | VisF (event, continue) ->
        begin match event with
        | Take -> Stuck "undefined behavior"
        | Choose -> Stuck "nondeterministic behavior"
        | IO (name, arg) when name = "esmeta.print" ->
            let value : val0 = Obj.obj arg in
            run (fuel - 1) (value :: prints) (continue (Obj.repr ()))
        | IO (name, _) -> Stuck ("unhandled IO: " ^ name)
        end

let math expected = function
  | VMath actual -> Big_int_Z.eq_big_int actual (Big_int_Z.big_int_of_int expected)
  | _ -> false

let expected_observation = function
  | { tier_result = Some result;
      tier_result_global = Some global_result;
      tier_box_global = Some (VAddr address);
      tier_box_object = Some (ORecord (name, [(field, field_value)]));
      tier_box_record_order = Some false;
      tier_alloc_counter = Some alloc;
      tier_ast_alloc_counter = Some ast_alloc }
    when math 7 result && math 7 global_result && address = 0 &&
         name = "Box" && field = "value" && math 7 field_value &&
         alloc = 1 && ast_alloc = 0 -> true
  | _ -> false

let () =
  match run 1_000_000 [] tier_a_generic_tree,
        run 1_000_000 [] tier_a_direct_tree with
  | Done (generic_observation, [generic_print]),
    Done (direct_observation, [direct_print])
      when generic_observation = direct_observation &&
           generic_print = direct_print &&
           math 7 generic_print && expected_observation generic_observation ->
      print_endline
        "PASS: Tier-A generic/direct result, trace, and diagnostic projection match"
  | Done _, Done _ ->
      prerr_endline "FAIL: Tier-A generic/direct observation mismatch";
      exit 1
  | Stuck why, _ | _, Stuck why ->
      prerr_endline ("FAIL: Tier-A stuck: " ^ why);
      exit 1
  | Out_of_fuel, _ | _, Out_of_fuel ->
      prerr_endline "FAIL: Tier-A out of fuel";
      exit 1

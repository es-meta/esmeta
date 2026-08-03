open Itree_smoke

type outcome =
  | Done of val0 * val0 list
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

let is_math expected = function
  | VMath actual -> Big_int_Z.eq_big_int actual (Big_int_Z.big_int_of_int expected)
  | _ -> false

let is_number expected = function
  | VNumber actual -> Float64.to_float actual = float_of_int expected
  | _ -> false

let failures = ref 0
let checks = ref 0

let pass label =
  incr checks;
  Printf.printf "PASS: %s\n" label

let fail label why =
  incr checks;
  incr failures;
  Printf.eprintf "FAIL: %s (%s)\n" label why

let expect_undefined label tree =
  match run 1_000_000 [] tree with
  | Done (VUndef, []) -> pass label
  | Done _ -> fail label "unexpected result or print trace"
  | Stuck why -> fail label why
  | Out_of_fuel -> fail label "out of fuel"

let expect_undefined_behavior label tree =
  match run 1_000_000 [] tree with
  | Stuck "undefined behavior" -> pass label
  | Stuck why -> fail label ("unexpected effect: " ^ why)
  | Done _ -> fail label "unexpected successful result"
  | Out_of_fuel -> fail label "out of fuel"

let expect_math label expected tree =
  match run 1_000_000 [] tree with
  | Done (actual, []) when is_math expected actual -> pass label
  | Done _ -> fail label "unexpected result or print trace"
  | Stuck why -> fail label why
  | Out_of_fuel -> fail label "out of fuel"

let expect_number label expected tree =
  match run 1_000_000 [] tree with
  | Done (actual, []) when is_number expected actual -> pass label
  | Done _ -> fail label "unexpected result or print trace"
  | Stuck why -> fail label why
  | Out_of_fuel -> fail label "out of fuel"

let () =
  expect_undefined "ITree returned undefined" smoke_tree;
  expect_number
    "ITree used the exact typed Number/Math composite cache"
    3 number_math_smoke_tree;
  expect_undefined_behavior
    "ITree rejects a missing Number/Math composite cache entry"
    number_math_missing_tree;
  expect_undefined_behavior
    "ITree rejects an ill-typed Number/Math composite result"
    number_math_wrong_type_tree;
  expect_math
    "ITree exposes the main function return as RESULT"
    7 main_return_tree;
  expect_undefined
    "ITree lets captured bindings override same-named parameters"
    captured_param_tree;
  (match run 1_000_000 [] print_tree with
   | Done (VUndef, [one; two]) when is_math 1 one && is_math 2 two ->
       pass "ITree preserved print order"
   | Done _ -> fail "print order" "unexpected result or print trace"
   | Stuck why -> fail "print order" why
   | Out_of_fuel -> fail "print order" "out of fuel");
  (match run 1_000_000 [] cont_tree with
   | Done (VUndef, [answer]) when is_math 42 answer ->
       pass "ITree restored continuation frame"
   | Done _ -> fail "continuation frame" "unexpected result or print trace"
   | Stuck why -> fail "continuation frame" why
   | Out_of_fuel -> fail "continuation frame" "out of fuel");
  expect_undefined_behavior
    "ITree rejects a continuation after its mutable caller resumed"
    stale_cont_tree;
  (match run 1_000_000 [] poison_cont_tree with
   | Done (VUndef, [answer]) when is_math 2 answer ->
       pass
         "ITree reuses poison chains discarded by a nonlocal return"
   | Done _ ->
       fail "lazy poison continuation"
         "unexpected result or print trace"
   | Stuck why -> fail "lazy poison continuation" why
   | Out_of_fuel -> fail "lazy poison continuation" "out of fuel");
  (match run 1_000_000 [] repeat_cont_tree with
   | Done (VUndef, [answer]) when is_math 2 answer ->
       pass "ITree clones a live continuation for repeated invocation"
   | Done _ ->
       fail "repeated continuation invocation"
         "unexpected result or print trace"
   | Stuck why -> fail "repeated continuation invocation" why
   | Out_of_fuel -> fail "repeated continuation invocation" "out of fuel");
  expect_undefined
    "ITree resolved exported AST field name"
    named_ast_tree;
  expect_undefined
    "ITree preserves exported AST parent cursors"
    ast_parent_exists_tree;
  expect_undefined
    "ITree converted integral Number to Math exactly"
    number_to_math_tree;
  expect_undefined
    "ITree used typed host numeric, BigInt, and formatting queries"
    host_tostr_tree;
  expect_number
    "ITree used the deterministic Math host result before Number conversion"
    2 math_host_tree;
  expect_number
    "ITree used the typed host query to round Math 2^53 + 1"
    9007199254740992 math_to_number_host_tree;
  expect_undefined_behavior
    "ITree rejects a missing typed host query"
    host_missing_tree;
  expect_undefined_behavior
    "ITree rejects an ill-typed host result"
    host_wrong_type_tree;
  expect_undefined
    "ITree normalizes AST parse text and inherited parameters"
    host_parse_tree;
  expect_undefined
    "ITree prefers the cached initial Script AST over a host entry"
    initial_parse_priority_tree;
  expect_undefined_behavior
    "ITree keeps effective parse parameters in the host key"
    host_parse_wrong_params_tree;
  expect_undefined
    "ITree allocates an empty error list for parser rejection"
    host_parse_failure_tree;
  expect_undefined
    "ITree filters and stably sorts integer map keys"
    integer_keys_tree;
  expect_undefined
    "ITree literals use last-key-wins insertion"
    duplicate_literals_tree;
  expect_undefined
    "ITree exposes exporter-observed initial record key order"
    initial_record_keys_tree;
  expect_undefined_behavior
    "ITree rejects invented runtime record key order"
    runtime_record_keys_tree;
  expect_undefined
    "ITree exposes the unique key order of an empty runtime record"
    runtime_empty_record_keys_tree;
  expect_undefined
    "ITree exposes the unique key order of a singleton runtime record"
    runtime_singleton_record_keys_tree;
  expect_undefined
    "ITree exposes the unique key order of a copied singleton record"
    copied_singleton_record_keys_tree;
  expect_undefined_behavior
    "ITree rejects record key order after a new field"
    expanded_record_keys_tree;
  expect_undefined_behavior
    "ITree rejects record key order after assigning a new field"
    assigned_record_keys_tree;
  expect_undefined_behavior
    "ITree rejects record key order after a HashMap copy"
    copied_record_keys_tree;
  expect_undefined
    "ITree recognized structural record refinement"
    record_refinement_tree;
  expect_undefined_behavior
    "ITree does not guess a projected nested record refinement"
    record_refinement_addr_tree;
  expect_undefined
    "ITree distinguished completion record subtypes"
    completion_discriminant_tree;
  expect_undefined_behavior
    "ITree conservatively rejects child-bearing runtime AST aliasing"
    runtime_syn_tree;
  expect_undefined
    "ITree gives repeated runtime leaf ASTs fresh identities"
    runtime_leaf_fresh_tree;
  expect_undefined
    "ITree recognized exact NumberInt values"
    number_int_refinement_tree;
  expect_undefined
    "ITree enforced NumberInt[0+] sign refinement"
    number_nonneg_int_tree;
  expect_undefined
    "ITree recognized structural FunctionObject refinement"
    function_object_refinement_tree;
  Printf.printf "smoke summary: %d checks, %d failed\n" !checks !failures;
  if !failures <> 0 then exit 1

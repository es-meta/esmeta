(** Temporary small extraction smoke test for the executable ITree path. *)

From Stdlib Require Import Extraction Floats.
From Stdlib Require Import ExtrOcamlBasic ExtrOcamlNatInt ExtrOcamlZBigInt.
From Stdlib Require Import ExtrOcamlNativeString ExtrOCamlFloats ExtrOCamlInt63.
From CRIS Require Import ExtrOcamlCRIS.

From ESMetaFV Require Import Fragment ITreeExec Programs.

Import ListNotations.

(** [ExtrOcamlNativeString] names the OCaml standard-library module
    [String].  This extraction also contains stdpp's Rocq module named
    [String], so unqualified references such as [String.get] become
    accidentally self-referential inside the generated module.  Keep the
    native representation, but qualify every runtime operation. *)
Extract Inductive string => "string"
  [ """"""
    "(fun (c, s) -> Stdlib.String.make 1 c ^ s)" ]
  "(fun f0 f1 s ->
      let l = Stdlib.String.length s in
      if l = 0 then f0 ()
      else f1 (Stdlib.String.get s 0) (Stdlib.String.sub s 1 (l - 1)))".

Extract Inlined Constant String.string_dec => "(=)".
Extract Inlined Constant String.eqb => "(=)".
Extract Inlined Constant String.append => "(^)".
Extract Inlined Constant String.concat => "Stdlib.String.concat".
Extract Inlined Constant String.prefix =>
  "(fun s1 s2 ->
      let l1 = Stdlib.String.length s1
      and l2 = Stdlib.String.length s2 in
      l1 <= l2 && Stdlib.String.sub s2 0 l1 = s1)".
Extract Inlined Constant String.string_of_list_ascii =>
  "(fun l ->
      let a = Array.of_list l in
      Stdlib.String.init (Array.length a) (fun i -> a.(i)))".
Extract Inlined Constant String.list_ascii_of_string =>
  "(fun s ->
      List.init (Stdlib.String.length s) (fun i -> Stdlib.String.get s i))".
Extract Inlined Constant String.string_of_list_byte =>
  "(fun l ->
      let a = Array.of_list l in
      Stdlib.String.init (Array.length a) (fun i -> a.(i)))".
Extract Inlined Constant String.list_byte_of_string =>
  "(fun s ->
      List.init (Stdlib.String.length s) (fun i -> Stdlib.String.get s i))".

Definition number_math_smoke_expr : expr :=
  EConvert CToNumber
    (EBinary BAdd
      (EConvert CToMath (ENumber (1.0000000000000000)%float))
      (EConvert CToMath (ENumber (2.0000000000000000)%float))).

Definition number_math_smoke_main : func :=
  mkFunc true "main" nil (IReturn number_math_smoke_expr).

Definition number_math_smoke_query : host_query :=
  HQNumberMathOp NMAdd
    (1.0000000000000000)%float (2.0000000000000000)%float.

Definition number_math_smoke_prog : prog :=
  mkProgFull [number_math_smoke_main] None None
    [mkHostCacheEntry number_math_smoke_query
      (VNumber (3.0000000000000000)%float)] nil nil.

Definition number_math_missing_prog : prog :=
  mkProgFull [number_math_smoke_main] None None nil nil nil.

Definition number_math_wrong_type_prog : prog :=
  mkProgFull [number_math_smoke_main] None None
    [mkHostCacheEntry number_math_smoke_query (VMath 3)] nil nil.

Definition smoke_tree := exec_itree "ESMetaFV-Smoke" sum_prog.
Definition number_math_smoke_tree :=
  exec_itree "ESMetaFV-NumberMathSmoke" number_math_smoke_prog.
Definition number_math_missing_tree :=
  exec_itree "ESMetaFV-NumberMathMissing" number_math_missing_prog.
Definition number_math_wrong_type_tree :=
  exec_itree "ESMetaFV-NumberMathWrongType" number_math_wrong_type_prog.
Definition main_return_tree :=
  exec_itree "ESMetaFV-MainReturnSmoke" main_return_prog.
Definition captured_param_tree :=
  exec_itree "ESMetaFV-CapturedParamSmoke" captured_param_prog.
Definition print_tree := exec_itree "ESMetaFV-PrintSmoke" print2_prog.
Definition cont_tree := exec_itree "ESMetaFV-ContSmoke" cont_prog.
Definition stale_cont_tree :=
  exec_itree "ESMetaFV-StaleContSmoke" stale_cont_prog.
Definition poison_cont_tree :=
  exec_itree "ESMetaFV-PoisonContSmoke" poison_cont_prog.
Definition repeat_cont_tree :=
  exec_itree "ESMetaFV-RepeatContSmoke" repeat_cont_prog.
Definition named_ast_tree :=
  exec_itree "ESMetaFV-NamedAstSmoke" named_ast_prog.
Definition ast_parent_exists_tree :=
  exec_itree "ESMetaFV-AstParentExistsSmoke" ast_parent_exists_prog.
Definition number_to_math_tree :=
  exec_itree "ESMetaFV-NumberToMathSmoke" number_to_math_prog.
Definition host_tostr_tree :=
  exec_itree "ESMetaFV-HostToStrSmoke" host_tostr_prog.
Definition math_host_tree :=
  exec_itree "ESMetaFV-MathHostSmoke" math_host_prog.
Definition math_to_number_host_tree :=
  exec_itree "ESMetaFV-MathToNumberHostSmoke" math_to_number_host_prog.
Definition host_missing_tree :=
  exec_itree "ESMetaFV-HostMissingSmoke" host_missing_prog.
Definition host_wrong_type_tree :=
  exec_itree "ESMetaFV-HostWrongTypeSmoke" host_wrong_type_prog.
Definition host_parse_tree :=
  exec_itree "ESMetaFV-HostParseSmoke" host_parse_prog.
Definition initial_parse_priority_tree :=
  exec_itree "ESMetaFV-InitialParsePrioritySmoke"
    initial_parse_priority_prog.
Definition host_parse_wrong_params_tree :=
  exec_itree "ESMetaFV-HostParseWrongParamsSmoke"
    host_parse_wrong_params_prog.
Definition host_parse_failure_tree :=
  exec_itree "ESMetaFV-HostParseFailureSmoke"
    host_parse_failure_prog.
Definition integer_keys_tree :=
  exec_itree "ESMetaFV-IntegerKeysSmoke" integer_keys_prog.
Definition duplicate_literals_tree :=
  exec_itree "ESMetaFV-DuplicateLiteralsSmoke" duplicate_literals_prog.
Definition initial_record_keys_tree :=
  exec_itree "ESMetaFV-InitialRecordKeysSmoke" initial_record_keys_prog.
Definition runtime_record_keys_tree :=
  exec_itree "ESMetaFV-RuntimeRecordKeysSmoke" runtime_record_keys_prog.
Definition runtime_empty_record_keys_tree :=
  exec_itree "ESMetaFV-RuntimeEmptyRecordKeysSmoke"
    runtime_empty_record_keys_prog.
Definition runtime_singleton_record_keys_tree :=
  exec_itree "ESMetaFV-RuntimeSingletonRecordKeysSmoke"
    runtime_singleton_record_keys_prog.
Definition copied_singleton_record_keys_tree :=
  exec_itree "ESMetaFV-CopiedSingletonRecordKeysSmoke"
    copied_singleton_record_keys_prog.
Definition expanded_record_keys_tree :=
  exec_itree "ESMetaFV-ExpandedRecordKeysSmoke" expanded_record_keys_prog.
Definition assigned_record_keys_tree :=
  exec_itree "ESMetaFV-AssignedRecordKeysSmoke" assigned_record_keys_prog.
Definition copied_record_keys_tree :=
  exec_itree "ESMetaFV-CopiedRecordKeysSmoke" copied_record_keys_prog.
Definition record_refinement_tree :=
  exec_itree "ESMetaFV-RecordRefinementSmoke" record_refinement_prog.
Definition record_refinement_addr_tree :=
  exec_itree "ESMetaFV-RecordRefinementAddrSmoke"
    record_refinement_addr_prog.
Definition completion_discriminant_tree :=
  exec_itree "ESMetaFV-CompletionDiscriminantSmoke"
    completion_discriminant_prog.
Definition runtime_syn_tree :=
  exec_itree "ESMetaFV-RuntimeSyntacticSmoke" runtime_syn_prog.
Definition runtime_leaf_fresh_tree :=
  exec_itree "ESMetaFV-RuntimeLeafFreshSmoke" runtime_leaf_fresh_prog.
Definition number_int_refinement_tree :=
  exec_itree "ESMetaFV-NumberIntRefinementSmoke"
    number_int_refinement_prog.
Definition number_nonneg_int_tree :=
  exec_itree "ESMetaFV-NumberNonNegIntSmoke" number_nonneg_int_prog.
Definition function_object_refinement_tree :=
  exec_itree "ESMetaFV-FunctionObjectRefinementSmoke"
    function_object_refinement_prog.

Extraction "itree_smoke.ml"
  smoke_tree
  number_math_smoke_tree number_math_missing_tree number_math_wrong_type_tree
  main_return_tree captured_param_tree print_tree cont_tree
  stale_cont_tree poison_cont_tree repeat_cont_tree
  named_ast_tree number_to_math_tree
  ast_parent_exists_tree
  host_tostr_tree math_host_tree math_to_number_host_tree
  host_missing_tree host_wrong_type_tree
  host_parse_tree initial_parse_priority_tree
  host_parse_wrong_params_tree host_parse_failure_tree
  integer_keys_tree duplicate_literals_tree
  initial_record_keys_tree runtime_record_keys_tree
  runtime_empty_record_keys_tree runtime_singleton_record_keys_tree
  copied_singleton_record_keys_tree
  expanded_record_keys_tree assigned_record_keys_tree copied_record_keys_tree
  record_refinement_tree record_refinement_addr_tree
  completion_discriminant_tree
  runtime_syn_tree runtime_leaf_fresh_tree
  number_int_refinement_tree number_nonneg_int_tree
  function_object_refinement_tree.

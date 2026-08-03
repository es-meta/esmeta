(** Test262-independent executable ITree wrapper shared by all shards. *)

From Stdlib Require Import String List.
From CRIS Require Import ExtrOcamlCRIS.

From ESMetaFV Require Import Fragment Domain ITreeExec TestEncoding.
From ESMetaFV Require Import Spec.

Import ListNotations.

Set Implicit Arguments.
Local Open Scope string_scope.

Definition TEST262_MODULE : String.string := "ESMetaFV-Test262".

(** The production verdict path uses the uninstrumented denotation.  Tracing
    is constructed separately and lazily through [tt_trace_func], so it cannot
    consume production fuel or change an out-of-fuel classification. *)
Definition production_exec := exec_itree.

Record test_tree : Type := mkTestTree {
  tt_name : String.string;
  tt_tree : itree coreE val;
  tt_expected_result : val;
  tt_expected_prints : list val;
  tt_trace_func : String.string -> itree coreE val;
}.

Definition make_test_tree
  (t : String.string * cstr * ast * list host_cache_entry *
       (val * list val)) : test_tree :=
  let '(name, src, a, hosts, expected) := t in
  let '(expected_result, expected_prints) := expected in
  let program := script_prog src a hosts in
  mkTestTree
    name
    (production_exec TEST262_MODULE program)
    expected_result
    expected_prints
    (exec_itree_trace_func TEST262_MODULE program).

Fixpoint vals_eqb (xs ys : list val) : bool :=
  match xs, ys with
  | nil, nil => true
  | x :: xt, y :: yt => andb (val_eqb x y) (vals_eqb xt yt)
  | _, _ => false
  end.

(** Differential verdicts compare AST values up to a bijection between the
    actual and expected reference identities.  A reference identity includes
    both the allocation origin and the cursor path; the focused AST payload is
    still compared structurally by [val_eqb]. *)
Definition ast_ref_key : Type := (ast_origin * list nat)%type.
Definition ast_ref_bijection : Type :=
  list (ast_ref_key * ast_ref_key).

Definition ast_ref_key_eqb (x y : ast_ref_key) : bool :=
  let '(origin1, path1) := x in
  let '(origin2, path2) := y in
  ast_ref_eqb origin1 path1 origin2 path2.

Fixpoint ast_ref_lookup_actual
  (actual : ast_ref_key) (refs : ast_ref_bijection)
  : option ast_ref_key :=
  match refs with
  | nil => None
  | (actual', expected') :: rest =>
      if ast_ref_key_eqb actual actual'
      then Some expected'
      else ast_ref_lookup_actual actual rest
  end.

Fixpoint ast_ref_lookup_expected
  (expected : ast_ref_key) (refs : ast_ref_bijection)
  : option ast_ref_key :=
  match refs with
  | nil => None
  | (actual', expected') :: rest =>
      if ast_ref_key_eqb expected expected'
      then Some actual'
      else ast_ref_lookup_expected expected rest
  end.

(** Scala captures are finite maps represented by association lists.  Lookup
    uses the last binding, matching construction through repeated map updates. *)
Fixpoint capture_lookup_last
  (name : string) (captured : list (string * val)) : option val :=
  match captured with
  | nil => None
  | (name', value) :: rest =>
      match capture_lookup_last name rest with
      | Some value' => Some value'
      | None => if String.eqb name name' then Some value else None
      end
  end.

Fixpoint capture_has_later
  (name : string) (captured : list (string * val)) : bool :=
  match captured with
  | nil => false
  | (name', _) :: rest =>
      orb (String.eqb name name') (capture_has_later name rest)
  end.

Fixpoint capture_domain_subset
  (actual expected : list (string * val)) : bool :=
  match actual with
  | nil => true
  | (name, _) :: rest =>
      if capture_has_later name rest
      then capture_domain_subset rest expected
      else
        match capture_lookup_last name expected with
        | Some _ => capture_domain_subset rest expected
        | None => false
        end
  end.

Definition capture_domains_eqb
  (actual expected : list (string * val)) : bool :=
  andb
    (capture_domain_subset actual expected)
    (capture_domain_subset expected actual).

Fixpoint observable_val_eqb
  (refs : ast_ref_bijection) (actual expected : val)
  {struct actual} : option ast_ref_bijection :=
  match actual, expected with
  | VAst actual_origin _ actual_path,
    VAst expected_origin _ expected_path =>
      if val_eqb actual expected then
        let actual_key := (actual_origin, actual_path) in
        let expected_key := (expected_origin, expected_path) in
        match ast_ref_lookup_actual actual_key refs,
              ast_ref_lookup_expected expected_key refs with
        | Some mapped_expected, Some mapped_actual =>
            if andb
                 (ast_ref_key_eqb mapped_expected expected_key)
                 (ast_ref_key_eqb mapped_actual actual_key)
            then Some refs
            else None
        | None, None => Some ((actual_key, expected_key) :: refs)
        | _, _ => None
        end
      else None
  | VClo actual_fn actual_captured, VClo expected_fn expected_captured =>
      if andb
           (String.eqb actual_fn expected_fn)
           (capture_domains_eqb actual_captured expected_captured) then
        (fix go
           (actual_env : list (string * val))
           (refs' : ast_ref_bijection) : option ast_ref_bijection :=
           match actual_env with
           | nil => Some refs'
           | (actual_name, actual_value) :: actual_rest =>
               if capture_has_later actual_name actual_rest then
                 go actual_rest refs'
               else
                 match capture_lookup_last actual_name expected_captured with
                 | Some expected_value =>
                     match observable_val_eqb
                             refs' actual_value expected_value with
                     | Some refs'' => go actual_rest refs''
                     | None => None
                     end
                 | None => None
                 end
           end) actual_captured refs
      else None
  | VCont actual_fn actual_captured actual_stack,
    VCont expected_fn expected_captured expected_stack =>
      if andb
           (String.eqb actual_fn expected_fn)
           (capture_domains_eqb actual_captured expected_captured) then
        match
          (fix go
             (actual_env : list (string * val))
             (refs' : ast_ref_bijection) : option ast_ref_bijection :=
             match actual_env with
             | nil => Some refs'
             | (actual_name, actual_value) :: actual_rest =>
                 if capture_has_later actual_name actual_rest then
                   go actual_rest refs'
                 else
                   match capture_lookup_last actual_name expected_captured with
                   | Some expected_value =>
                       match observable_val_eqb
                               refs' actual_value expected_value with
                       | Some refs'' => go actual_rest refs''
                       | None => None
                       end
                   | None => None
                   end
             end) actual_captured refs
        with
        | Some refs' =>
            match actual_stack, expected_stack with
            | None, None => Some refs'
            | Some actual_id, Some expected_id =>
                if Nat.eqb actual_id expected_id then Some refs' else None
            | _, _ => None
            end
        | None => None
        end
      else None
  | _, _ =>
      if val_eqb actual expected then Some refs else None
  end.

Fixpoint observable_vals_eqb
  (refs : ast_ref_bijection) (actual expected : list val)
  : option ast_ref_bijection :=
  match actual, expected with
  | nil, nil => Some refs
  | actual_value :: actual_rest, expected_value :: expected_rest =>
      match observable_val_eqb refs actual_value expected_value with
      | Some refs' =>
          observable_vals_eqb refs' actual_rest expected_rest
      | None => None
      end
  | _, _ => None
  end.

Definition observable_outcome_eqb
  (actual expected : val * list val) : bool :=
  let '(actual_result, actual_prints) := actual in
  let '(expected_result, expected_prints) := expected in
  match observable_val_eqb nil actual_result expected_result with
  | Some refs =>
      match observable_vals_eqb refs actual_prints expected_prints with
      | Some _ => true
      | None => false
      end
  | None => false
  end.

(** Native-string extraction setup for a standalone Tier-A tree. *)

From Stdlib Require Import Extraction Floats.
From Stdlib Require Import ExtrOcamlBasic ExtrOcamlNatInt ExtrOcamlZBigInt.
From Stdlib Require Import ExtrOcamlNativeString ExtrOCamlFloats ExtrOCamlInt63.
From CRIS Require Import ExtrOcamlCRIS.
From ESMetaFV Require Import DirectITreeTierA.

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


Extraction "direct_itree_tier_a_generic.ml" tier_a_generic_tree.

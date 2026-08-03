(** Shared OCaml extraction configuration for the modular ITree runner. *)

From Stdlib Require Import String ZArith List Extraction.
From Stdlib Require Import ExtrOcamlBasic ExtrOcamlNatInt ExtrOcamlZBigInt.
From Stdlib Require Import ExtrOcamlNativeString ExtrOCamlFloats ExtrOCamlInt63.
From Stdlib Require Import ExtrOCamlPString.
From CRIS Require Import ExtrOcamlCRIS.

From ESMetaFV Require Import TestEncoding.

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
      List.init (Stdlib.String.length s)
        (fun i -> Stdlib.String.get s i))".
Extract Inlined Constant String.string_of_list_byte =>
  "(fun l ->
      let a = Array.of_list l in
      Stdlib.String.init (Array.length a) (fun i -> a.(i)))".
Extract Inlined Constant String.list_byte_of_string =>
  "(fun s ->
      List.init (Stdlib.String.length s)
        (fun i -> Stdlib.String.get s i))".

Extract Constant utf16_hex =>
  "(fun source ->
      let bytes = Pstring.to_string source in
      let nibble ch =
        let code = Stdlib.Char.code ch in
        if 48 <= code && code <= 57 then code - 48
        else if 97 <= code && code <= 102 then code - 87
        else -1
      in
      let rec loop index acc =
        if index < 0 then acc
        else
          let a = nibble (Stdlib.String.get bytes index)
          and b = nibble (Stdlib.String.get bytes (index + 1))
          and c = nibble (Stdlib.String.get bytes (index + 2))
          and d = nibble (Stdlib.String.get bytes (index + 3)) in
          if a < 0 || b < 0 || c < 0 || d < 0 then []
          else
            let unit = (((a * 16 + b) * 16 + c) * 16 + d) in
            loop (index - 4) (Big_int_Z.big_int_of_int unit :: acc)
      in
      let length = Stdlib.String.length bytes in
      if length mod 4 <> 0 then [] else loop (length - 4) [])".

Extract Constant nat_decimal =>
  "(fun source -> Stdlib.int_of_string (Pstring.to_string source))".

Extract Inlined Constant PosDef.Pos.of_succ_nat =>
  "(fun n ->
      Big_int_Z.add_int_big_int 1 (Big_int_Z.big_int_of_int n))".

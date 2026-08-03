(** * ESMetaFV.TestEncoding — compact UTF-16 payloads for extracted tests

    Test262 AST nodes retain their source text.  Emitting every UTF-16 code
    unit as an explicit Rocq [list Z] makes generated test files and their
    extracted OCaml closure unnecessarily large.  The Scala exporter
    therefore writes four lowercase hexadecimal digits per code unit and
    leaves a call to [utf16_hex] in the generated term.

    This module is part of the test-data boundary, not the IR semantics.
    Decoding preserves every UTF-16 code unit, including unpaired
    surrogates. *)

From Stdlib Require Import String ZArith List PString.
From ESMetaFV Require Import Fragment.

Import ListNotations.

Local Open Scope string_scope.

Definition hex_nibble (a : PrimString.char63) : option Z :=
  let n := Uint63.to_Z a in
  if andb (Z.leb 48 n) (Z.leb n 57) then
    Some (n - 48)%Z
  else if andb (Z.leb 97 n) (Z.leb n 102) then
    Some (n - 87)%Z
  else None.

Fixpoint decode_utf16_hex_chars
  (cs : list PrimString.char63) : option cstr :=
  match cs with
  | nil => Some nil
  | a :: b :: c :: d :: rest =>
      match hex_nibble a, hex_nibble b, hex_nibble c, hex_nibble d with
      | Some ha, Some hb, Some hc, Some hd =>
          match decode_utf16_hex_chars rest with
          | Some tail =>
              Some (((((ha * 16 + hb) * 16 + hc) * 16 + hd)%Z) :: tail)
          | None => None
          end
      | _, _, _, _ => None
      end
  | _ => None
  end.

Definition decode_utf16_hex
  (s : PrimString.string) : option cstr :=
  decode_utf16_hex_chars (PrimStringAxioms.to_list s).

(** Generated input is always valid.  Invalid hand-written input maps to
    the empty source instead of adding an [option] wrapper to every AST
    source field; [decode_utf16_hex] remains available when validation of
    an external payload is required. *)
Definition utf16_hex (s : PrimString.string) : cstr :=
  match decode_utf16_hex s with
  | Some units => units
  | None => nil
  end.

(** Large generated naturals (heap addresses and AST production indices)
    must not be written as ordinary Rocq numerals at the extraction
    boundary.  [ExtrOcamlNatInt] represents [nat] as [int], but an ordinary
    numeral is still elaborated as a unary [S] chain before extraction.
    The initial heap then expands a few thousand addresses into millions of
    nested [Stdlib.Int.succ] calls, deep enough to overflow [ocamlopt].

    Keep the logical value as a total Rocq decoder while retaining the
    decimal payload as a primitive string for extraction.  Generated input
    is non-empty and decimal; malformed hand-written input maps to zero. *)
Definition decimal_digit (a : PrimString.char63) : option nat :=
  let z := Uint63.to_Z a in
  if andb (Z.leb 48 z) (Z.leb z 57) then
    Some (Z.to_nat (z - 48))
  else None.

Fixpoint decode_nat_decimal_chars
  (cs : list PrimString.char63) (acc : nat) : option nat :=
  match cs with
  | nil => Some acc
  | c :: rest =>
      match decimal_digit c with
      | Some digit => decode_nat_decimal_chars rest (10 * acc + digit)
      | None => None
      end
  end.

Definition decode_nat_decimal
  (s : PrimString.string) : option nat :=
  match PrimStringAxioms.to_list s with
  | nil => None
  | chars => decode_nat_decimal_chars chars 0
  end.

Definition nat_decimal (s : PrimString.string) : nat :=
  match decode_nat_decimal s with
  | Some n => n
  | None => 0
  end.

Example decode_utf16_hex_ascii :
  decode_utf16_hex "007600610072"%pstring =
    Some [118%Z; 97%Z; 114%Z].
Proof. vm_compute. reflexivity. Qed.

Example decode_utf16_hex_surrogate_pair :
  decode_utf16_hex "d83dde00"%pstring = Some [55357%Z; 56832%Z].
Proof. vm_compute. reflexivity. Qed.

Example decode_utf16_hex_rejects_partial_unit :
  decode_utf16_hex "006"%pstring = None.
Proof. vm_compute. reflexivity. Qed.

Example decode_nat_decimal_address :
  decode_nat_decimal "2575"%pstring = Some 2575.
Proof. vm_compute. reflexivity. Qed.

Example decode_nat_decimal_rejects_non_digit :
  decode_nat_decimal "12a"%pstring = None.
Proof. vm_compute. reflexivity. Qed.

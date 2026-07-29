(** * ESMetaFV.TyModel — record subtyping, GENERATED — DO NOT EDIT
  *
  * Regenerate with:
  *   sbt "run dump-debugger" && sbt "runMain esmeta.fv.FVTyModel"
  *
  * Exported from ESMeta's own type model (esmeta.ty.TyModel.parentOf), so
  * the model's [ETypeCheck] agrees with ESMeta's [ty.contains] on record
  * type names rather than guessing the hierarchy.  Pinned to ESMeta 0.7.3
  * with ecma262 @ 84b38ad8.
  *)

From Stdlib Require Import String.
Local Open Scope string_scope.

(** Immediate parent of a record type name, if any. *)
Definition record_parent (t : string) : option string :=
  match t with
  | "AbruptCompletion" => Some "CompletionRecord"
  | "ArgumentsExoticObject" => Some "ExoticObject"
  | "Array" => Some "ExoticObject"
  | "ArrayBuffer" => Some "Object"
  | "ArrayIteratorInstance" => Some "OrdinaryObject"
  | "AsyncFromSyncIterator" => Some "OrdinaryObject"
  | "AsyncGenerator" => Some "OrdinaryObject"
  | "BigInt64Array" => Some "TypedArray"
  | "BigIntObject" => Some "OrdinaryObject"
  | "BigUint64Array" => Some "TypedArray"
  | "BooleanObject" => Some "OrdinaryObject"
  | "BoundFunctionExoticObject" => Some "ExoticObject"
  | "BreakCompletion" => Some "AbruptCompletion"
  | "BuiltinFunctionObject" => Some "OrdinaryObject"
  | "Constructor" => Some "FunctionObject"
  | "ContinueCompletion" => Some "AbruptCompletion"
  | "CyclicModuleRecord" => Some "ModuleRecord"
  | "DataView" => Some "OrdinaryObject"
  | "Date" => Some "OrdinaryObject"
  | "DeclarativeEnvironmentRecord" => Some "EnvironmentRecord"
  | "ECMAScriptCodeExecutionContext" => Some "ExecutionContext"
  | "ECMAScriptFunctionObject" => Some "OrdinaryObject"
  | "ErrorObject" => Some "OrdinaryObject"
  | "ExoticObject" => Some "Object"
  | "FinalizationRegistry" => Some "OrdinaryObject"
  | "Float16Array" => Some "TypedArray"
  | "Float32Array" => Some "TypedArray"
  | "Float64Array" => Some "TypedArray"
  | "ForInIterator" => Some "OrdinaryObject"
  | "FunctionEnvironmentRecord" => Some "DeclarativeEnvironmentRecord"
  | "FunctionObject" => Some "Object"
  | "Generator" => Some "OrdinaryObject"
  | "GeneratorExecutionContext" => Some "ExecutionContext"
  | "GlobalEnvironmentRecord" => Some "EnvironmentRecord"
  | "IdentifierReferenceRecord" => Some "ReferenceRecord"
  | "ImmutableBinding" => Some "Binding"
  | "ImmutablePrototypeExoticObject" => Some "ExoticObject"
  | "Int16Array" => Some "TypedArray"
  | "Int32Array" => Some "TypedArray"
  | "Int8Array" => Some "TypedArray"
  | "Iterator" => Some "Object"
  | "Map" => Some "OrdinaryObject"
  | "ModuleEnvironmentRecord" => Some "DeclarativeEnvironmentRecord"
  | "ModuleNamespaceExoticObject" => Some "ExoticObject"
  | "MutableBinding" => Some "Binding"
  | "NormalCompletion" => Some "CompletionRecord"
  | "NumberObject" => Some "OrdinaryObject"
  | "ObjectEnvironmentRecord" => Some "EnvironmentRecord"
  | "OrdinaryObject" => Some "Object"
  | "PendingPromise" => Some "Promise"
  | "PrivateReferenceRecord" => Some "ReferenceRecord"
  | "Promise" => Some "OrdinaryObject"
  | "PromiseAllResolveElementFunction" => Some "BuiltinFunctionObject"
  | "PropertyReferenceRecord" => Some "ReferenceRecord"
  | "ProxyExoticObject" => Some "ExoticObject"
  | "RegExp" => Some "OrdinaryObject"
  | "ReturnCompletion" => Some "AbruptCompletion"
  | "Set" => Some "OrdinaryObject"
  | "SettledPromise" => Some "Promise"
  | "SharedArrayBuffer" => Some "Object"
  | "SourceTextModuleRecord" => Some "CyclicModuleRecord"
  | "StringExoticObject" => Some "ExoticObject"
  | "SuperReferenceRecord" => Some "ReferenceRecord"
  | "SymbolObject" => Some "OrdinaryObject"
  | "ThrowCompletion" => Some "AbruptCompletion"
  | "TypedArray" => Some "ExoticObject"
  | "Uint16Array" => Some "TypedArray"
  | "Uint32Array" => Some "TypedArray"
  | "Uint8Array" => Some "TypedArray"
  | "Uint8ClampedArray" => Some "TypedArray"
  | "UnresolvableReferenceRecord" => Some "ReferenceRecord"
  | "WeakMap" => Some "OrdinaryObject"
  | "WeakRef" => Some "OrdinaryObject"
  | "WeakSet" => Some "OrdinaryObject"
  | _ => None
  end.

(** [record_subtype d a] holds when record type [d] is [a] or descends
    from it.  The fuel bound is the number of declarations, which exceeds
    the depth of any chain in the exported model. *)
Fixpoint record_subtype_fuel (n : nat) (d a : string) : bool :=
  if String.eqb d a then true else
  match n with
  | O => false
  | S n' =>
      match record_parent d with
      | Some p => record_subtype_fuel n' p a
      | None => false
      end
  end.

Definition record_subtype (d a : string) : bool :=
  record_subtype_fuel 113 d a.

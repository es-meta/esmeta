# ESMeta IR to Rocq ITree

The active `rocq` command translates standalone ESMeta IR programs with a
shallow embedding. Control flow, evaluation order, returns, and local-variable
updates become ITree combinators directly. Primitive operations, heap effects,
function calls, SDO calls, and printing remain typed ITree events for a later
handler; the generated Rocq does not contain an embedded IR AST or call a
generic IR denoter.

Computation-valued combinators keep generated terms close to the IR tree.
`ir_unary`, `ir_binary`, variadic and conversion operations, calls, references,
assignments, assertions, and returns accept computations directly.  Their
shared evaluators preserve the IR's left-to-right operand order; `ir_if` and
the logical combinators additionally preserve single evaluation and
short-circuiting.  Consequently, generated modules do not expose temporary
Rocq binders merely to pass an expression result to its parent.  `%0`, `%1`,
and other actual IR temporaries remain explicit `IR_Temp` environment entries.

Sequencing that discards a statement's result is written
`computation ;;; continuation`.  A return is carried through ordinary internal
binds, so generated functions do not need a separate block datatype or
block-sequencing layer.

From the repository root:

```sh
esmeta rocq                         # all files below tests/ir
esmeta rocq tests/ir/gcd.ir        # one file
esmeta rocq tests/ir/expr          # one directory
esmeta rocq tests/ir -rocqgen:out=/tmp/esmeta-rocq
esmeta rocq tests/ir/branch.ir -rocqgen:proof-obligations
```

The default output is `logs/rocq`. It contains `ITreeIR.v`, one `.v` module per
input program, `_CoqProject`, a `Makefile`, and this README. With `coq-itree`
installed, run `make` in that directory to check every generated module with
Rocq.

`-rocqgen:proof-obligations` turns each supported IR assertion into a
path-sensitive Rocq verification condition. The initial fragment handles
Boolean constants/connectives and comparisons/equalities between integer
mathematical literals; loops are rejected until loop invariants are available.
The generated module discharges these arithmetic conditions with `lia`, so an
unprovable assertion makes Rocq compilation fail.

These are source-level verification conditions. A complete safety theorem for
the resulting ITree will additionally require proving that each event handler
implements the primitive semantics assumed by the conditions; `ir_assert`
keeps its runtime Boolean check in the meantime.

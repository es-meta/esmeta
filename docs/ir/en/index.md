# IRes

[한국어](../ko/index.md)

IRes is the intermediate language that [ESMeta][esmeta] compiles the ECMAScript
specification down to. Writing it out here lets the specification be executed
directly, rather than through an interpreter written by hand alongside it.

A program is a table of functions. One of them is the main function, and
running a program opens a context on it and steps its body until nothing is
left to run.

<a id="program"></a>
<a id="func"></a>
```spectec
syntax program = (func)*

syntax func = 
   | {bool funcKind id (param)* type inst}
```

## Reading this specification

| Chapter | Contents |
| --- | --- |
| [Syntax](syntax.md) | Instructions, expressions, references, operators, and the value and type domains |
| [State](state.md) | The function table, the two variable environments, the call stack, and the heap |
| [Expressions](expressions.md) | How an expression is evaluated to a value, and how a reference finds its target |
| [Instructions](instructions.md) | How each instruction changes the state, one step at a time |
| [Running a program](running.md) | How a program is started, driven to the end, and stopped |
| [Abstract Operations](abstract-operations.md) | Every operation the rules call that the specification defines itself |
| [Transparent Builtins](builtins.md) | Every operation it declares but leaves to the standard library or the host |

## Notation

Two marks appear in the prose that are not part of the object language.

<a id="option_get"></a>
**!** marks an assertion, written the way ECMA-262 writes it. `Let x be ! f(y)`
means that `f(y)` yields an optional result, that the result is asserted to be
present, and that `x` is bound to what is inside it. No derivation exists in
which the result is absent, so a program that reaches one is
[stuck](running.md#stopping) rather than answered.

<a id="check_let"></a>
**Let!<sub>type</sub>** marks a binding that first checks the shape of what it
is binding. `Let!type x be y` means that `y` is checked against the type or
pattern the rule expects, and that `x` is bound to it only if the check passes.
As with **!**, a failed check has no derivation.

Neither mark is a step the interpreter performs. Both record a condition the
rule depends on.

## Two evaluation styles, on purpose

Expressions are evaluated **big-step**, so that one relation carries an
expression all the way to a value and handles subexpressions through premises
of that same relation. Instructions are evaluated **small-step**, so that one
relation takes a state together with the instructions left to run and produces
the next state and the next sequence.

The split follows what each has to express. An expression has no control flow
to suspend, so big-step costs nothing and reads directly. An instruction
sequence does have control flow to suspend, because a call has to set its
caller aside and resume it later, and that means the remaining instructions
must be part of the configuration rather than hidden inside a derivation.

That choice has one visible consequence, which the rest of the specification is
written around. Because expressions are evaluated big-step, there is no channel
by which one could yield a trap instead of a value, so an expression-level
failure leaves the program *stuck* rather than trapped. Only instruction-level
failures are labelled. See [how a program stops](running.md#stopping).

[esmeta]: https://github.com/es-meta/esmeta

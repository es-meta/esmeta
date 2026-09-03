# Running a program

[← Instructions](instructions.md) · [Abstract Operations →](abstract-operations.md)

A task hands the interpreter one relation and the values to invoke it with, so
the driver that repeats the [instruction step](instructions.md) has to live in
the specification rather than in the host language. That driver is the
reflexive-transitive closure of the step, and it carries the same
configuration, a state together with the sequence left to run.

<a id="Steps"></a>
`s ⊢ inst'''* ⟹ % ; %`:
Running `inst'''*` in state `s`

```spectec
rule Steps/step:
  s |- inst* ==> s_2 ; inst_2*
 -- Step: s |- inst* ==> s_1 ; inst_1*
 -- Steps: s_1 |- inst_1* ==> s_2 ; inst_2*
```

<a id="Steps/step"></a>
Running `inst*` in state `s` yields `inst_2*` in state `s_2`, provided:

1. [Stepping `inst*` in state `s`](instructions.md#Step) yields `inst_1*` in state `s_1`.
1. [Running `inst_1*` in state `s_1`](running.md#Steps) yields `inst_2*` in state `s_2`.

## <a id="stopping"></a>How a program stops

There are two terminals. Both are irreducible, and both report the sequence
they stopped on, so a caller can tell them apart.

An empty sequence with an empty stack is a program that ran to completion.

```spectec
rule Steps/done:
  s |- eps ==> s ; eps
 -- if s.STACK = eps
```

<a id="Steps/done"></a>
Running `inst*` in state `s` yields `·` in state `s`, provided:

1. `inst*` is an empty list.
1. `s.STACK` is equal to `·`.

A leading `ITrap` is a program that stopped for a stated reason.

```spectec
rule Steps/trap:
  s |- (ITrap trap :: inst*) ==> s ; (ITrap trap :: inst*)
```

<a id="Steps/trap"></a>
Running `inst''*` in state `s` yields `ITrap trap :: inst*` in state `s`, provided:

1. `inst''*` is a non-empty list.
1. Let `inst' :: inst*` be `inst''*`.
1. `inst'` matches pattern **`ITrap`** `_`.
1. Let `ITrap trap` be `inst'`.

Neither terminal has a step of its own, so the stepping rule cannot apply to
them. They are matched first all the same, because a rule that cannot apply is
only discovered by failing.

### Three ways to not finish

The specification distinguishes three outcomes that a single "error" would
conflate.

| Outcome | How it appears | Meaning |
| --- | --- | --- |
| Completion | `eps`, empty stack | The program ran to the end |
| Trap | leading `ITrap` | It stopped for a reason the specification states |
| Stuck | no rule applies | No rule was written for this configuration |

A trap is a *labelled* terminal. Because [expressions](expressions.md) are
evaluated big-step, with no channel by which one could yield a trap instead of
a value, only instruction-level failures can be labelled, and an
expression-level failure is stuck instead. The trap reasons are exactly those
instruction-level failures.

<a id="trap"></a>
```spectec
syntax trap = 
   | {AssertFail expr}
   | NoReturn
   | {NoCallable value}
   | {NoBool value}
```

Stuckness is not a defect to be smoothed over. It is how the specification says
"this is not modelled yet" out loud, rather than answering a question it cannot
actually answer. An unmodelled type in [`ETypeCheck`](expressions.md#type-checks)
reaches it the same way.

## The entry point

`$init_state` loads the function table and opens a context on the main
function, and `$init_insts` is the body it starts on. The main function's
parameters are ignored, because nothing supplies arguments.

### Initial State

`$init_state(program)`:
The initial state for `program`

```spectec
def $init_state(program) = $empty_state($init_context($main_func(program)))[FUNC = $funcs_of(program)]
```

### Initial Context

`$init_context(func)`:
An initial context on `func`

```spectec
def $init_context(func) = {FUNC func, LOCAL $empty_map<local, value>}
```

### Initial Sequence

`$init_insts(program)`:
The initial instruction sequence of `program`

```spectec
def $init_insts(program) = $body_of($main_func(program))
```

Running a program demands that the sequence be exhausted, so a program that
traps has no derivation at all. That is what makes an `assert` failure a *failed* task
rather than a task whose result happens to be a trap.

```spectec
rule Run:
   |- program ==> s
 -- Steps: $init_state(program) |- $init_insts(program) ==> s ; eps
```

<a id="Run"></a>
Running `program` yields `s`, provided:

1. [Running the initial instruction sequence of `program` in state the initial state for `program`](running.md#Steps) yields `inst*` in state `s`.
1. `inst*` is an empty list.

# State

[← Syntax](syntax.md) · [Expressions →](expressions.md)

The state holds the function table, the current context, the stack of suspended
callers, the global environment, the heap, the output stream, and the counter
that names freshly allocated AST nodes.

<a id="state"></a>
```spectec
syntax state = {FUNC map<id, func>, CTX context, STACK callctx*, GLOBAL map<global, value>, HEAP memory, OUT value*, ASTID nat}
```

Every operation that reads or writes one of these fields is defined in
[Abstract Operations](abstract-operations.md#state-operations), so no rule
elsewhere reaches into the record directly.

## The function table

`FUNC` is what ESMeta reaches the CFG for. `cfg.getFunc(fname)` resolves a name
to the function it names, and the CFG adds nothing else the semantics uses,
since its entry node is a compiled view of the body already in `func`. Holding
the program's functions in the state keeps the whole specification at the IR
level.

It is fixed once the program is loaded, so it has a builder
([`$funcs_of`](abstract-operations.md#funcs_of)) and a reader
([`$find_func`](abstract-operations.md#find_func)) but no binder, because
nothing during evaluation adds a function.

Exactly one function carries the main flag; both zero and several are errors,
and [`$main_func`](abstract-operations.md#main_func) is undefined in either
case, so such a program is stuck rather than silently given a starting point.

## Contexts and the call stack

A context is ESMeta's `Context` less its cursor. What is left to run is carried
by the [instruction rules](instructions.md) rather than stored here. With no
CFG, "where we are in the function" is just the sequence those rules are
holding, and only a *suspended* frame has to write it down.

<a id="context"></a>
<a id="callctx"></a>
```spectec
syntax context = {FUNC func, LOCAL map<local, value>}

syntax callctx = {CTX context, INST inst*, RETID local}
```

A `callctx` is therefore the caller's whole context, the cursor it resumes at,
and the local its result lands in.
[`$callee_context`](abstract-operations.md#callee_context) builds the context a
call enters, and [`$bind_params`](abstract-operations.md#bind_params) binds its
arguments.

## Variables

Locals live in the current context, globals in the state. Reading goes through
[`$find_var`](abstract-operations.md#find_var), which dispatches to
[`$find_local`](abstract-operations.md#find_local) or
[`$find_global`](abstract-operations.md#find_global); binding goes through
[`$bind_local`](abstract-operations.md#bind_local) and
[`$bind_global`](abstract-operations.md#bind_global) rather than through a
record update at the use site.

## The heap

The heap is a map from address to object plus a size counter, which is what
makes a fresh address derivable rather than guessed, so that
[`$alloc_heap`](abstract-operations.md#alloc_heap) can hand out the next one.

<a id="memory"></a>
<a id="addr"></a>
<a id="obj"></a>
```spectec
syntax memory = {MAP map<addr, obj>, SIZE nat}

syntax addr = 
   | {NamedAddr text}
   | {DynamicAddr int}

syntax obj = 
   | {RecordObj text map<text, value>}
   | {MapObj map<value, value>}
   | {ListObj value*}
   | {YetObj text text}
```

## AST identity

An AST built during evaluation needs an identity that structural equality
cannot supply, because two separately constructed nodes of the same shape are
distinct in ESMeta. Every node therefore gets a serial number. The counter is
this specification's own device, since ESMeta has no field for it.
[`$fresh_ast_id`](abstract-operations.md#fresh_ast_id) reads it and advances
it in one step.

## The output stream

`OUT` is the output stream, to which `IPrint` appends each value it evaluated,
in order. ESMeta's `IPrint` calls `println` and keeps nothing, but a side
effect is not writable here, because the interpreter backtracks and a print
performed inside a rule that later fails cannot be taken back. Holding the
stream in the state makes
printing observable *and* undone by backtracking, at the cost of one field.

How a value is rendered stays a presentation concern, so `OUT` holds the values
themselves rather than text.

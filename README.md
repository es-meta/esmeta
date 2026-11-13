[![test](https://github.com/es-meta/esmeta/actions/workflows/ci.yml/badge.svg)](https://github.com/es-meta/esmeta/actions)
[![license](https://badgen.net/github/license/es-meta/esmeta)](https://github.com/es-meta/esmeta/blob/main/LICENSE.md)
[![release](https://badgen.net/github/release/es-meta/esmeta)](https://github.com/es-meta/esmeta/releases)

# ESMeta

**ESMeta** is an **E**CMAScript **S**pecification **Meta**language. It extracts a
mechanized specification from a given version of ECMAScript/JavaScript
specification ([ECMA-262](https://tc39.es/ecma262/)) and automatically generates
language-based tools: a JavaScript interpreter, a double debugger, a conformance
test synthesizer, and a specification type analyzer.

## Installation

ESMeta is written in Scala and requires
[JDK 21+](https://www.oracle.com/java/technologies/downloads/) and
[sbt](https://www.scala-sbt.org/).

```bash
git clone https://github.com/es-meta/esmeta.git
cd esmeta
git checkout v0.8.2  # optional: a released version (`main` has the latest changes)
```

Add the following to `~/.bashrc` (or `~/.zshrc`), where `<path to ESMeta>` is the
absolute path of the cloned repository:

```bash
export ESMETA_HOME="<path to ESMeta>"  # IMPORTANT!!!
export PATH="$ESMETA_HOME/bin:$PATH"   # for the `esmeta` executable
source $ESMETA_HOME/.completion        # for auto-completion
```

Then update the git submodules and build the binary:

```bash
git submodule update --init && sbt assembly && source .completion
```

Run `esmeta` to check the installation. It should print a welcome message:

```
$ esmeta
Welcome to ESMeta v0.8.2 - ECMAScript Specification Metalanguage.
Please type `esmeta help` to see the help message.
```

## Usage

```bash
esmeta <command> <option>* <filename>*
```

| Command          | Description                                                                     |
| ---------------- | ------------------------------------------------------------------------------- |
| `extract`        | extract a specification model from ECMA-262 (`ecma262/spec.html`)               |
| `compile`        | compile a specification to an IR program                                        |
| `build-cfg`      | build a control-flow graph (CFG) from an IR program                             |
| `parse` / `eval` | parse or evaluate an ECMAScript file                                            |
| `tycheck`        | perform a type analysis of ECMA-262                                             |
| `test262-test`   | run [Test262](https://github.com/tc39/test262) tests (default: `tests/test262`) |
| `fuzz`           | synthesize JavaScript programs using specification coverage                     |
| `inject`         | inject assertions to check the final state of an ECMAScript file                |
| `gen-poly`       | generate a polyfill library for built-ins from ECMA-262                         |

Use `esmeta help` or `esmeta help <command>` for the full list of commands,
global options, and command-specific options.

## Features

ESMeta extracts a mechanized specification from ECMA-262 and builds language
tools on top of it.

### Mechanized Specification and Interpreter

`esmeta build-cfg` runs the core pipeline:

1. `extract` — build a specification model (`esmeta.spec.Spec`) from `ecma262/spec.html`
2. `compile` — lower it into an IR program (`esmeta.ir.Program`)
3. `build-cfg` — build a control-flow graph (`esmeta.cfg.CFG`) from that program

By default, `extract` reads the version of ECMA-262 that the `ecma262` submodule
points to. Use the `-extract:target` option with any git tag, branch name, or
commit hash to target another version:

```bash
esmeta build-cfg -extract:target=origin/main  # a branch (the latest draft)
esmeta build-cfg -extract:target=es2026       # a tag
esmeta build-cfg -extract:target=828d35d      # a commit
```

The control-flow graph is an executable interpreter, so `parse` and `eval` run
JavaScript directly against the specification.

```bash
echo 'let x; x ??= class {}; print(typeof x);' > example.js
esmeta parse example.js         # print the parse tree
esmeta eval example.js -silent  # print only the output of `print`: "function"
esmeta eval example.js          # also print the final state (context and heap)
```

### ECMA Visualizer and Double Debugger

Two tools for exploring the mechanized specification interactively
([FSE 2025 Demo](https://doi.org/10.1145/3696630.3728579)).

> [!NOTE]
> A short [introduction video](https://youtu.be/4XMjJPNmuBM) for both tools is
> available.

<img width="1150" alt="ecma-visualizer" src="https://github.com/user-attachments/assets/733403f5-03cc-4465-a773-e57d46d35180" />

[**ECMA Visualizer**](https://chromewebstore.google.com/detail/nlfpedidieegejndiikebcgclhggaocd)
is a Chrome extension that displays information collected by ESMeta alongside the
ECMA-262 web documentation:

- Minimal JavaScript programs that reach a specific algorithm step or branch
- Conformance tests (from Test262) that pass through selected steps
- Filtering of displayed JS code by call path
- One-click debugging that resumes from the selected step

<img width="1150" alt="debugger" src="https://github.com/user-attachments/assets/6c5f29a3-6d8a-458d-a4ed-478bb00666d7">

The [**ECMAScript Double Debugger**](https://es-meta.github.io/playground/)
extends the interpreter to show how a JavaScript program runs according to
ECMA-262: step-by-step execution of ECMA-262 algorithms, line-by-line execution
of JavaScript code, breakpoints by abstract algorithm name, inspection of
internal states, and stepping backward to the provenance of a specification
record.

> [!TIP]
> Try the Double Debugger right now at
> **[es-meta.github.io/playground](https://es-meta.github.io/playground/)** — no
> installation required.

### Type Analysis

`esmeta tycheck` infers unknown types in the specification by analyzing the
extracted IR with condition-based type refinement
([ASE 2021](https://doi.org/10.1109/ASE51524.2021.9678781)) and reports the type
errors it finds, such as return type mismatches. Without an option it
analyzes the version of ECMA-262 that the `ecma262` submodule points to; use
`-extract:target` to check another one:

```bash
esmeta tycheck                            # the current submodule version
esmeta tycheck -extract:target=origin/main
```

> [!WARNING]
> The meta-level static analyzer (`analyze` command,
> [ESEC/FSE 2022](https://dl.acm.org/doi/10.1145/3540250.3549097)) is temporarily
> removed and will be re-introduced in a future version.

### Conformance Testing

`esmeta test262-test` runs the [Test262](https://github.com/tc39/test262)
conformance suite against the mechanized specification to check that Test262 and
ECMA-262 agree. Pass paths to restrict the run to specific files or directories:

```bash
esmeta test262-test tests/test262/test/language/expressions/addition
```

ESMeta can also synthesize new conformance tests. `esmeta fuzz` synthesizes
JavaScript programs using node/branch coverage of the mechanized specification
([ICSE 2021](https://doi.org/10.1109/ICSE43902.2021.00015)), or feature-sensitive
coverage with `-fuzz:k-fs=<int>` and `-fuzz:cp`
([PLDI 2023](https://doi.org/10.1145/3591240),
[TOSEM 2026](https://doi.org/10.1145/3808231)). `esmeta inject` then injects
assertions derived from the semantics described in ECMA-262, turning synthesized
programs into conformance tests.

```bash
esmeta fuzz -fuzz:duration=3600 -fuzz:out=out    # node/branch coverage, for an hour
esmeta fuzz -fuzz:k-fs=2 -fuzz:cp -fuzz:out=out  # feature-sensitive coverage
esmeta inject out/minimal/0.js -inject:defs -inject:out=0.test.js
```

### Polyfill Generation

`esmeta gen-poly` generates JavaScript polyfills for built-in functions directly
from their algorithms in ECMA-262
([ASE 2026](https://doi.org/10.1145/3832783.3837549)). With `-gen-poly:opt`, it
rewrites specification steps into optimized runtime operations. Use
`-gen-poly:target` to select built-ins; the output goes to
`logs/polyfill/minipoly` by default:

```bash
esmeta gen-poly -gen-poly:opt
esmeta gen-poly -gen-poly:target='Array.prototype.*'
```

## Publications

- **[ASE 2026] [MiniPoly: Automatic Extraction of Efficient JavaScript Polyfill from Language Specification](https://doi.org/10.1145/3832783.3837549)** 🏆 _ACM SIGSOFT Distinguished Paper Award_
- **[TOSEM 2026] [Selective Feature-Sensitive Coverage for Conformance Testing of Programming Languages](https://doi.org/10.1145/3808231)**
- **[FSE 2025 Demo] [JSSpecVis: A JavaScript Language Specification Visualization Tool](https://doi.org/10.1145/3696630.3728579)**
- **[CACM 2024] [JavaScript Language Design and Implementation in Tandem](https://doi.org/10.1145/3624723)**
- **[PLDI 2023] [Feature-Sensitive Coverage for Conformance Testing of Programming Language Implementations](https://doi.org/10.1145/3591240)**
- **[ESEC/FSE 2022] [Automatically Deriving JavaScript Static Analyzers from Specifications using Meta-Level Static Analysis](https://doi.org/10.1145/3540250.3549097)**
- **[ASE 2021] [JSTAR: JavaScript Specification Type Analyzer using Refinement](https://doi.org/10.1109/ASE51524.2021.9678781)**
- **[ICSE 2021] [JEST: N+1-version Differential Testing of Both JavaScript Engines and Specification](https://doi.org/10.1109/ICSE43902.2021.00015)** 🏆 _ACM SIGSOFT Distinguished Paper Award_
- **[ASE 2020] [JISET: JavaScript IR-based Semantics Extraction Toolchain](https://doi.org/10.1145/3324884.3416632)**

[![test](https://github.com/es-meta/esmeta/actions/workflows/ci.yml/badge.svg)](https://github.com/es-meta/esmeta/actions)
[![license](https://badgen.net/github/license/es-meta/esmeta)](https://github.com/es-meta/esmeta/blob/main/LICENSE.md)
[![release](https://badgen.net/github/release/es-meta/esmeta)](https://github.com/es-meta/esmeta/releases)
[![prs](https://badgen.net/github/prs/es-meta/esmeta)](https://github.com/es-meta/esmeta/pulls)
[![slack](https://badgen.net/badge/slack/esmeta/blue)](https://esmeta.slack.com/)
[![site](https://badgen.net/badge/site/jekyll/blue)](https://es-meta.github.io/)
[![doc](https://badgen.net/badge/doc/scaladoc/blue)](https://es-meta.github.io/esmeta)

# ESMeta
**ESMeta** is an **E**CMAScript **S**pecification **Meta**language. This
framework extracts a mechanized specification from a given version of
ECMAScript/JavaScript specification ([ECMA-262](https://tc39.es/ecma262/)) and
automatically generates language-based tools.

## Table of Contents

  * [Installation Guide](#installation-guide)
    + [Download ESMeta](#download-esmeta)
    + [Environment Setting](#environment-setting)
    + [Installation of ESMeta using `sbt`](#installation-of-esmeta-using--sbt-)
  * [Basic Commands](#basic-commands)
    + [Parsing and Executing ECMAScript files](#parsing-and-executing-ecmascript-files)
    + [Executing Test262 tests](#executing-test262-tests)
  * [Supported Features](#supported-features)
    + [Specification Exemplified with ECMA Visualizer](#specification-exemplified-with-ecma-visualizer)
    + [Interactive Execution with ECMAScript Double Debugger](#interactive-execution-with-ecmascript-double-debugger)
    + [Conformance Test Synthesizer from ECMA-262](#conformance-test-synthesizer-from-ecma-262)
    + [Type Analysis on ECMA-262](#type-analysis-on-ecma-262)
    + ~~[Meta-Level Static Analyzer for ECMAScript](#meta-level-static-analyzer-for-ecmascript)~~ (temporarily removed)
  * [Academic Achievement](#academic-achievement)
    + [Publications](#publications)
    + [PLDI 2022 Tutorial](#pldi-2022-tutorial)
    + [Communications of the ACM (CACM)](#communications-of-the-acm-cacm)


## Installation Guide

We explain how to install ESMeta with the necessary environment settings from
scratch. Our framework is developed in Scala, which works on JDK 17+. So before
installation, please install [JDK
17+](https://www.oracle.com/java/technologies/downloads/) and
[sbt](https://www.scala-sbt.org/), an interactive build tool for Scala.


### Download ESMeta
```bash
$ git clone https://github.com/es-meta/esmeta.git
```

### Environment Setting
Insert the following commands to `~/.bashrc` (or `~/.zshrc`):
```bash
# for ESMeta
export ESMETA_HOME="<path to ESMeta>" # IMPORTANT!!!
export PATH="$ESMETA_HOME/bin:$PATH" # for executables `esmeta` and etc.
source $ESMETA_HOME/.completion # for auto-completion
```
The `<path to ESMeta>` should be the absolute path of the ESMeta repository.


### Installation of ESMeta using `sbt`

Please type the following command to 1) update the git submodules, 2) generate
binary file `bin/esmeta`, and 3) apply the `.completion` for auto-completion.

```bash
$ cd esmeta && git submodule update --init && sbt assembly && source .completion
```

If you see the following message, ESMeta is successfully installed:
```bash
$ esmeta
# Welcome to ESMeta v0.6.3 - ECMAScript Specification Metalanguage.
# Please type `esmeta help` to see the help message.
```


## Basic Commands

You can run this framework with the following command:
```bash
$ esmeta <command> <option>* <filename>*
```
It supports the following commands:
- `help` shows help messages.
- `extract` extracts specification model from ECMA-262 (`ecma262/spec.html`).
- `compile` compiles a specification to an IR program.
- `build-cfg` builds a control-flow graph (CFG) from an IR program.
- `tycheck` performs a type analysis of ECMA-262.
- `parse` parses an ECMAScript file.
- `eval` evaluates an ECMAScript file.
- `web` starts a web server for an ECMAScript double debugger.
- `test262-test` tests Test262 tests with harness files (default: tests/test262).
- `inject` injects assertions to check final state of an ECMAScript file.
- `mutate` mutates an ECMAScript program.
- `analyze` analyzes an ECMAScript file using meta-level static analysis. (temporarily removed)
- `dump-debugger` dumps the resources required by the standalone debugger. (for internal use)
- `dump-visualizer` dumps the resources required by the visualizer. (for internal use)

and global options:
- `-silent` does not show final results.
- `-error` shows error stack traces.
- `-status` exits with status.
- `-time` displays the duration time.
- `-test262dir={string}` sets the directory of Test262 (default: `$ESMETA_HOME/tests/test262`).

If you want to see the detailed help messages and command-specific options,
please use the `help` command:
```bash
# show help messages for all commands
$ esmeta help

# show help messages for specific commands with more details
$ esmeta help <command>
```

Please use the `build-cfg` command to extract a mechanized specification as a
control-flow graph from ECMA-262:
```bash
$ esmeta build-cfg
# ========================================
#  extract phase
# ----------------------------------------
# ========================================
#  compile phase
# ----------------------------------------
# ========================================
#  build-cfg phase
# ----------------------------------------
# 0: def <BUILTIN>:INTRINSICS.SyntaxError(...): Unknown {
#   ...
# }
# 1: def <INTERNAL>:BuiltinFunctionObject.Construct(...): Normal[Object] | Abrupt[throw] {
#   ...
# }
# ...
```
The `build-cfg` command consists of three phases:
  1. The `extract` phase extracts specification model (`esmeta.spec.Spec`) from
     ECMA-262 (`spec.html`).
  2. The `compile` phase compiles it into a program (`esmeta.ir.Program`) in
     **IRES**, an **I**ntermediate **R**epresentations for **E**CMAScript
     **S**pecification.
  3. The `build-cfg` phase builds a control-flow graph (`esmeta.cfg.CFG`) for a
     given IRES program.

You can extract mechanized specifications from other versions of ECMA-262 with
the `-extract:target` option. Please enter any git tag/branch names or commit
hash as an input of the option:
```bash
# extract a mechanized specification from the origin/main branch version of ECMA-262
$ esmeta build-cfg -extract:target=origin/main

# extract a mechanized specification from the 2c78e6f commit version of ECMA-262
$ esmeta build-cfg -extract:target=2c78e6f
```

### Parsing and Executing ECMAScript files

After extracting mechanized specifications from ECMA-262, you can parse or
execute ECMAScript/JavaScript programs. For example, consider the following
example JavaScript file:
```js
// example.js
let x; x ??= class {}; function* f() {}
```
You can parse or execute it with `parse` and `eval` commands.
```bash
# parse example.js
$ esmeta parse example.js

# execute example.js
$ esmeta eval example.js
```

### Executing Test262 tests

ESMeta supports the execution of [Test262](https://github.com/tc39/test262)
tests to check the conformance between Test262 and ECMA-262.
```bash
# run all the applicable Test262 tests
$ esmeta test262-test
# ...
# ========================================
#  test262-test phase
# ----------------------------------------
# - harness                       :    96 tests are removed
# ...
# ----------------------------------------
# - total: 31,537 available tests
#   - normal: 31,537 tests
#   - error: 0 tests
# ----------------------------------------
# ...
```
If you want to execute specific Test262 files or directories, please list their
paths as arguments:
```bash
# run Test262 tests in a given directory
$ esmeta test262-test tests/test262/test/language/expressions/addition
```


## Supported Features

ESMeta supports other features utilizing mechanized specifications, including 1) exemplify specification with ECMA Visualizer, 2)
interactive execution of ECMAScript/JavaScript file with a double debugger, 3)
conformance test synthesizer, 4) type analysis of ECMA-262, and 5) meta-level
static analysis for ECMAScript/JavaScript files.  All of them utilize mechanized
specifications from ECMA-262. Thus, ESMeta always extracts mechanized
specifications as control-flow graphs before performing these features.

### Specification Exemplified with ECMA Visualizer

> [!NOTE]
>
> **A short [introduction video](https://youtu.be/4XMjJPNmuBM) for ECMA Visualizer and Double Debugger is available.**

<img width="1150" alt="ecma-visualizer" src="https://github.com/user-attachments/assets/733403f5-03cc-4465-a773-e57d46d35180" />

[**ECMA Visualizer**](https://chromewebstore.google.com/detail/nlfpedidieegejndiikebcgclhggaocd) is a Chrome Extension that helps users understand specifications by displaying rich information alongside the ecma-262 web documentation, collected through pre-fuzzing/measurement using ESMeta. This allows users to see helpful examples directly within the ecma-262 web documents. It provides the following features:

- Viewing minimal JavaScript program that passes through specific algorithm steps or control flow branches (`ReturnIfAbrupt`, denoted as `?`) in the specification
- Viewing conformance tests (from test262) that pass through selected steps
- Filtering displayed JS code using callpath
- One-click debugging capability to execute the displayed minimal JS code, resuming from the selected step

### Interactive Execution with ECMAScript Double Debugger

> [!NOTE]
>
> **A short [introduction video](https://youtu.be/4XMjJPNmuBM) for ECMA Visualizer and Double Debugger is available.**

**ECMAScript Double Debugger** extends the ECMAScript/JavaScript interpreter in
ESMeta to help you understand how a JavaScript Program runs according to
ECMA-262. Currently, it is in an **beta stage** and supports following features:

- Step-by-step execution of ECMA-262 algorithms
- Line-by-line execution of ECMAScript/JavaScript code
- Breakpoints by abstract algorithm names in ECMA-262
- Inspection of ECMA-262 internal states
- Seeing JavaScript state from the refined ECMA-262 state
- Step backward, or even trace back to the provenance of specification record

You can access the standalone debugger for the latest official release of specification directly at [es-meta.github.io/playground/](https://es-meta.github.io/playground/). Using this link gives you immediate access without having to install ESMeta, Scala, or any other dependencies on your system.

If needed, you can also set up and run the debugger locally with the following instructions:
```bash
# turn on server of the double debugger
$ esmeta web

# install and turn on the client-side application using NPM
$ cd client && npm install && npm start
```

<img width="1150" alt="debugger" src="https://github.com/user-attachments/assets/6c5f29a3-6d8a-458d-a4ed-478bb00666d7">

We will enhance it with the following features:
- Add more debugger features:
  - Record timestamps during execution for resume & suspend steps (especially for Generator).
  - ...
- Show the type of each variable using the type analysis result.
- Live-edit of `ecma262/spec.html` in the specification viewer.

### Conformance Test Synthesizer from ECMA-262

ESMeta supports the synthesis of JavaScript files as conformance tests. We
introduced the main concept of the test synthesis in the [ICSE 2021
paper](https://doi.org/10.1109/ICSE43902.2021.00015) with a tool named
[JEST](https://github.com/kaist-plrg/jest), a **J**avaScript **E**ngines and
**S**pecification **T**ester. The test synthesis technique consists of two
parts: 1) _program synthesis_ of JavaScript programs using **specification
coverage** and 2) _assertion injection_ based on the mechanized specification
extract from ECMA-262.

#### Synthesis of JavaScript Programs

If you want to synthesize JavaScript programs, please use the `fuzz` command:
```bash
esmeta fuzz -fuzz:log
```
It basically uses the **node/branch coverage** in the mechanized specification
to synthesize JavaScript programs. The `-fuzz:log` option dumps the synthesized
JavaScript programs into the `logs/fuzz/fuzz-<date>` directory with the detailed
information of the synthesis process:

* `seed`: seed of the random number generator
* `node-coverage.json`: node coverage information
* `branch-coverage.json`: branch coverage information
* `constructor.json`: constructor information
* `mutation-stat.tsv`: statistics of mutation methods
* `selector-stat.tsv`: statistics of selector methods
* `summary.tsv`: summary of the synthesis process for each logging interval
* `target-conds.json`: target conditions for the synthesis
* `unreach-funcs`: unreachable functions
* `version`: ESMeta version information

In addition, you can use **feature-sensitive coverage**, which is introduced in
the [PLDI 2023 paper](https://doi.org/10.1145/3591240), with the following
options:

* `-fuzz:k-fs=<int>`: the depth of features for feature-sensitive coverage
* `-fuzz:cp`: use the call path for feature-sensitive coverage

For example, you can synthesize JavaScript programs with the 2-FCPS coverage
(2-feature-sensitive coverage with call path) as follows:
```bash
esmeta fuzz -fuzz:log -fuzz:k-fs=2 -fuzz:cp
```

#### Assertion Injection

You can inject assertions into the synthesized JavaScript programs according to
the mechanized specification. If you want to inject assertions, please use the
`inject` command:
```bash
# inject assertions based on the semantics described in ECMA-262
$ esmeta inject example.js
# ...
# ========================================
#  inject phase
# ----------------------------------------
# // [EXIT] normal
# let x; x ??= class {}; function* f() {}
#
# $algo.set(f, "GeneratorDeclaration[0,0].InstantiateGeneratorFunctionObject")
# $assert.sameValue(Object.getPrototypeOf(f), GeneratorFunction.prototype);
# $assert.sameValue(Object.isExtensible(f), true);
# ...
```
It prints the assertion-injected JavaScript program without definitions of
assertions. The comment `// [EXIT] normal` denotes that this program should
normally terminate. From the fourth line, injected assertions represent the
expected value stored in variables, objects, or even internal properties.

If you want to dump the assertion-injected code to a program, please use the
`-inject:out` option. If you want to inject definitions of assertions as well,
please use the `-inject:defs` option:
```bash
$ esmeta inject example.js -silent -inject:defs -inject:out=test.js
# - Dumped an assertion-injected ECMAScript program into test.js.
```


### Type Analysis on ECMA-262

ESMeta provides a type analysis on ECMA-262 to infer unknown types in the
specification. We introduced its main concept in the [ASE 2021
paper](https://doi.org/10.1109/ASE51524.2021.9678781) with a tool names
[JSTAR](https://github.com/kaist-plrg/jstar), a **J**avaScript **S**pecification
**T**ype **A**nalyzer using **R**efinement. It analyzes types of mechanized
specification by performing type analysis of IRES. We utilized _condition-based
type refinement_ to prune out infeasible types in each branch for enhanced
analysis precision.

If you want to perform a type analysis of
[ES2022](https://262.ecma-international.org/13.0/) (or ES13), the latest
official version of ECMA-262, please type the following command:
```bash
$ esmeta tycheck
# ...
# ========================================
#  tycheck phase
# ----------------------------------------
# - 1806 functions are initial targets.
# - 2372 functions are analyzed in 32493 iterations.
```

You can perform type analysis on other versions of ECMA-262 with the
`-extract:target` option. Please enter any git tag/branch names or commit hash
as an input of the option:
```bash
# analyze types for origin/main branch version of ECMA-262
$ esmeta tycheck -extract:target=origin/main

# analyze types for 2c78e6f commit version of ECMA-262
$ esmeta tycheck -extract:target=2c78e6f
```


### ~~Meta-Level Static Analyzer for ECMAScript~~

> [!WARNING]
>
> The meta-level static analyzer is temporarily removed from the current version
> of ESMeta. We are working on the improvement of the meta-level static analyzer
> for ECMAScript/JavaScript programs. We will re-introduce this feature in the
> future version of ESMeta.

ESMeta also supports a meta-level static analyzer for ECMAScript/JavaScript
programs based on mechanized specifications extracted from ECMA-262. A
mechanized specification is an interpreter that can parse and execute JavaScript
programs. We introduced a way to indirectly analyze an ECMAScript/JavaScript
program by analyzing its interpreter with a restriction with the given program.
We call it _meta-level static analysis_ and presented this technique at
[ESEC/FSE 2022](https://dl.acm.org/doi/10.1145/3540250.3549097).

If you want to analyze JavaScript program using a meta-level static analysis,
please use the `analyze` command:
```bash
$ esmeta analyze example.js
# ...
# ========================================
#  analyze phase
# ----------------------------------------
# - 108 functions are analyzed in 1688 iterations.
```

ESMeta supports an interactive Read–eval–print loop (REPL) for the analysis with
the `-analyze:repl` option:
```bash
$ esmeta analyze example.js -analyze:repl
# ========================================
#  analyze phase
# ----------------------------------------
#
# command list:
# - help                     Show help message.
# ...
#
# [1] RunJobs[42]:Call[339] -> {
#   ...
# }

analyzer> continue
# - Static analysis finished. (# iter: 1688)

analyzer> print -expr @REALM.GlobalObject.SubMap.f.Value.SubMap.name.Value
# "f"

analyzer> exit
```
It showed that the property `name` of the global variable `f` points to a single
string `"f"`.

In the future version of ESMeta, we will add more kind documentation for this
analyzer REPL.

## Academic Achievement

### Publications

- **[FSE 2025 Demo] JSSpecVis: A JavaScript Language Specification Visualization Tool**
  ([old repo](https://github.com/ku-plrg/js-spec-vis))
- **[PLDI 2023] [Feature-Sensitive Coverage for Conformance Testing of Programming Language Implementations](https://doi.org/10.1145/3591240)**
  ([old repo](https://github.com/jestfs/jestfs))
- **[ESEC/FSE 2022] [Automatically Deriving JavaScript Static Analyzers from Specifications using Meta-Level Static Analysis](https://doi.org/10.1145/3540250.3549097)**
  ([old repo](https://github.com/kaist-plrg/jsaver))
- **[ASE 2021] [JSTAR: JavaScript Specification Type Analyzer using Refinement](https://doi.org/10.1109/ASE51524.2021.9678781)**
  ([old repo](https://github.com/kaist-plrg/jstar))
- **[ICSE 2021] [JEST: N+1-version Differential Testing of Both JavaScript Engines](https://doi.org/10.1109/ICSE43902.2021.00015)**
  ([old repo](https://github.com/kaist-plrg/jest))
- **[ASE 2020] [JISET: JavaScript IR-based Semantics Extraction Toolchain](https://doi.org/10.1145/3324884.3416632)**
  ([old repo](https://github.com/kaist-plrg/jiset))

### PLDI 2022 Tutorial

**Title**: **[Filling the gap between the JavaScript language specification and tools using the JISET family](https://pldi22.sigplan.org/details/pldi-2022-tutorials/1/Filling-the-gap-between-the-JavaScript-language-specification-and-tools-using-the-JIS)**
- Presenters: [Jihyeok Park](https://park.jihyeok.site/), [Seungmin An](https://github.com/h2oche), and [Sukyoung Ryu](https://plrg.kaist.ac.kr/ryu)
- [Session 1](https://plrg.korea.ac.kr/assets/data/slides/2022/pldi22-tutorial-1.pdf)
- [Session 2-1](https://plrg.korea.ac.kr/assets/data/slides/2022/pldi22-tutorial-2.pdf)
- [Session 2-2](https://plrg.korea.ac.kr/assets/data/slides/2022/pldi22-tutorial-3.pdf)

### Communications of the ACM (CACM)

- **Title**: **[JavaScript Language Design and Implementation in Tandem](https://cacm.acm.org/research/javascript-language-design-and-implementation-in-tandem/)**
  - DOI: [10.1145/3624723](https://doi.org/10.1145/3624723)

See the following video for more details:

<p align="center"><img src="http://img.youtube.com/vi/JGxc-KIUnQY/maxresdefault.jpg" width="500"></p>

- link: [https://youtu.be/JGxc-KIUnQY](https://youtu.be/JGxc-KIUnQY)

[![test](https://github.com/es-meta/esmeta/actions/workflows/ci.yml/badge.svg)](https://github.com/es-meta/esmeta/actions)
[![license](https://badgen.net/github/license/es-meta/esmeta)](https://github.com/es-meta/esmeta/blob/main/LICENSE.md)
[![release](https://badgen.net/github/release/es-meta/esmeta)](https://github.com/es-meta/esmeta/releases)
[![prs](https://badgen.net/github/prs/es-meta/esmeta)](https://github.com/es-meta/esmeta/pulls)
[![slack](https://badgen.net/badge/slack/esmeta/blue)](https://esmeta.slack.com/)

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
    + [Interactive Execution with ECMAScript Double Debugger](#interactive-execution-with-ecmascript-double-debugger)
    + [Conformance Test Synthesizer from ECMA-262](#conformance-test-synthesizer-from-ecma-262)
    + [Type Analysis on ECMA-262](#type-analysis-on-ecma-262)
    + [Meta-Level Static Analyzer for ECMAScript](#meta-level-static-analyzer-for-ecmascript)
  * [Academic Achievement](#academic-achievement)
    + [Publications](#publications)
    + [PLDI 2022 Tutorial](#pldi-2022-tutorial)


## Installation Guide

We explain how to install ESMeta with the necessary environment settings from
scratch. Our framework is developed in Scala, which works on JDK 8+, including
GraalVM. So before installation, please install [JDK
8+](https://www.oracle.com/java/technologies/downloads/) and
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
# Welcome to ESMeta v0.1.0-RC1 - ECMAScript Specification Metalanguage.
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
- `analyze` analyzes an ECMAScript file using meta-level static analysis.

and global options:
- `-silent` does not show final results.
- `-error` shows error stack traces.
- `-time` displays the duration time.

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
# 0: def <SYNTAX>:CallExpression[3,0].Evaluation(this: Unknown): Unknown {
#   ...
# }
# 1: def <CONT>:AsyncGeneratorStart:cont0(): Unknown {
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
# - total: 31,920 available tests
#   - normal: 31,920 tests
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

ESMeta supports other features utilizing mechanized specifications, including 1)
interactive execution of ECMAScript/JavaScript file with a double debugger, 2)
conformance test synthesizer, 3) type analysis of ECMA-262, and 4) meta-level
static analysis for ECMAScript/JavaScript files.  All of them utilize mechanized
specifications from ECMA-262. Thus, ESMeta always extracts mechanized
specifications as control-flow graphs before performing these features.

### Interactive Execution with ECMAScript Double Debugger

**ECMAScript Double Debugger** extends the ECMAScript/JavaScript interpreter in
ESMeta to help you understand how a JavaScript Program runs according to
ECMA-262. Currently, it is in an **alpha stage** and supports only basic
features such as:

- Step-by-step execution of ECMA-262 algorithms
- Line-by-line execution of ECMAScript/JavaScript code
- Breakpoints by abstract algorithm names in ECMA-262
- Visualization of ECMA-262 internal states

You can start it with the following instructions:
```bash
# turn on server of the double debugger
$ esmeta web

# install and turn on the client-side application using NPM
$ cd client && npm install && npm start
```

**A short [introduction video](https://youtu.be/syfZ3v6JNg8) is also available.**

<img width="1150" alt="debugger" src="https://user-images.githubusercontent.com/7039121/151577359-7d6a90af-7940-4904-912e-dd9113b8ba2f.png">

We will enhance it with the following features:
- Add more debugger features:
  - Show a JavaScript state by refining an ECMAScript state.
  - Record timestamps during execution for resume & suspend steps (especially for Generator).
  - ...
- Show relevant [Test262](https://github.com/tc39/test262) tests for each
  algorithm step in the specification viewer.
- Show the type of each variable using the type analysis result.
- Live-edit of `ecma262/spec.html` in the specification viewer.

### Conformance Test Synthesizer from ECMA-262

ESMeta supports the synthesis of JavaScript files as conformance tests. We
introduced the main concept of the test synthesis in the [ICSE
2021 paper](https://doi.org/10.1109/ICSE43902.2021.00015) with a tool named
[JEST](https://github.com/kaist-plrg/jest), a **J**avaScript **E**ngines and
**S**pecification **T**ester. The test synthesis technique consists of two
parts: 1) _program synthesis_ of JavaScript files and 2) _assertion injection_
based on the mechanized specification extract from ECMA-262.

The current version of ESMeta focuses on the assertion injection to a given
JavaScript file. If you want to inject assertions into the program conforming to
ECMA-262, please use the `inject` command:
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

In the future version of ESMeta, we plan to support the program synthesis
feature as well.


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
# - 1779 functions are initial targets.
# - 2360 functions are analyzed in 38704 iterations.
# --------------------------------------------------------------------------------
# ...
# --------------------------------------------------------------------------------
#    def <SYNTAX>:ThrowStatement[0,0].Evaluation(this: Ast:ThrowStatement[0,0]): Unknown
# -> def <SYNTAX>:ThrowStatement[0,0].Evaluation(this: Ast:ThrowStatement[0,0]): Abrupt
# ...
# --------------------------------------------------------------------------------
#    def ToBoolean(argument: Unknown): Unknown
# -> def ToBoolean(argument: ESValue): Boolean
# ...
```

You can perform type analysis on other versions of ECMA-262 with the
`-extract:target` option. Please enter any git tag/branch names or commit hash
as an input of the option:
```bash
# analyze types for origin/main branch version of ECMA-262
$ esmeta tycheck -extract:target=origin/main

# analyze types for 2c78e6f commit verison of ECMA-262
$ esmeta tycheck -extract:target=2c78e6f
```

In the future version of ESMeta, we plan to support a bug detector to detect
type-related editorial bugs in ECMA-262 based on the type analysis results.


### Meta-Level Static Analyzer for ECMAScript

ESMeta also supports a meta-level static analyzer for ECMAScript/JavaScript
programs based on mechanized specifications extracted from ECMA-262. A
mechanized specification is an interpreter that can parse and execute JavaScript
programs. We introduced a way to indirectly analyze an ECMAScript/JavaScript
program by analyzing its interpreter with a restriction with the given program.
We call it _meta-level static analysis_ and will present this technique at
[ESEC/FSE
2022](https://2022.esec-fse.org/details/fse-2022-research-papers/19/Automatically-Deriving-JavaScript-Static-Analyzers-from-Specifications-using-Meta-Lev).

If you want to analyze JavaScript program using a meta-level static analysis,
please use the `analyze` command:
```bash
$ esmeta analyze example.js
# ...
# ========================================
#  analyze phase
# ----------------------------------------
# - 111 functions are analyzed in 1708 iterations.
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
# [1] RunJobs[62]:Call[637] -> {
#   ...
# }

analyzer> continue
# - Static analysis finished. (# iter: 1708)

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

- [ASE 2020] [JISET: JavaScript IR-based Semantics Extraction
  Toolchain](https://doi.org/10.1145/3324884.3416632) [[old repo](https://github.com/kaist-plrg/jiset)]
- [ICSE 2021] [JEST: N+1-version Differential Testing of Both JavaScript
  Engines](https://doi.org/10.1109/ICSE43902.2021.00015) [[old repo](https://github.com/kaist-plrg/jest)]
- [ASE 2021] [JSTAR: JavaScript Specification Type Analyzer using Refinement](https://doi.org/10.1109/ASE51524.2021.9678781) [[old repo](https://github.com/kaist-plrg/jstar)]
- [ESEC/FSE 2022] Automatically Deriving JavaScript Static Analyzers from Specifications using Meta-Level Static Analysis [[old repo](https://github.com/kaist-plrg/jsaver)]

### PLDI 2022 Tutorial

**Title**: [Filling the gap between the JavaScript language specification and tools using the JISET family](https://pldi22.sigplan.org/details/pldi-2022-tutorials/1/Filling-the-gap-between-the-JavaScript-language-specification-and-tools-using-the-JIS)
- Presenters: [Jihyeok Park](https://park.jihyeok.site/), [Seungmin An](https://github.com/h2oche), and [Sukyoung Ryu](https://plrg.kaist.ac.kr/ryu)
- [Session 1](https://park.jihyeok.site/assets/data/slide/2022/pldi22-tutorial-1.pdf)
- [Session 2-1](https://park.jihyeok.site/assets/data/slide/2022/pldi22-tutorial-2.pdf)
- [Session 2-2](https://park.jihyeok.site/assets/data/slide/2022/pldi22-tutorial-3.pdf)

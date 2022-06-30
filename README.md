# ESMeta
**ESMeta** is an **E**CMAScript **S**pecification **Meta**language. This framework extracts a mechanized specification from a given version of ECMAScript specification ([ECMA-262](https://tc39.es/ecma262/)) and automatically generates language-based tools.

--------------------------------------------------------------------------------

## Publications

Details of the ESMeta are available in our papers:
- [ASE 2020] [JISET: JavaScript IR-based Semantics Extraction
  Toolchain](https://doi.org/10.1145/3324884.3416632) [[old repo](https://github.com/kaist-plrg/jiset)]
- [ICSE 2021] [JEST: N+1-version Differential Testing of Both JavaScript
  Engines](https://doi.org/10.1109/ICSE43902.2021.00015) [[old repo](https://github.com/kaist-plrg/jest)]
- [ASE 2021] [JSTAR: JavaScript Specification Type Analyzer using Refinement](https://doi.org/10.1109/ASE51524.2021.9678781) [[old repo](https://github.com/kaist-plrg/jstar)]

--------------------------------------------------------------------------------

## PLDI 2022 Tutorial

**Title**: Filling the gap between the JavaScript language specification and tools using the JISET family

- [Session 1](https://drive.google.com/file/d/17f6r35mCL0X_opndo2amruoDS-O9YNGg/view?usp=sharing) by Jihyeok Park
- [Session 2](https://drive.google.com/file/d/10h3MCcKPgxJ9X54GVRb9wVJBQpC_Oj_2/view?usp=sharing) by Seungmin An

--------------------------------------------------------------------------------

## Installation Guide

We explain how to install ESMeta with necessary environment settings from the
scratch. Our framework is developed in Scala, which works on JDK 8+, including GraalVM. Before installation, please install [JDK 8+](https://www.oracle.com/java/technologies/downloads/) and [sbt](https://www.scala-sbt.org/), an interactive build tool for Scala.


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
source $ESMETA_HOME/.completion # for autocompletion
```
The `<path to ESMeta>` should be the absolute path of ESMeta repository.


### Installation of ESMeta using `sbt`

```bash
$ cd esmeta && git submodule update --init && sbt assembly
```

--------------------------------------------------------------------------------

## Basic Commands

You can run this framework with the following command:
```bash
$ esmeta <command> <option>*
```
with the following commands:
- `help` shows the help message.
- `extract` extracts specification model from ECMA-262 (`ecma262/spec.html`).
- `compile` compiles a specification to an IR program.
- `build-cfg` builds a control-flow graph (CFG) from an IR program.
- `ir-eval` evaluates an IR file.
- `js-parse` parses a JavaScript file.
- `js-eval` evaluates a JavaScript file.
- `filter-test262` extracts and filters out metadata of
  [Test262](https://github.com/tc39/test262) tests.
- `web` starts a web server for interactive execution.

and global options:
- `-silent` does not show final results.
- `-debug` turns on the debug mode.
- `-log` turns on the logging mode.
- `-time` displays the duration time.

--------------------------------------------------------------------------------

## Future Plans

### Conformance Test Synthesizer for ECMAScript
We will import the conformance test synthesizer we developed
in [JEST](https://github.com/kaist-plrg/jest).

### Type Analyzer for ECMA-262
We will import the type analyzer for ECMA-262 we developed
[JSTAR](https://github.com/kaist-plrg/jstar).

### ECMAScript Double Debugger

**ECMAScript Double Debugger** extends the JavaScript interpreter in ESMeta to
help you understand how a JavaScript Program runs according to ECMA-262.
Currently, it is in an **alpha stage** and supports only basic features such as:

- Step-by-step execution of ECMAScript
- Breakpoints by abstract algorithm names in ECMAScript
- Visualization of states like a call stack, an environment, and a heap of ECMAScript
- Line-by-line execution of JavaScript

You can start it with the following instructions:
```bash
# turn on server of the double debugger
$ esmeta web

# install the client-side application using NPM
$ git clone https://github.com/es-meta/esmeta-debugger-client.git
$ cd esmeta-debugger-client
$ npm i

# turn on the client-side application
$ npm start
```

**A short [introduction video](https://youtu.be/syfZ3v6JNg8) is also available.**

<img width="1150" alt="debugger" src="https://user-images.githubusercontent.com/7039121/151577359-7d6a90af-7940-4904-912e-dd9113b8ba2f.png">

We will import the ECMAScript Debugger from
[JISET](https://github.com/kaist-plrg/jiset) and enhance it with the following features:
- Add more debugger features:
  - Show a JavaScript state by refining an ECMAScript state.
  - Record timestamps during execution for resume & suspend steps (especially for Generator).
  - ...
- Show relevant [Test262](https://github.com/tc39/test262) tests for each
  algorithm step in the specification viewer.
- Show the type of each variable using the type analysis result of JSTAR.
- Live-edit of `ecma262/spec.html` in the specification viewer.

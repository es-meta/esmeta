# ESMeta
**ESMeta** is **E**CMA**S**cript **Meta**language for Generation of
Language-based Tools. It automatically generates language-based tools using any
version of ECMAScript specification ([ECMA-262](https://tc39.es/ecma262/)).

## Publications

Details of the ESMeta are available in our papers:
- [ASE 2020] [JISET: JavaScript IR-based Semantics Extraction
  Toolchain](https://doi.org/10.1145/3324884.3416632)
- [ICSE 2021] [JEST: N+1-version Differential Testing of Both JavaScript
  Engines](https://doi.org/10.1109/ICSE43902.2021.00015)
- [ASE 2021] JSTAR: JavaScript Specification Type Analyzer using Refinement

## Installation Guide

We explain how to install ESMeta with necessary environment settings from the
scratch.  Before installation, please install JDK 8 or later version and
[`sbt`](https://www.scala-sbt.org/).

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
```
The `<path to ESMeta>` should be the absolute path of ESMeta repository.

### Installation of ESMeta using `sbt`
```bash
$ cd esmeta && sbt assembly
```

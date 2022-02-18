# ESMeta
**ESMeta** is an **E**CMAScript **S**pecification **Meta**language. This framework extracts a mechanized specification from a given version of ECMAScript specification ([ECMA-262](https://tc39.es/ecma262/)) and automatically generates language-based tools. 


## Publications

Details of the ESMeta are available in our papers:
- [ASE 2020] [JISET: JavaScript IR-based Semantics Extraction
  Toolchain](https://doi.org/10.1145/3324884.3416632) [[old repo](https://github.com/kaist-plrg/jiset)]
- [ICSE 2021] [JEST: N+1-version Differential Testing of Both JavaScript
  Engines](https://doi.org/10.1109/ICSE43902.2021.00015) [[old repo](https://github.com/kaist-plrg/jest)]
- [ASE 2021] [JSTAR: JavaScript Specification Type Analyzer using Refinement](https://doi.org/10.1109/ASE51524.2021.9678781) [[old repo](https://github.com/kaist-plrg/jstar)]


## Installation Guide

We explain how to install ESMeta with necessary environment settings from the
scratch. Our framework is developed in Scala, which works on JDK 11+, including GraalVM. Before installation, please install [JDK 11+](https://www.oracle.com/java/technologies/downloads/) and [sbt](https://www.scala-sbt.org/), an interactive build tool for Scala.


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
$ cd esmeta && git submodule init && git submodule update && sbt assembly
```

## Basic Commands

You can run this framework with the following command:
```bash
$ esmeta <command> <option>*
```
with the following commands:
- `help` shows the help message.
- `extract` extracts specification model from ECMA-262 (`spec.html`).
- `compile` compiles specification to a control-flow graph (CFG).

and global options:
- `-silent` does not show final results.
- `-debug` turns on the debug mode.
- `-log` turns on the logging mode.
- `-time` displays the duration time.

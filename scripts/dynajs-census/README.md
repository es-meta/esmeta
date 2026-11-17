# Node-kind census for DynaJS (ES2025 built-ins)

Classifies every metalanguage node kind that the algorithms reached by the
ES2025 built-ins use, by how `PolyfillGenerator` compiles it (Table I hook,
`default`, control, algorithm call, passthrough, Record, List structure,
unsupported, unparsed).

    sbt "runMain esmeta.opcount.AllBuiltins census-nodes.json"
    python3 scripts/dynajs-census/exhaustive_nodes.py census-nodes.json NODES.md

- `builtins.txt`: the 80 built-ins DynaJS extracts (the "80" columns).
- `aos_manual.txt`: abstract operations DynaJS models by hand; the closure of
  the 80 built-ins stops there.
- The script fails if a node kind seen in the census has no category.

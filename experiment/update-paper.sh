#!/usr/bin/env bash
# Update the paper's numbers from experiment/data; pass --coverage-only until oracle runs are ready.
set -euo pipefail
cd -- "$(dirname -- "$0")/.."
export ESMETA_HOME="$PWD"
paper=../synth262-paper/fse27
for script in rq1_table rq1_venn rq2_table; do
  python3 "experiment/$script.py" "$@" --tex "$paper/numbers/$script.tex"
done

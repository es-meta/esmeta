#!/usr/bin/env bash
# Recheck the current oracle against one fixed local corpus.
set -euo pipefail
cd -- "$(dirname -- "$0")/.."
export ESMETA_HOME="$PWD"
corpus=experiment/data/solve-1
[[ -d "$corpus" ]] || tar -xzf "$corpus.tar.gz" -C experiment/data
engine_home=${ESMETA_ENGINE_HOME:-$HOME/frozen/home}
for engine in v8 jsc graaljs sm xs qjs; do
  test -x "$engine_home/.jsvu/bin/$engine" || {
    echo "Missing frozen engine: $engine_home/.jsvu/bin/$engine" >&2
    exit 1
  }
done
mkdir -p logs/oracle-check
sbt assembly
time JAVA_OPTS="${JAVA_OPTS:--Xmx3g -Xss4m} -Duser.home=$engine_home" \
  bash bin/esmeta conform-test "$corpus/programs" -status \
  -conform-test:interaction -conform-test:out=logs/oracle-check/conform.json
python3 - <<'PYTHON'
import json, sys
from pathlib import Path
sys.path.insert(0, 'experiment')
from rq1_table import Defects
baseline = json.loads(Path('experiment/data/solve-1/baseline.json').read_text())
previous = set(baseline['known_rows'])
defects = Defects()
found = defects.found(Path('logs/oracle-check/conform.json'))
print(f'Known defects: {len(found)} (baseline: {len(previous)})')
print('No longer matched:', ', '.join(sorted(previous - found)) or '(none)')
print('Newly matched:', ', '.join(sorted(found - previous)) or '(none)')
print(f'Unclassified engine/program pairs: {len(defects.pending)}')
if defects.pending:
    print('Changed program text needs triage; unmatched is not confirmed regression.')
print('Report: logs/oracle-check/conform.json')
PYTHON

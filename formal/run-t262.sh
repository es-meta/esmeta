#!/bin/sh
# Run the exported Test262 tests against the Rocq model and write a report.
#
#   cd formal && ./run-t262.sh [timeout-seconds]
#
# Expects validation/Spec.v and validation/t262/*.v to exist; generate them
# with, from the repository root:
#
#   sbt "runMain esmeta.fv.FVInitState --test262 20"
#
# Every line of the report is one test: PASS, MISMATCH (with the reason the
# model got stuck, or the two observables that failed to unify), or TIMEOUT.
# A PASS means `vm_compute` reduced the model's whole run of that script and
# the result matched what ESMeta produced from the same initial state.
set -u

LIMIT=${1:-900}
LOGDIR=logs
REPORT=$LOGDIR/t262-report.txt
mkdir -p "$LOGDIR"

COQFLAGS="-q -w -all -Q . ESMetaFV"

if [ ! -f validation/Spec.vo ] || [ validation/Spec.v -nt validation/Spec.vo ]; then
  echo "== compiling validation/Spec.v (once, ~65s) =="
  if ! coqc $COQFLAGS validation/Spec.v > "$LOGDIR/Spec.log" 2>&1; then
    echo "Spec.v FAILED — see $LOGDIR/Spec.log"; exit 1
  fi
fi

: > "$REPORT"
pass=0; mismatch=0; timedout=0

for f in validation/t262/T*.v; do
  [ -e "$f" ] || { echo "no tests in validation/t262/ — generate them first"; exit 1; }
  n=$(basename "$f" .v)
  src=$(grep -m1 '^(\* AUTO-GENERATED' "$f" | sed 's/.*— //; s/ \*)//')
  start=$(date +%s)

  # portable timeout: run in the background and poll (macOS has no `timeout`)
  coqc $COQFLAGS "$f" > "$LOGDIR/$n.log" 2>&1 &
  pid=$!
  waited=0
  while kill -0 "$pid" 2>/dev/null && [ "$waited" -lt "$LIMIT" ]; do
    sleep 1; waited=$((waited + 1))
  done
  if kill -0 "$pid" 2>/dev/null; then
    kill -9 "$pid" 2>/dev/null; wait "$pid" 2>/dev/null
    rc=124
  else
    wait "$pid"; rc=$?
  fi
  elapsed=$(( $(date +%s) - start ))

  if [ "$rc" -eq 0 ]; then
    verdict="PASS"; pass=$((pass + 1))
  elif [ "$rc" -eq 124 ]; then
    verdict="TIMEOUT (${LIMIT}s)"; timedout=$((timedout + 1))
  else
    why=$(grep -o 'Stuck "[A-Za-z()]*"' "$LOGDIR/$n.log" | head -1)
    [ -n "$why" ] || why=$(grep -m1 'Error:' "$LOGDIR/$n.log" | cut -c1-90)
    verdict="MISMATCH  $why"; mismatch=$((mismatch + 1))
  fi
  printf '%-6s %5ss  %-28s %s\n' "$n" "$elapsed" "$verdict" "$src" | tee -a "$REPORT"
done

total=$((pass + mismatch + timedout))
{
  echo
  echo "matched     $pass / $total"
  echo "mismatched  $mismatch / $total"
  echo "timed out   $timedout / $total"
  echo
  echo "Tests ESMeta itself could not run, and tests whose data the model"
  echo "cannot represent, are never emitted — see the counts printed by"
  echo "FVInitState --test262, which are NOT part of the totals above."
} | tee -a "$REPORT"
echo "report: $REPORT   per-test logs: $LOGDIR/T*.log"

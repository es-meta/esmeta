#!/bin/zsh
# Run every generated payload through the native direct-backend worker.
#
# The worker is one-shot, so payloads are handed out in batches: each process
# pays the ~5 s startup (a 407 MB binary) once per batch, not once per test.
#
#   ./run-direct-t262.sh [JOBS] [BATCH]
#
# Results land in logs/direct-t262-full.log; the tally is printed at the end
# and is derived from that file, so an interrupted run can still be counted.
set -e
cd "$(dirname "$0")"

JOBS=${1:-14}
BATCH=${2:-250}
LOG=logs/direct-t262-full.log

test -x ./fvitree-direct-worker || {
  echo "missing ./fvitree-direct-worker — run: make direct-worker" >&2
  exit 1
}

mkdir -p logs
: > "$LOG"
count=$(ls validation/payload/*.fvt | wc -l | tr -d ' ')
echo "running $count payload(s), $JOBS jobs x $BATCH per batch"
start=$(date +%s)

# Per-test verdict lines only; the per-batch summaries would double-count.
ls validation/payload/*.fvt \
  | xargs -P "$JOBS" -n "$BATCH" ./fvitree-direct-worker \
  | grep -E '^(PASS|MISMATCH|STUCK|FUEL|DECODE-ERROR) ' >> "$LOG" || true

elapsed=$(( $(date +%s) - start ))
echo
echo "=== $count payload(s) in ${elapsed}s ($JOBS jobs) ==="
for verdict in PASS MISMATCH STUCK FUEL DECODE-ERROR; do
  printf '%-13s %s\n' "$verdict" "$(grep -c "^$verdict " "$LOG" || true)"
done
echo "report: $LOG"

#!/bin/zsh
# Full Test262 sweep on the direct backend, shard by shard.
#
# Why sharded: the exporter materialises every payload of a batch before
# writing any (FVInitState.scala:2682), so one 32,207-test batch would need
# ~35 GB of payload bytes resident.  It also deletes the payload directory at
# the start of each batch, so each shard has to be consumed before the next
# one is exported — hence export and run alternate here.
#
# ESMeta is invoked as a plain `java` process, not through sbt: sbt's
# .jvmopts pins -Xmx3g and wins over the launcher's -J flags.
#
#   ./run-direct-t262-full.sh [SHARD] [JOBS] [BATCH] [START_OFFSET]
set -e
cd "$(dirname "$0")"

SHARD=${1:-500}
JOBS=${2:-14}
BATCH=${3:-50}
START=${4:-0}

TOTAL=$(sed -n '1p' validation/test262-inventory.tsv \
  | grep -o 'target=[0-9]*' | cut -d= -f2)
CP=$(cat /tmp/esmeta_cp.txt)
LOG=logs/direct-t262-full.log
STATE=logs/direct-t262-full.state

test -x ./fvitree-direct-worker || { echo "missing worker" >&2; exit 1; }
mkdir -p logs
if [[ "$START" == "0" ]]; then : > "$LOG"; : > "$STATE"; fi

echo "target=$TOTAL shard=$SHARD jobs=$JOBS batch=$BATCH start=$START"
began=$(date +%s)

offset=$START
while (( offset < TOTAL )); do
  shard_start=$(date +%s)

  java -Xmx32g -Xss512m -cp "$CP" esmeta.fv.FVInitState \
    --test262-shard "$offset" "$SHARD" --payload-only --reuse-test262-base \
    > "logs/export-$offset.log" 2>&1 || {
      echo "EXPORT-FAILED offset=$offset (see logs/export-$offset.log)" \
        | tee -a "$STATE"
      offset=$(( offset + SHARD ))
      continue
    }
  exported=$(ls validation/payload/*.fvt 2>/dev/null | wc -l | tr -d ' ')
  export_done=$(date +%s)

  ls validation/payload/*.fvt \
    | xargs -P "$JOBS" -n "$BATCH" ./fvitree-direct-worker \
    | grep -E '^(PASS|MISMATCH|STUCK|FUEL|DECODE-ERROR) ' >> "$LOG" || true
  shard_end=$(date +%s)

  printf 'offset=%s exported=%s export=%ss run=%ss cumulative=%s\n' \
    "$offset" "$exported" "$(( export_done - shard_start ))" \
    "$(( shard_end - export_done ))" "$(wc -l < "$LOG" | tr -d ' ')" \
    | tee -a "$STATE"

  offset=$(( offset + SHARD ))
done

echo
echo "=== finished in $(( $(date +%s) - began ))s ==="
for verdict in PASS MISMATCH STUCK FUEL DECODE-ERROR; do
  printf '%-13s %s\n' "$verdict" "$(grep -c "^$verdict " "$LOG" || true)"
done

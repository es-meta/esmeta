#!/bin/sh
# Pack and unpack the fuzzer and solver run data; git carries the tarballs, not
# the hundreds of megabytes of extracted files.
#
#   experiment/data.sh unpack        extract every tarball that has no directory
#   experiment/data.sh unpack -f     extract them all, replacing what is there
#   experiment/data.sh pack          re-pack every directory
#   experiment/data.sh pack NAME...  re-pack only the named runs
#   experiment/data.sh pack-logs     re-pack the conform-test logs
#
# gzip -n and COPYFILE_DISABLE are load-bearing: without the first, re-packing
# an unchanged run writes new bytes and git stores it again; without the second
# macOS tar adds an AppleDouble ._name beside anything with an extended
# attribute. Run tarballs keep raw inputs and provenance.
# Conform-test reports go together in conform/logs.tar.gz, which unpack also
# extracts.
#
# A run is frozen from its log directory, timed since the paper reports it:
#
#   export ESMETA_HOME=$PWD JAVA_OPTS="-Xmx32g -Xss1g"
#   time bin/esmeta solve -solve:log
#   mkdir experiment/data/solve-6
#   cp -R logs/solver/recent/{programs,branch-coverage.json,summary,templates.json} experiment/data/solve-6/
#   time bin/esmeta fuzz -fuzz:log -fuzz:duration=180000   # 50 hours
#   cp -R logs/fuzz/recent/ experiment/data/fuzz-6
#   experiment/data.sh pack solve-6 fuzz-6

set -eu

dir=$(CDPATH= cd -- "$(dirname -- "$0")/data" && pwd)
logs="$dir/../conform/logs.tar.gz"
action=${1:-}
force=${2:-}
[ $# -gt 0 ] && shift

case "$action" in
  pack)
    found=0
    [ $# -gt 0 ] || set -- $(cd "$dir" && ls -d */ | tr -d /)
    for name in "$@"; do
      [ -d "$dir/$name" ] || { echo "no run directory $dir/$name" >&2; exit 1; }
      COPYFILE_DISABLE=1 tar --exclude '.DS_Store' --exclude '._*' \
        --exclude "$name/amplified" --exclude "$name/amplify.log" --exclude "$name/conform-*" \
        --exclude "$name/edit-distance.tsv" \
        -cf - -C "$dir" "$name" | gzip -n9 > "$dir/$name.tar.gz"
      printf '  packed   %-8s %s\n' "$name" "$(du -h "$dir/$name.tar.gz" | cut -f1)"
      found=$((found + 1))
    done
    [ "$found" -gt 0 ] || { echo "no run directories under $dir" >&2; exit 1; }
    ;;
  unpack)
    found=0
    for path in "$dir"/*.tar.gz; do
      [ -f "$path" ] || continue
      name=$(basename "$path" .tar.gz)
      if [ -d "$dir/$name" ] && [ "$force" != "-f" ]; then
        printf '  kept     %s\n' "$name"
      else
        rm -rf "$dir/${name:?}"
        tar -xzf "$path" -C "$dir"
        printf '  unpacked %s\n' "$name"
      fi
      found=$((found + 1))
    done
    [ "$found" -gt 0 ] || { echo "no tarballs under $dir" >&2; exit 1; }
    if [ -f "$logs" ]; then
      tar -xzf "$logs" -C "$dir"
      printf '  unpacked conform-test logs\n'
    fi
    ;;
  pack-logs)
    (cd "$dir" && ls -d */conform-*.json) > /dev/null
    # machine paths out, so the artifact names no one: the repository root,
    # any jsvu home or frozen prefix, and the temporary directory become
    # relative, also for logs made on another machine
    root=$(cd "$dir/../.." && pwd)
    tmp=${TMPDIR:-/tmp/}
    for f in "$dir"/*/conform-*.json; do
      perl -pe "s|\Q$root/\E||g; s|\Q${tmp%/}/\E|\\\$TMPDIR/|g; s|/tmp/esmeta-|\\\$TMPDIR/esmeta-|g; s|/[^\" ]*/(?=experiment/data/)||g; s|/[^\" ]*/\.jsvu/|~/.jsvu/|g; s|/[^\" ]*/frozen/|~/frozen/|g" "$f" > "$f.new"
      # an unchanged file keeps its mtime, so the tarball keeps its bytes
      if cmp -s "$f" "$f.new"; then rm "$f.new"; else mv "$f.new" "$f"; fi
    done
    (cd "$dir" && COPYFILE_DISABLE=1 tar -cf - */conform-*.json) | gzip -n9 > "$logs"
    printf '  packed   conform-test logs %s\n' "$(du -h "$logs" | cut -f1)"
    ;;
  *)
    sed -n '2,16p' "$0" | sed 's/^# \{0,1\}//'
    exit 1
    ;;
esac

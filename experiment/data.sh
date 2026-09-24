#!/bin/sh
# Pack and unpack the fuzzer and solver run data; git carries the tarballs, not
# the hundreds of megabytes of extracted files.
#
#   experiment/data.sh unpack        extract every tarball that has no directory
#   experiment/data.sh unpack -f     extract them all, replacing what is there
#   experiment/data.sh pack          re-pack every directory
#   experiment/data.sh pack NAME...  re-pack only the named runs
#
# gzip -n and COPYFILE_DISABLE are load-bearing: without the first, re-packing
# an unchanged run writes new bytes and git stores 8M again; without the second
# macOS tar adds an AppleDouble ._name beside anything with an extended
# attribute. What reduce and conform-test derive from a run stays out: reduce
# is deterministic and conform-test reruns from the programs.

set -eu

dir=$(CDPATH= cd -- "$(dirname -- "$0")/data" && pwd)
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
        --exclude "$name/reduced" --exclude "$name/reduce.log" --exclude "$name/conform-*" \
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
    ;;
  *)
    sed -n '2,15p' "$0" | sed 's/^# \{0,1\}//'
    exit 1
    ;;
esac

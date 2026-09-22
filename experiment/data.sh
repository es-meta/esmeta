#!/bin/sh
# Pack and unpack the fuzzer run data; git carries the tarballs, not the 259M
# of extracted files.
#
#   experiment/data.sh unpack        extract every tarball that has no directory
#   experiment/data.sh unpack -f     extract them all, replacing what is there
#   experiment/data.sh pack          re-pack every directory
#
# gzip -n and COPYFILE_DISABLE are load-bearing: without the first, re-packing
# an unchanged run writes new bytes and git stores 8M again; without the second
# macOS tar adds an AppleDouble ._name beside anything with an extended
# attribute.

set -eu

dir=$(CDPATH= cd -- "$(dirname -- "$0")/data" && pwd)
action=${1:-}
force=${2:-}

case "$action" in
  pack)
    found=0
    for path in "$dir"/*/; do
      [ -d "$path" ] || continue
      name=$(basename "$path")
      COPYFILE_DISABLE=1 tar --exclude '.DS_Store' --exclude '._*' \
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

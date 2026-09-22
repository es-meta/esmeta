#!/bin/sh
# Pack and unpack the fuzzer run data.
#
# Git carries one compressed tarball per run and ignores the extracted
# directory: the nine runs are 259M of mostly JSON on disk but 8M packed, and
# 21,000 of the files are one-line programs under minimal/.
#
#   experiment/data.sh unpack        extract every tarball that has no directory
#   experiment/data.sh unpack -f     extract them all, replacing what is there
#   experiment/data.sh pack          re-pack every directory
#
# gzip is told not to record its own timestamp, so re-packing an unchanged run
# produces the same bytes and git sees no diff. COPYFILE_DISABLE keeps macOS
# tar from writing an AppleDouble ._name beside every file that carries an
# extended attribute, and .DS_Store never goes in.

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

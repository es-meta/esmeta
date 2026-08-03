#!/bin/sh
set -eu

tests_file=${1:-validation/Tests.v}
output=${2:-validation/ExtractShard.v}
split_dir=${3:-build/itree/extract-shard}

modules=$(
  sed -n \
    's/^From ESMetaFV\.validation\.itree Require Import \(.*\)\.$/\1/p' \
    "$tests_file"
)

if [ -z "$modules" ]; then
  echo "no Test262 modules found in $tests_file" >&2
  exit 1
fi

mkdir -p "$split_dir"

write_if_changed() {
  target=$1
  temporary=$(mktemp "${target}.tmp.XXXXXX")
  cat > "$temporary"
  if [ -f "$target" ] && cmp -s "$temporary" "$target"; then
    rm -f "$temporary"
  else
    mv "$temporary" "$target"
  fi
}

{
  echo '(** AUTO-GENERATED modular extraction driver.  DO NOT EDIT. *)'
  echo 'From Stdlib Require Import Extraction.'
  echo 'From ESMetaFV Require Import ExtractionConfig ITreeCore.'
  echo 'From ESMetaFV Require Import Tests.'
  echo
  echo 'Set Extraction Output Directory "build/itree/shard".'
  echo 'Extraction Blacklist String List.'
  for module in $modules; do
    printf 'Extraction Library %s.\n' "$module"
  done
  echo 'Extraction Library Tests.'
} | write_if_changed "$output"

for module in $modules; do
  {
    echo '(** AUTO-GENERATED parallel shard extraction driver.  DO NOT EDIT. *)'
    echo 'From Stdlib Require Import Extraction.'
    echo 'From ESMetaFV Require Import ExtractionConfig ITreeCore.'
    printf 'From ESMetaFV.validation.itree Require Import %s.\n' "$module"
    echo
    echo 'Set Extraction Output Directory "build/itree/shard".'
    echo 'Extraction Blacklist String List.'
    printf 'Extraction Library %s.\n' "$module"
  } | write_if_changed "$split_dir/Extract_${module}.v"
done

# [Tests.v] only assembles the already typechecked per-test payloads into a
# list.  Running Rocq extraction for that list reloads every multi-megabyte
# AST module and becomes a serial memory/time bottleneck.  Emit the identical
# OCaml list directly; each referenced [test_NNN] still comes exclusively
# from its independently checked and extracted Rocq module.
{
  echo '(* AUTO-GENERATED aggregate of Rocq-extracted Test262 payloads. *)'
  echo 'let tests ='
  for module in $modules; do
    suffix=${module#T}
    printf '  %s.test_%s ::\n' "$module" "$suffix"
  done
  echo '  []'
} | write_if_changed "$split_dir/Tests.ml"

{
  echo '(** AUTO-GENERATED parallel aggregate extraction driver.  DO NOT EDIT. *)'
  echo 'From Stdlib Require Import Extraction.'
  echo 'From ESMetaFV Require Import ExtractionConfig ITreeCore.'
  echo 'From ESMetaFV Require Import Tests.'
  echo
  echo 'Set Extraction Output Directory "build/itree/shard".'
  echo 'Extraction Blacklist String List.'
  echo 'Extraction Library Tests.'
} | write_if_changed "$split_dir/Extract_Tests.v"

# Only generated drivers live here.  Remove modules that disappeared from
# Tests.v so a smaller regenerated shard cannot accidentally link stale code.
for driver in "$split_dir"/Extract_T*.v; do
  [ -e "$driver" ] || continue
  name=${driver##*/Extract_}
  name=${name%.v}
  if [ "$name" = Tests ]; then
    continue
  fi
  found=false
  for module in $modules; do
    if [ "$name" = "$module" ]; then
      found=true
      break
    fi
  done
  if [ "$found" = false ]; then
    rm -f "$driver"
  fi
done

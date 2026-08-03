#!/bin/zsh
# Compile the separately-extracted direct/generic Test262 runner in dependency
# order, mirroring the $(ITREE_CORE_BUILD)/.compiled recipe in Makefile.
set -e
cd "$(dirname "$0")/build/direct-t262"

PKGS=zarith,rocq-runtime.kernel,unix
# zsh does not word-split a scalar, so the flags have to be an array.
FLAGS=(-thread -w -a)

ocamldep -sort *.mli > .mli-order
total=$(wc -w < .mli-order | tr -d " ")
n=0
for source in $(cat .mli-order); do
  n=$((n + 1))
  echo "[mli $n/$total] $source"
  ocamlfind ocamlc -package $PKGS $FLAGS -I . -c "$source"
done

ocamldep -sort *.ml > .ml-order
total=$(wc -w < .ml-order | tr -d " ")
n=0
for source in $(cat .ml-order); do
  n=$((n + 1))
  echo "[ml $n/$total] $source"
  ocamlfind ocamlc -package $PKGS $FLAGS -I . -c "$source"
done

echo "COMPILED"

#!/bin/zsh
# Native build of the direct-only Test262 runner.
#
# `ocamlopt` was previously out of reach because the generated specification
# extracted as one enormous Spec module.  The direct lane does not reach it:
# direct_script_prog leaves the IR function list out of the program, so no
# SpecFuncs module is in the closure and every remaining module is small.
set -e
cd "$(dirname "$0")/build/direct-worker"

# ocamlopt recurses deeply over these generated terms; the default 8 MB stack
# is not enough (it overflowed on a 2.9 MB module before the payload split).
ulimit -s "$(ulimit -Hs)" 2>/dev/null || true

PKGS=zarith,rocq-runtime.kernel,unix
# zsh does not word-split a scalar, so the flags have to be an array.
FLAGS=(-thread -w -a)

ocamldep -sort *.mli > .mli-order
total=$(wc -w < .mli-order | tr -d " ")
n=0
for source in $(cat .mli-order); do
  n=$((n + 1))
  echo "[mli $n/$total] $source"
  ocamlfind ocamlopt -package $PKGS $FLAGS -I . -c "$source"
done

ocamldep -sort *.ml > .ml-order
total=$(wc -w < .ml-order | tr -d " ")
n=0
for source in $(cat .ml-order); do
  n=$((n + 1))
  echo "[ml $n/$total] $source"
  ocamlfind ocamlopt -package $PKGS $FLAGS -I . -c "$source"
done

echo "COMPILED"

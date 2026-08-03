#!/usr/bin/env python3
"""Prepare a native-only extracted core backed by a stable spec snapshot."""

from __future__ import annotations

import re
import shutil
from pathlib import Path


SCRIPT_DIR = Path(__file__).resolve().parent
SOURCE = SCRIPT_DIR / "build" / "itree" / "core"
OUTPUT = SCRIPT_DIR / "build" / "itree" / "native-core"

SPEC_MLI = """open Fragment
open TestEncoding

val spec_funcs : func list
val base_globals : (string * coq_val) list
val init_heap : obj option list
val script_prog : cstr -> ast -> host_cache_entry list -> prog
"""

SPEC_ML = """open Fragment
open TestEncoding

let snapshot_magic = "ESMETA_FV_SPEC_SNAPSHOT_V1"

let snapshot_path () =
  match Sys.getenv_opt "ESMETA_FV_SPEC_SNAPSHOT" with
  | Some path -> path
  | None ->
      Filename.concat (Filename.dirname Sys.executable_name)
        "build/itree/spec-data.bin"

let load_spec_data () =
  let path = snapshot_path () in
  let input = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in input)
    (fun () ->
      let magic, spec_funcs, base_globals, init_heap =
        (Marshal.from_channel input :
          string * func list * (string * coq_val) list * obj option list)
      in
      if magic <> snapshot_magic then
        failwith ("unsupported specification snapshot: " ^ magic);
      spec_funcs, base_globals, init_heap)

let spec_funcs, base_globals, init_heap = load_spec_data ()

let script_prog src a hosts =
  { p_funcs = spec_funcs; p_source = Some src; p_cached = Some a;
    p_hosts = hosts; p_globals = ("SOURCE_TEXT", VStr src) :: base_globals;
    p_heap = init_heap }
"""

# Rocq's [Require Export Spec] facade does not change the defining module of
# these generated constants.  Extraction therefore emits some clients that
# open [SpecFuncs], [SpecGlobals], and [SpecHeap] directly.  Preserve those
# module names as tiny aliases to the single snapshot loaded by [Spec].
SPEC_COMPAT_MODULES = {
    "SpecFuncs.mli": """open Fragment

val spec_funcs : func list
""",
    "SpecFuncs.ml": """let spec_funcs = Spec.spec_funcs
""",
    "SpecGlobals.mli": """open Fragment

val base_globals : (string * coq_val) list
""",
    "SpecGlobals.ml": """let base_globals = Spec.base_globals
""",
    "SpecHeap.mli": """open Fragment

val init_heap : obj option list
""",
    "SpecHeap.ml": """let init_heap = Spec.init_heap
""",
}


def is_split_spec_module(path: Path) -> bool:
    """Return whether an extracted module belongs to generated Spec data.

    The native worker replaces the stable `Spec` facade with one snapshot
    loader.  Copying any of its generated data shards would compile and link
    the same multi-megabyte immutable state a second time.
    """

    stem = path.stem
    return (
        stem in {"Spec", "SpecFuncs", "SpecGlobals", "SpecHeap"}
        or re.fullmatch(r"Spec(?:Funcs|Heap)_\d{4}", stem) is not None
    )


def main() -> int:
    source = SOURCE.resolve()
    output = OUTPUT.resolve()
    if not (source / ".extracted").is_file():
        raise ValueError(f"extracted core is incomplete: {source}")
    if output.exists():
        shutil.rmtree(output)
    output.mkdir(parents=True)
    for path in sorted(source.iterdir()):
        if path.suffix not in {".ml", ".mli"} or is_split_spec_module(path):
            continue
        shutil.copy2(path, output / path.name)
    (output / "Spec.mli").write_text(SPEC_MLI, encoding="utf-8")
    (output / "Spec.ml").write_text(SPEC_ML, encoding="utf-8")
    for name, contents in SPEC_COMPAT_MODULES.items():
        (output / name).write_text(contents, encoding="utf-8")
    (output / ".prepared").touch()
    print(f"prepared native core in {output}")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, ValueError) as error:
        raise SystemExit(f"error: {error}")

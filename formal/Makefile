# ESMetaFV — Rocq development for IR-Core transpiler verification.
#
# Requires the `cris-workshop` opam switch (Rocq 9.0.0 + pinned rocq-cris);
# see README.md in this directory for one-time setup.
#
# Build:  make        (from this directory)
# Clean build/generated artifacts: make clean (campaign logs are preserved)

COQC ?= coqc
COQDEP ?= coqdep
ITREE_FUEL ?= 100000000
ITREE_ALGORITHM_LINES ?= 120
ITREE_BUILD := build/itree
ITREE_CORE_BUILD := $(ITREE_BUILD)/core
ITREE_NATIVE_CORE_BUILD := $(ITREE_BUILD)/native-core
ITREE_NATIVE_WORKER_BUILD := $(ITREE_BUILD)/native-worker
ITREE_SPEC_SNAPSHOT := $(ITREE_BUILD)/spec-data.bin
ITREE_SHARD_BUILD := $(ITREE_BUILD)/shard
ITREE_SHARD_DRIVERS := $(ITREE_BUILD)/extract-shard
ITREE_JOBS ?= 4
OCAML_EXTRACT_PACKAGES := zarith,rocq-runtime.kernel,unix
OCAML_GENERATED_FLAGS := -thread -w -a
OCAML_RUNTIME_FLAGS := -thread -w +a-4-70 \
	-warn-error +8+11+26+27+32+39
OCAMLFIND_CORE_ENV := \
	OCAMLFIND_IGNORE_DUPS_IN=$(abspath $(ITREE_CORE_BUILD)):$(abspath $(ITREE_NATIVE_CORE_BUILD))
T262_FULL_ARGS ?=
JS_EQUIV_INPUT_STAMP := $(ITREE_BUILD)/js-equiv-inputs.stamp
JS_EQUIV_ARTIFACT_STAMP := $(ITREE_BUILD)/js-equiv-artifacts.stamp
SPEC_SOURCE_MANIFEST := validation/SpecSources.mk

# FVInitState emits the immutable specification as independent compilation
# units.  Keep the manifest optional for the ordinary, specification-free
# build; targets that consume Spec.v validate it explicitly below.
-include $(SPEC_SOURCE_MANIFEST)

SPEC_FUNC_SHARD_SOURCES := \
	$(filter validation/spec/SpecFuncs_%.v,$(SPEC_GENERATED_SOURCES))
SPEC_HEAP_SHARD_SOURCES := \
	$(filter validation/spec/SpecHeap_%.v,$(SPEC_GENERATED_SOURCES))
SPEC_FUNC_SHARD_OBJECTS := $(SPEC_FUNC_SHARD_SOURCES:.v=.vo)
SPEC_HEAP_SHARD_OBJECTS := $(SPEC_HEAP_SHARD_SOURCES:.v=.vo)
SPEC_FACADE_SOURCES := \
	validation/spec/SpecFuncs.v \
	validation/spec/SpecGlobals.v \
	validation/spec/SpecHeap.v
SPEC_FACADE_OBJECTS := $(SPEC_FACADE_SOURCES:.v=.vo)

# Flags mirror the CRIS workshop build (2026-verification-workshop/Makefile):
# same warning suppressions and the pre-imported monad notation.
COQFLAGS := \
	-q \
	-w -deprecated-hint-without-locality \
	-w -deprecated-instance-without-locality \
	-w -notation-incompatible-prefix \
	-w -notation-overriden \
	-w -ambiguous-paths \
	-w -redundant-canonical-projection \
	-w -cannot-define-projection \
	-require-import ExtLib.Structures.Monad \
	-Q . ESMetaFV

# coqdep accepts only the load-path options, not -q/-require-import.
COQDEPFLAGS := -Q . ESMetaFV

TYMODEL_SHARD_SOURCES := \
	TyModelBase.v \
	$(sort $(wildcard TyModelParent*.v)) \
	$(sort $(wildcard TyModelBindings*.v))

SOURCES := \
	Fragment.v \
	$(TYMODEL_SHARD_SOURCES) \
	TyModel.v \
	TestEncoding.v \
	Domain.v \
	Events.v \
	Semantics.v \
	ITreeExec.v \
	Programs.v \
	Examples.v \
	Exec.v \
	Transform.v \
	Validation.v \
	T1Proof.v
OBJECTS := $(SOURCES:.v=.vo)
DEPS := .deps.mk

all: $(OBJECTS)

# Compile the complete active proof/regression surface.  Extraction and the
# generated Test262 specification remain explicit targets because they are
# expensive and require generated inputs.
check: all regressions

# Differential validation (PO-011): compile the generated expectations.
# Regenerate with:  sbt "runMain esmeta.fv.FVExport"   (from the repo root)
validate: all validation/Generated.v
	$(COQC) $(COQFLAGS) validation/Generated.v

validation/Generated.v:
	@echo "validation/Generated.v missing — run: sbt \"runMain esmeta.fv.FVExport\"" && exit 1

# The ECMAScript specification plus its initial state (stage G4).
# Regenerate with:  sbt "runMain esmeta.fv.FVInitState [source]"
spec: all validation/Spec.vo

validation/Spec.v:
	@echo "validation/Spec.v missing — run: sbt \"runMain esmeta.fv.FVInitState\"" && exit 1

.PHONY: spec-manifest-ready
spec-manifest-ready:
	@test -f "$(SPEC_SOURCE_MANIFEST)" || { \
	  echo "$(SPEC_SOURCE_MANIFEST) missing — run: sbt \"runMain esmeta.fv.FVInitState\""; \
	  exit 1; \
	}
	@test -n "$(strip $(SPEC_GENERATED_SOURCES))" || { \
	  echo "$(SPEC_SOURCE_MANIFEST) contains no generated sources"; \
	  exit 1; \
	}

# Native execution of the actual ITree denotation in Semantics.v.
# Needs validation/Spec.v and validation/Tests.v, from the repository root:
#   sbt "runMain esmeta.fv.FVInitState"
#   sbt "runMain esmeta.fv.FVInitState --test262 20"
validation/spec/SpecFuncs_%.vo: validation/spec/SpecFuncs_%.v \
    Fragment.vo Domain.vo TestEncoding.vo
	$(COQC) $(COQFLAGS) $<

validation/spec/SpecHeap_%.vo: validation/spec/SpecHeap_%.v \
    Fragment.vo Domain.vo TestEncoding.vo
	$(COQC) $(COQFLAGS) $<

validation/spec/SpecFuncs.vo: validation/spec/SpecFuncs.v \
    $(SPEC_FUNC_SHARD_OBJECTS) Fragment.vo Domain.vo TestEncoding.vo
	$(COQC) $(COQFLAGS) $<

validation/spec/SpecGlobals.vo: validation/spec/SpecGlobals.v \
    Fragment.vo Domain.vo TestEncoding.vo
	$(COQC) $(COQFLAGS) $<

validation/spec/SpecHeap.vo: validation/spec/SpecHeap.v \
    $(SPEC_HEAP_SHARD_OBJECTS) Fragment.vo Domain.vo TestEncoding.vo
	$(COQC) $(COQFLAGS) $<

validation/Spec.vo: validation/Spec.v $(SPEC_FACADE_OBJECTS) \
    Fragment.vo Domain.vo TestEncoding.vo | spec-manifest-ready
	$(COQC) $(COQFLAGS) validation/Spec.v

ITREE_TEST_MODULES := $(shell sed -n \
	's/^From ESMetaFV\.validation\.itree Require Import \(.*\)\.$$/\1/p' \
	validation/Tests.v 2>/dev/null)
ITREE_TEST_SOURCES := $(addprefix validation/itree/, \
	$(addsuffix .v,$(ITREE_TEST_MODULES)))
ITREE_TEST_OBJECTS := $(ITREE_TEST_SOURCES:.v=.vo)

validation/Tests.vo: validation/Tests.v Fragment.vo $(ITREE_TEST_OBJECTS)
	$(COQC) $(COQFLAGS) validation/Tests.v

validation/itree/%.vo: validation/itree/%.v Fragment.vo Domain.vo TestEncoding.vo
	$(COQC) $(COQFLAGS) $<

# Small, specification-independent regression for the actual ITree call
# machine.  It covers ordinary calls, print order, and nonlocal ECont
# restore before the multi-megabyte generated Test262 specification is
# rebuilt.
itree_smoke.ml: ITreeSmokeExtract.v ITreeExec.vo Programs.vo
	$(COQC) $(COQFLAGS) ITreeSmokeExtract.v

itree_smoke.mli: itree_smoke.ml
	@test -f $@

itree_smoke.cmi: itree_smoke.mli
	ocamlfind ocamlopt -package zarith,rocq-runtime.kernel \
	  $(OCAML_GENERATED_FLAGS) -c itree_smoke.mli

itree_smoke.cmx: itree_smoke.ml itree_smoke.cmi
	ocamlfind ocamlopt -package zarith,rocq-runtime.kernel \
	  $(OCAML_GENERATED_FLAGS) -c itree_smoke.ml

itree_smoke_driver.cmx: itree_smoke_driver.ml itree_smoke.cmi \
    itree_smoke.cmx
	ocamlfind ocamlopt -package zarith,rocq-runtime.kernel \
	  $(OCAML_GENERATED_FLAGS) -c itree_smoke_driver.ml

itree_smoke: itree_smoke.cmx itree_smoke_driver.cmx
	ocamlfind ocamlopt -package zarith,rocq-runtime.kernel -linkpkg \
	  $(OCAML_GENERATED_FLAGS) itree_smoke.cmx itree_smoke_driver.cmx -o $@

itree-smoke: itree_smoke Int32OpsRegression.vo
	./itree_smoke

extract: itree-core

# Modular extraction keeps the multi-megabyte specification and executable
# ITree semantics stable while Test262 payload shards are regenerated.  The
# native compact-payload worker is the production lane; Rocq tuple modules and
# the bytecode worker remain independent audit/fallback lanes.
ExtractionConfig.vo: ExtractionConfig.v TestEncoding.vo
	$(COQC) $(COQFLAGS) $<

ITreeCore.vo: ITreeCore.v ITreeExec.vo validation/Spec.vo
	$(COQC) $(COQFLAGS) $<

SpecAlgorithmITree.vo: SpecAlgorithmITree.v ITreeExec.vo validation/Spec.vo
	$(COQC) $(COQFLAGS) $<

ITreeCoreRegression.vo: ITreeCoreRegression.v ITreeCore.vo ITreeExec.vo \
    validation/Spec.vo
	$(COQC) $(COQFLAGS) $<

ITreeExecRegression.vo: ITreeExecRegression.v ITreeExec.vo Exec.vo Programs.vo
	$(COQC) $(COQFLAGS) $<

Int32OpsRegression.vo: Int32OpsRegression.v Domain.vo
	$(COQC) $(COQFLAGS) $<

ContEqualityRegression.vo: ContEqualityRegression.v Domain.vo Semantics.vo Exec.vo
	$(COQC) $(COQFLAGS) $<

TypeTrimRegression.vo: TypeTrimRegression.v Domain.vo Exec.vo
	$(COQC) $(COQFLAGS) $<

NumberMathRegression.vo: NumberMathRegression.v Domain.vo Semantics.vo Exec.vo
	$(COQC) $(COQFLAGS) $<

NumberToBigIntRegression.vo: NumberToBigIntRegression.v Domain.vo Exec.vo
	$(COQC) $(COQFLAGS) $<

FloorDivisionNormalizationRegression.vo: FloorDivisionNormalizationRegression.v \
    Domain.vo Exec.vo
	$(COQC) $(COQFLAGS) $<

ExtMathRegression.vo: ExtMathRegression.v Domain.vo Semantics.vo Exec.vo
	$(COQC) $(COQFLAGS) $<

TyExpSetRegression.vo: TyExpSetRegression.v Domain.vo Exec.vo
	$(COQC) $(COQFLAGS) $<

EParseRecoveryRegression.vo: EParseRecoveryRegression.v Domain.vo \
    Semantics.vo Exec.vo
	$(COQC) $(COQFLAGS) $<

SpecAssertionProof.vo: SpecAssertionProof.v Exec.vo validation/Spec.vo
	$(COQC) $(COQFLAGS) $<

JSClosedEquiv.vo: JSClosedEquiv.v ITreeExec.vo validation/Spec.vo
	$(COQC) $(COQFLAGS) $<

# The JavaScript witnesses cross the complete trusted Scala frontend: the
# ECMAScript source is parsed with the extracted grammar, compiled to IR,
# initialized, interpreted, and finally exported.  Hash the conservative input
# closure on every make invocation so additions and deletions are visible too.
# The manifest mtime changes only when its contents do, keeping the JVM cold on
# no-op builds.
$(JS_EQUIV_INPUT_STAMP): FORCE
	@mkdir -p "$(dir $@)"
	@tmp="$@.tmp.$$$$"; \
	  { \
	    find ../src/main/scala/esmeta -type f -name '*.scala' -print0; \
	    find ../src/main/resources -type f -print0; \
	    find ../tests/fv/js-equiv -type f -name '*.js' -print0; \
	    find ../project \
	      \( -type d \( -name target -o -name .bloop \) -prune \) -o \
	      \( -type f \( -name '*.sbt' -o -name 'build.properties' \) \
	        -print0 \); \
	    for file in ../build.sbt ../ecma262/spec.html \
	      ../ecma262/esmeta-ignore.json ../.jvmopts ../.sbtopts; do \
	      test ! -f "$$file" || printf '%s\0' "$$file"; \
	    done; \
	  } | LC_ALL=C sort -zu | xargs -0 shasum -a 256 > "$$tmp"; \
	  if test -f "$@" && cmp -s "$$tmp" "$@"; then \
	    rm -f "$$tmp"; \
	  else \
	    mv "$$tmp" "$@"; \
	    rm -f "$(JS_EQUIV_ARTIFACT_STAMP)"; \
	  fi

# This stamp records that the generator has consumed the current input
# manifest.  It is deliberately separate from JSEquivArtifacts.v because the
# exporter preserves that file's mtime when its generated contents are equal.
$(JS_EQUIV_ARTIFACT_STAMP):
	@mkdir -p "$(dir $@)"
	cd .. && sbt "runMain esmeta.fv.FVInitState --js-equiv"
	@test -f validation/JSEquivArtifacts.v
	@touch $@

# Use separate recursive make evaluations so a manifest change that removes the
# generation stamp is observed immediately; GNU Make otherwise caches the old
# existence check for the remainder of the current evaluation.
js-equiv-freshness:
	@$(MAKE) --no-print-directory $(JS_EQUIV_INPUT_STAMP)
	@$(MAKE) --no-print-directory $(JS_EQUIV_ARTIFACT_STAMP)

# Generation is deliberately staged by [js-equiv-freshness] before the proof
# graph is entered.  Auto-generating this source as one parallel prerequisite
# could rewrite Spec shards while another coqc process is reading them.
validation/JSEquivArtifacts.v:
	@echo "$@ missing — run: make js-equiv-artifacts" && exit 1

validation/JSEquivArtifacts.vo: validation/JSEquivArtifacts.v Fragment.vo \
    Domain.vo TestEncoding.vo
	$(COQC) $(COQFLAGS) $<

JSEquivProof.vo: JSEquivProof.v JSClosedEquiv.vo \
    validation/JSEquivArtifacts.vo $(JS_EQUIV_INPUT_STAMP)
	@test -f "$(JS_EQUIV_ARTIFACT_STAMP)" || { \
	  echo "JavaScript equivalence artifacts are stale — run: make js-equiv"; \
	  exit 2; \
	}
	$(COQC) $(COQFLAGS) $<

js-equiv-artifacts: js-equiv-freshness
	@test -f validation/JSEquivArtifacts.v
	@test -f $(SPEC_SOURCE_MANIFEST)
	@echo "generated fresh split Spec and JavaScript equivalence artifacts"

js-equiv: js-equiv-artifacts
	$(MAKE) JSEquivProof.vo
	@echo "proved ASI-canonicalized JavaScript ITree equivalence"

regressions: ITreeCoreRegression.vo ITreeExecRegression.vo Int32OpsRegression.vo \
	    ContEqualityRegression.vo TypeTrimRegression.vo NumberMathRegression.vo \
	    NumberToBigIntRegression.vo FloorDivisionNormalizationRegression.vo \
	    ExtMathRegression.vo TyExpSetRegression.vo EParseRecoveryRegression.vo \
	    SpecAssertionProof.vo JSClosedEquiv.vo
	$(MAKE) js-equiv
	@echo "compiled semantic regression proofs"

$(ITREE_CORE_BUILD)/.extracted: ExtractCore.v ExtractionConfig.vo ITreeCore.vo \
    SpecAlgorithmITree.vo
	mkdir -p $(ITREE_CORE_BUILD)
	find $(ITREE_CORE_BUILD) -maxdepth 1 -type f -delete
	$(COQC) $(COQFLAGS) ExtractCore.v
	touch $@

$(ITREE_CORE_BUILD)/.compiled: $(ITREE_CORE_BUILD)/.extracted
	cd $(ITREE_CORE_BUILD) && \
	  ocamldep -sort *.mli > .mli-order && \
	  for source in $$(cat .mli-order); do \
	    $(OCAMLFIND_CORE_ENV) ocamlfind ocamlc \
	      -package $(OCAML_EXTRACT_PACKAGES) $(OCAML_GENERATED_FLAGS) \
	      -I . -c "$$source" || exit 1; \
	  done && \
	  ocamldep -sort *.ml > .ml-order && \
	  for source in $$(cat .ml-order); do \
	    $(OCAMLFIND_CORE_ENV) ocamlfind ocamlc \
	      -package $(OCAML_EXTRACT_PACKAGES) $(OCAML_GENERATED_FLAGS) \
	      -I . -c "$$source" || exit 1; \
	  done
	touch $@

# The extracted Spec module is too large for ocamlopt.  Serialize its immutable
# data once with the bytecode core, then replace Spec.ml in an isolated native
# core with a small loader for that snapshot.
spec_snapshot_writer.cmo: spec_snapshot_writer.ml \
    $(ITREE_CORE_BUILD)/.compiled
	$(OCAMLFIND_CORE_ENV) ocamlfind ocamlc \
	  -package $(OCAML_EXTRACT_PACKAGES) $(OCAML_RUNTIME_FLAGS) \
	  -I $(ITREE_CORE_BUILD) -c $<

spec-snapshot-writer: $(ITREE_CORE_BUILD)/.compiled \
    spec_snapshot_writer.cmo
	core_cmos=$$(sed 's/\.ml/.cmo/g' $(ITREE_CORE_BUILD)/.ml-order); \
	$(OCAMLFIND_CORE_ENV) ocamlfind ocamlc \
	  -package $(OCAML_EXTRACT_PACKAGES) -linkpkg $(OCAML_GENERATED_FLAGS) \
	  -I $(ITREE_CORE_BUILD) \
	  $$(for file in $$core_cmos; do printf '%s/%s ' \
	      $(ITREE_CORE_BUILD) "$$file"; done) \
	  spec_snapshot_writer.cmo -o $@

$(ITREE_SPEC_SNAPSHOT): spec-snapshot-writer validation/Spec.v \
    $(ITREE_CORE_BUILD)/.compiled
	mkdir -p $(dir $@)
	@tmp="$@.tmp.$$$$"; \
	  trap 'rm -f "$$tmp"' EXIT; \
	  ./spec-snapshot-writer "$$tmp"; \
	  mv "$$tmp" "$@"; \
	  trap - EXIT

$(ITREE_NATIVE_CORE_BUILD)/.prepared: prepare-native-core.py \
    $(ITREE_CORE_BUILD)/.extracted
	python3 ./prepare-native-core.py
	@test -f $@

$(ITREE_NATIVE_CORE_BUILD)/.compiled: \
    $(ITREE_NATIVE_CORE_BUILD)/.prepared
	cd $(ITREE_NATIVE_CORE_BUILD) && \
	  ocamldep -sort *.mli > .mli-order && \
	  for source in $$(cat .mli-order); do \
	    $(OCAMLFIND_CORE_ENV) ocamlfind ocamlopt \
	      -package $(OCAML_EXTRACT_PACKAGES) $(OCAML_GENERATED_FLAGS) \
	      -I . -c "$$source" || exit 1; \
	  done && \
	  ocamldep -sort *.ml > .ml-order && \
	  for source in $$(cat .ml-order); do \
	    $(OCAMLFIND_CORE_ENV) ocamlfind ocamlopt \
	      -package $(OCAML_EXTRACT_PACKAGES) $(OCAML_GENERATED_FLAGS) \
	      -I . -c "$$source" || exit 1; \
	  done
	touch $@

$(ITREE_NATIVE_WORKER_BUILD)/.prepared: itree_test_runtime.ml \
    payload_codec.ml payload_worker.ml
	mkdir -p $(ITREE_NATIVE_WORKER_BUILD)
	find $(ITREE_NATIVE_WORKER_BUILD) -maxdepth 1 -type f -delete
	cp itree_test_runtime.ml payload_codec.ml payload_worker.ml \
	  $(ITREE_NATIVE_WORKER_BUILD)/
	touch $@

$(ITREE_NATIVE_WORKER_BUILD)/.compiled: \
    $(ITREE_NATIVE_WORKER_BUILD)/.prepared \
    $(ITREE_NATIVE_CORE_BUILD)/.compiled
	cd $(ITREE_NATIVE_WORKER_BUILD) && \
	  $(OCAMLFIND_CORE_ENV) ocamlfind ocamlopt \
	    -package $(OCAML_EXTRACT_PACKAGES) $(OCAML_RUNTIME_FLAGS) \
	    -I $(abspath $(ITREE_NATIVE_CORE_BUILD)) \
	    -c itree_test_runtime.ml && \
	  $(OCAMLFIND_CORE_ENV) ocamlfind ocamlopt \
	    -package $(OCAML_EXTRACT_PACKAGES) $(OCAML_RUNTIME_FLAGS) \
	    -I $(abspath $(ITREE_NATIVE_CORE_BUILD)) \
	    -c payload_codec.ml && \
	  $(OCAMLFIND_CORE_ENV) ocamlfind ocamlopt \
	    -package $(OCAML_EXTRACT_PACKAGES) $(OCAML_RUNTIME_FLAGS) \
	    -I $(abspath $(ITREE_NATIVE_CORE_BUILD)) \
	    -c payload_worker.ml
	touch $@

fvitree-worker-native: $(ITREE_SPEC_SNAPSHOT) \
    $(ITREE_NATIVE_CORE_BUILD)/.compiled \
    $(ITREE_NATIVE_WORKER_BUILD)/.compiled
	cd $(ITREE_NATIVE_WORKER_BUILD) && \
	  core_cmxs=$$(sed 's/\.ml/.cmx/g' \
	    $(abspath $(ITREE_NATIVE_CORE_BUILD))/.ml-order); \
	  $(OCAMLFIND_CORE_ENV) ocamlfind ocamlopt \
	    -package $(OCAML_EXTRACT_PACKAGES) -linkpkg $(OCAML_GENERATED_FLAGS) \
	    -I $(abspath $(ITREE_NATIVE_CORE_BUILD)) -I . \
	    $$(for file in $$core_cmxs; do printf '%s/%s ' \
	        $(abspath $(ITREE_NATIVE_CORE_BUILD)) "$$file"; done) \
	    itree_test_runtime.cmx payload_codec.cmx payload_worker.cmx \
	    -o $(abspath $@)

itree-native-core: $(ITREE_NATIVE_CORE_BUILD)/.compiled
	@echo "built snapshot-backed native core in $(ITREE_NATIVE_CORE_BUILD)"

itree-spec-snapshot: $(ITREE_SPEC_SNAPSHOT)
	@echo "built specification snapshot $(ITREE_SPEC_SNAPSHOT)"

itree-worker-native: fvitree-worker-native
	@echo "built ./fvitree-worker-native"

ITREE_TEST_EXTRACT_DRIVERS := $(addprefix $(ITREE_SHARD_DRIVERS)/Extract_, \
	$(addsuffix .v,$(ITREE_TEST_MODULES)))
ITREE_TEST_EXTRACT_MLS := $(addprefix $(ITREE_SHARD_BUILD)/, \
	$(addsuffix .ml,$(ITREE_TEST_MODULES)))
ITREE_TEST_EXTRACT_MLIS := $(ITREE_TEST_EXTRACT_MLS:.ml=.mli)
ITREE_TEST_EXTRACT_CMIS := $(ITREE_TEST_EXTRACT_MLS:.ml=.cmi)
ITREE_TEST_EXTRACT_CMOS := $(ITREE_TEST_EXTRACT_MLS:.ml=.cmo)
ITREE_TESTS_EXTRACT_ML := $(ITREE_SHARD_BUILD)/Tests.ml
ITREE_TESTS_EXTRACT_MLI := $(ITREE_SHARD_BUILD)/Tests.mli
ITREE_TESTS_EXTRACT_CMI := $(ITREE_SHARD_BUILD)/Tests.cmi
ITREE_TESTS_EXTRACT_CMO := $(ITREE_SHARD_BUILD)/Tests.cmo
ITREE_TESTS_AGGREGATE_ML := $(ITREE_SHARD_DRIVERS)/Tests.ml

$(ITREE_SHARD_DRIVERS)/.generated: validation/Tests.v gen-extract-shard.sh
	./gen-extract-shard.sh $< validation/ExtractShard.v \
	  $(ITREE_SHARD_DRIVERS)
	touch $@

validation/ExtractShard.v $(ITREE_TEST_EXTRACT_DRIVERS) \
    $(ITREE_SHARD_DRIVERS)/Extract_Tests.v $(ITREE_TESTS_AGGREGATE_ML): \
    $(ITREE_SHARD_DRIVERS)/.generated
	@test -f $@

# Cleaning is serialized before any per-module extraction begins.  A changed
# Tests.v invalidates this stamp, while a no-op build keeps every shard output.
$(ITREE_SHARD_BUILD)/.prepared: $(ITREE_SHARD_DRIVERS)/.generated \
    $(ITREE_TESTS_AGGREGATE_ML)
	mkdir -p $(ITREE_SHARD_BUILD)
	find $(ITREE_SHARD_BUILD) -maxdepth 1 -type f -delete
	cp $(ITREE_TESTS_AGGREGATE_ML) $(ITREE_TESTS_EXTRACT_ML)
	touch $@

$(ITREE_TEST_EXTRACT_MLS): $(ITREE_SHARD_BUILD)/%.ml: \
    validation/itree/%.vo $(ITREE_SHARD_DRIVERS)/Extract_%.v \
    ExtractionConfig.vo ITreeCore.vo | $(ITREE_SHARD_BUILD)/.prepared
	$(COQC) $(COQFLAGS) $(ITREE_SHARD_DRIVERS)/Extract_$*.v
	@test -f $@ && test -f $(@:.ml=.mli)

$(ITREE_TESTS_EXTRACT_ML): $(ITREE_TESTS_AGGREGATE_ML) \
    | $(ITREE_SHARD_BUILD)/.prepared
	cp $< $@

$(ITREE_TEST_EXTRACT_MLIS): %.mli: %.ml
	@test -f $@

$(ITREE_TESTS_EXTRACT_MLI): $(ITREE_TESTS_EXTRACT_ML) \
    $(ITREE_TEST_EXTRACT_CMIS) $(ITREE_CORE_BUILD)/.compiled \
    | $(ITREE_SHARD_BUILD)/.prepared
	$(OCAMLFIND_CORE_ENV) ocamlfind ocamlc \
	  -package $(OCAML_EXTRACT_PACKAGES) $(OCAML_GENERATED_FLAGS) \
	  -I $(ITREE_CORE_BUILD) -I $(ITREE_SHARD_BUILD) \
	  -i $< > $@.tmp
	mv $@.tmp $@

$(ITREE_TEST_EXTRACT_CMIS): $(ITREE_SHARD_BUILD)/%.cmi: \
    $(ITREE_SHARD_BUILD)/%.mli $(ITREE_CORE_BUILD)/.compiled \
    | $(ITREE_SHARD_BUILD)/.prepared
	$(OCAMLFIND_CORE_ENV) ocamlfind ocamlc \
	  -package $(OCAML_EXTRACT_PACKAGES) $(OCAML_GENERATED_FLAGS) \
	  -I $(ITREE_CORE_BUILD) -I $(ITREE_SHARD_BUILD) -c $<

$(ITREE_TESTS_EXTRACT_CMI): $(ITREE_TESTS_EXTRACT_MLI) \
    $(ITREE_TEST_EXTRACT_CMIS) $(ITREE_CORE_BUILD)/.compiled \
    | $(ITREE_SHARD_BUILD)/.prepared
	$(OCAMLFIND_CORE_ENV) ocamlfind ocamlc \
	  -package $(OCAML_EXTRACT_PACKAGES) $(OCAML_GENERATED_FLAGS) \
	  -I $(ITREE_CORE_BUILD) -I $(ITREE_SHARD_BUILD) -c $<

$(ITREE_TEST_EXTRACT_CMOS): $(ITREE_SHARD_BUILD)/%.cmo: \
    $(ITREE_SHARD_BUILD)/%.ml $(ITREE_SHARD_BUILD)/%.cmi \
    | $(ITREE_SHARD_BUILD)/.prepared
	$(OCAMLFIND_CORE_ENV) ocamlfind ocamlc \
	  -package $(OCAML_EXTRACT_PACKAGES) $(OCAML_GENERATED_FLAGS) \
	  -I $(ITREE_CORE_BUILD) -I $(ITREE_SHARD_BUILD) -c $<

$(ITREE_TESTS_EXTRACT_CMO): $(ITREE_TESTS_EXTRACT_ML) \
    $(ITREE_TESTS_EXTRACT_CMI) $(ITREE_TEST_EXTRACT_CMOS) \
    | $(ITREE_SHARD_BUILD)/.prepared
	$(OCAMLFIND_CORE_ENV) ocamlfind ocamlc \
	  -package $(OCAML_EXTRACT_PACKAGES) $(OCAML_GENERATED_FLAGS) \
	  -I $(ITREE_CORE_BUILD) -I $(ITREE_SHARD_BUILD) -c $<

$(ITREE_SHARD_BUILD)/.compiled: $(ITREE_TEST_EXTRACT_CMOS) \
    $(ITREE_TESTS_EXTRACT_CMO) | $(ITREE_SHARD_BUILD)/.prepared
	{ for module in $(ITREE_TEST_MODULES); do \
	    printf '%s.mli\n' "$$module"; \
	  done; \
	  printf 'Tests.mli\n'; \
	} > $(ITREE_SHARD_BUILD)/.mli-order
	{ for module in $(ITREE_TEST_MODULES); do \
	    printf '%s.ml\n' "$$module"; \
	  done; \
	  printf 'Tests.ml\n'; \
	} > $(ITREE_SHARD_BUILD)/.ml-order
	touch $@

itree_test_runtime.cmo: itree_test_runtime.ml $(ITREE_CORE_BUILD)/.compiled
	$(OCAMLFIND_CORE_ENV) ocamlfind ocamlc \
	  -package $(OCAML_EXTRACT_PACKAGES) $(OCAML_RUNTIME_FLAGS) \
	  -I $(ITREE_CORE_BUILD) -c $<

spec_algorithm_itree_driver.cmo: spec_algorithm_itree_driver.ml \
    $(ITREE_CORE_BUILD)/.compiled
	$(OCAMLFIND_CORE_ENV) ocamlfind ocamlc \
	  -package $(OCAML_EXTRACT_PACKAGES) $(OCAML_GENERATED_FLAGS) \
	  -I $(ITREE_CORE_BUILD) -c $<

fvitree-spec-algorithm: $(ITREE_CORE_BUILD)/.compiled \
    spec_algorithm_itree_driver.cmo
	core_cmos=$$(sed 's/\.ml/.cmo/g' $(ITREE_CORE_BUILD)/.ml-order); \
	$(OCAMLFIND_CORE_ENV) ocamlfind ocamlc \
	  -package $(OCAML_EXTRACT_PACKAGES) -linkpkg $(OCAML_GENERATED_FLAGS) \
	  -I $(ITREE_CORE_BUILD) \
	  $$(for file in $$core_cmos; do printf '%s/%s ' \
	      $(ITREE_CORE_BUILD) "$$file"; done) \
	  spec_algorithm_itree_driver.cmo -o $@

itree-spec-algorithm: fvitree-spec-algorithm
	./fvitree-spec-algorithm $(ITREE_ALGORITHM_LINES) \
	  logs/itree-spec-IsCallable.log

modular_driver.cmo: modular_driver.ml itree_test_runtime.cmo \
    $(ITREE_CORE_BUILD)/.compiled \
    $(ITREE_SHARD_BUILD)/.compiled
	$(OCAMLFIND_CORE_ENV) ocamlfind ocamlc \
	  -package $(OCAML_EXTRACT_PACKAGES) $(OCAML_GENERATED_FLAGS) \
	  -I $(ITREE_CORE_BUILD) -I $(ITREE_SHARD_BUILD) \
	  -c modular_driver.ml

fvitree-modular: $(ITREE_CORE_BUILD)/.compiled \
    $(ITREE_SHARD_BUILD)/.compiled modular_driver.cmo
	core_cmos=$$(sed 's/\.ml/.cmo/g' $(ITREE_CORE_BUILD)/.ml-order); \
	shard_cmos=$$(sed 's/\.ml/.cmo/g' $(ITREE_SHARD_BUILD)/.ml-order); \
	$(OCAMLFIND_CORE_ENV) ocamlfind ocamlc \
	  -package $(OCAML_EXTRACT_PACKAGES) -linkpkg $(OCAML_GENERATED_FLAGS) \
	  -I $(ITREE_CORE_BUILD) -I $(ITREE_SHARD_BUILD) \
	  $$(for file in $$core_cmos; do printf '%s/%s ' \
	      $(ITREE_CORE_BUILD) "$$file"; done) \
	  itree_test_runtime.cmo \
	  $$(for file in $$shard_cmos; do printf '%s/%s ' \
	      $(ITREE_SHARD_BUILD) "$$file"; done) \
	  modular_driver.cmo -o $@

payload_codec.cmo: payload_codec.ml $(ITREE_CORE_BUILD)/.compiled
	$(OCAMLFIND_CORE_ENV) ocamlfind ocamlc \
	  -package $(OCAML_EXTRACT_PACKAGES) $(OCAML_RUNTIME_FLAGS) \
	  -I $(ITREE_CORE_BUILD) -c $<

payload_worker.cmo: payload_worker.ml payload_codec.cmo \
    itree_test_runtime.cmo $(ITREE_CORE_BUILD)/.compiled
	$(OCAMLFIND_CORE_ENV) ocamlfind ocamlc \
	  -package $(OCAML_EXTRACT_PACKAGES) $(OCAML_RUNTIME_FLAGS) \
	  -I $(ITREE_CORE_BUILD) -c $<

fvitree-worker: $(ITREE_CORE_BUILD)/.compiled itree_test_runtime.cmo \
    payload_codec.cmo payload_worker.cmo
	core_cmos=$$(sed 's/\.ml/.cmo/g' $(ITREE_CORE_BUILD)/.ml-order); \
	$(OCAMLFIND_CORE_ENV) ocamlfind ocamlc \
	  -package $(OCAML_EXTRACT_PACKAGES) -linkpkg $(OCAML_GENERATED_FLAGS) \
	  -I $(ITREE_CORE_BUILD) \
	  $$(for file in $$core_cmos; do printf '%s/%s ' \
	      $(ITREE_CORE_BUILD) "$$file"; done) \
	  itree_test_runtime.cmo payload_codec.cmo payload_worker.cmo -o $@

payload_roundtrip.cmo: payload_roundtrip.ml payload_codec.cmo \
    $(ITREE_SHARD_BUILD)/.compiled
	$(OCAMLFIND_CORE_ENV) ocamlfind ocamlc \
	  -package $(OCAML_EXTRACT_PACKAGES) $(OCAML_GENERATED_FLAGS) \
	  -I $(ITREE_CORE_BUILD) -I $(ITREE_SHARD_BUILD) -c $<

payload-roundtrip: $(ITREE_CORE_BUILD)/.compiled \
    $(ITREE_SHARD_BUILD)/.compiled payload_codec.cmo payload_roundtrip.cmo
	core_cmos=$$(sed 's/\.ml/.cmo/g' $(ITREE_CORE_BUILD)/.ml-order); \
	shard_cmos=$$(sed 's/\.ml/.cmo/g' $(ITREE_SHARD_BUILD)/.ml-order); \
	$(OCAMLFIND_CORE_ENV) ocamlfind ocamlc \
	  -package $(OCAML_EXTRACT_PACKAGES) -linkpkg $(OCAML_GENERATED_FLAGS) \
	  -I $(ITREE_CORE_BUILD) -I $(ITREE_SHARD_BUILD) \
	  $$(for file in $$core_cmos; do printf '%s/%s ' \
	      $(ITREE_CORE_BUILD) "$$file"; done) \
	  $$(for file in $$shard_cmos; do printf '%s/%s ' \
	      $(ITREE_SHARD_BUILD) "$$file"; done) \
	  payload_codec.cmo payload_roundtrip.cmo -o $@

itree-core: $(ITREE_CORE_BUILD)/.compiled ITreeCoreRegression.vo
	@echo "built reusable ITree core in $(ITREE_CORE_BUILD)"

itree-shard:
	python3 ./with-itree-build-lock.py $(ITREE_JOBS) fvitree-modular
	@echo "built ./fvitree-modular from the current Test262 shard"

itree-t262-modular: itree-shard
	./fvitree-modular $(ITREE_FUEL)

t262-full:
	./run-test262-full.py $(T262_FULL_ARGS)

t262-full-dry:
	./run-test262-full.py --dry-run --smoke $(T262_FULL_ARGS)

validation/Tests.v:
	@echo "validation/Tests.v missing — run: sbt \"runMain esmeta.fv.FVInitState --test262 20\"" && exit 1

%.vo: %.v
	$(COQC) $(COQFLAGS) $<

$(DEPS): $(SOURCES)
	$(COQDEP) $(COQDEPFLAGS) $(SOURCES) > $@

-include $(DEPS)

# Build cleanup intentionally preserves campaign evidence under [logs].
# Use [purge-runs CONFIRM=YES] only when archived Test262 results are no
# longer needed.
clean-build:
	rm -f fvrun fvitree itree_smoke *.cm* *.o
	rm -f *.vo *.vos *.vok *.glob $(DEPS)
	rm -f .*.aux *.aux
	rm -rf build/itree
	rm -f fvitree-modular modular_driver.cmo modular_driver.cmi
	rm -f itree_test_runtime.cmo itree_test_runtime.cmi
	rm -f fvitree-spec-algorithm
	rm -f spec_algorithm_itree_driver.cmo spec_algorithm_itree_driver.cmi
	rm -f fvitree-worker payload_worker.cmo payload_worker.cmi
	rm -f fvitree-worker-native spec-snapshot-writer
	rm -f payload-roundtrip payload_codec.cmo payload_codec.cmi
	rm -f payload_roundtrip.cmo payload_roundtrip.cmi
	rm -f ExtractionConfig.vo ExtractionConfig.vos ExtractionConfig.vok
	rm -f ITreeCore.vo ITreeCore.vos ITreeCore.vok
	rm -f ExtractCore.vo ExtractCore.vos ExtractCore.vok
	rm -f validation/ExtractShard.v validation/ExtractShard.vo
	rm -f validation/ExtractShard.vos validation/ExtractShard.vok
	rm -f validation/*.vo validation/*.vos validation/*.vok validation/*.glob
	rm -f validation/.*.aux
	rm -f validation/spec/*.vo validation/spec/*.vos
	rm -f validation/spec/*.vok validation/spec/*.glob
	rm -f validation/spec/.*.aux
	rm -f validation/itree/*.vo validation/itree/*.vos
	rm -f validation/itree/*.vok validation/itree/*.glob
	rm -f validation/itree/.*.aux

# Remove only regenerable extraction/shard outputs.  Keep Spec.v, the Test262
# inventory/base stamp, build caches, and all campaign logs for fast resume and
# reproducible audit.
clean-generated:
	rm -f fvrun.ml fvrun.mli fvitree.ml fvitree.mli
	rm -f itree_smoke.ml itree_smoke.mli
	rm -f fvrun fvitree fvitree-native fvitree.cmi fvitree.cmo fvitree.cmx
	rm -f driver.cmi driver.cmo driver.cmx itree_smoke itree_smoke.cmi
	rm -f itree_smoke.cmo itree_smoke.cmx itree_smoke_driver.cmi
	rm -f itree_smoke_driver.cmo itree_smoke_driver.cmx parse_probe
	rm -f parse_probe.cmi parse_probe.cmo parse_probe.cmx parse_probe.o
	rm -rf extracted validation/itree validation/t262 validation/payload
	rm -f validation/ExtractShard.v validation/Tests.v
	rm -f validation/test262-shard.tsv
	rm -f $(JS_EQUIV_INPUT_STAMP) $(JS_EQUIV_ARTIFACT_STAMP)
	rm -f ToStringAssertionScratch.vo ToStringAssertionScratch.vos
	rm -f ToStringAssertionScratch.vok ToStringAssertionScratch.glob
	rm -f .ToStringAssertionScratch.aux

clean: clean-build clean-generated

purge-runs:
	@test "$(CONFIRM)" = YES || { \
	  echo "refusing to delete campaign evidence; rerun with CONFIRM=YES"; \
	  exit 2; \
	}
	rm -rf logs

.PHONY: FORCE all check validate spec extract itree-smoke regressions js-equiv-freshness \
	itree-core itree-shard \
	itree-t262-modular itree-spec-algorithm fvitree-worker payload-roundtrip \
	itree-native-core \
	itree-spec-snapshot itree-worker-native js-equiv-artifacts js-equiv t262-full \
	t262-full-dry clean clean-build clean-generated purge-runs

FORCE:

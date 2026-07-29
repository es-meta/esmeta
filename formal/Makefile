# ESMetaFV — Rocq development for IR-Core transpiler verification.
#
# Requires the `cris-workshop` opam switch (Rocq 9.0.0 + pinned rocq-cris);
# see README.md in this directory for one-time setup.
#
# Build:  make        (from this directory)
# Clean:  make clean

COQC ?= coqc
COQDEP ?= coqdep

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

SOURCES := \
	Fragment.v \
	Domain.v \
	Events.v \
	Semantics.v \
	Programs.v \
	Examples.v \
	Exec.v \
	Transform.v \
	Validation.v \
	T1Proof.v \
	T2Proof.v \
	T3Proof.v
OBJECTS := $(SOURCES:.v=.vo)
DEPS := .deps.mk

all: $(OBJECTS)

# Differential validation (PO-011): compile the generated expectations.
# Regenerate with:  sbt "runMain esmeta.fv.FVExport"   (from the repo root)
validate: all validation/Generated.v
	$(COQC) $(COQFLAGS) validation/Generated.v

validation/Generated.v:
	@echo "validation/Generated.v missing — run: sbt \"runMain esmeta.fv.FVExport\"" && exit 1

%.vo: %.v
	$(COQC) $(COQFLAGS) $<

$(DEPS): $(SOURCES)
	$(COQDEP) $(COQDEPFLAGS) $(SOURCES) > $@

-include $(DEPS)

clean:
	rm -f *.vo *.vos *.vok *.glob $(DEPS)
	rm -f .*.aux *.aux
	rm -f validation/*.vo validation/*.vos validation/*.vok validation/*.glob
	rm -f validation/.*.aux

.PHONY: all validate clean

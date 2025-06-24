# Makefile for Verilisp
#
# Copyright (C) 2023--2025 Simon Dobson
#
# This file is part of verilisp, a very Lisp approach to hardware synthesis
#
# verilisp is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# verilisp is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

# The version we're building
VERSION = $(shell cat version.sexp)


# ----- Sources -----

SOURCES_ASDF = verilisp.asd

SOURCES = $(shell find src -name '*.lisp' -print)
SOURCES_GENERATED =
SOURCES_GENERATED_EXTRA = TAGS

OBJECTS = $(shell find src -name '*.fasl' -print)

SOURCES_DOC =  $(shell ls doc/*.rst)
SOURCES_DOC_CONF = doc/conf.py
SOURCES_DOC_BUILD_DIR = doc/_build

BINARIES = verilispc
BINARIES_BUILD_DIR = bin

CONTAINERS = \
	lib/lattice \
	lib/risc-v


# ----- Tools -----

# Base commands
LISP = sbcl
LISPOPTS = --core ~/sbcl-dev.core
PYTHON = python3
PIP = pip
VIRTUALENV = $(PYTHON) -m venv
ACTIVATE = . $(VENV)/bin/activate
ETAGS = etags
CHDIR = cd
RM = rm -fr
MKDIR = mkdir -p

# Files that are locally changed vs the remote repo
# (See https://unix.stackexchange.com/questions/155046/determine-if-git-working-directory-is-clean-from-a-script)
GIT_DIRTY = $(shell $(GIT) status --untracked-files=no --porcelain)

# The git branch we're currently working on
GIT_BRANCH = $(shell $(GIT) rev-parse --abbrev-ref HEAD 2>/dev/null)

# Root directory
ROOT = $(shell pwd)

# Requirements for the documentation venv
VENV = venv3
REQUIREMENTS = requirements.txt

 # Constructed commands
RUN_SPHINX_HTML = $(CHDIR) doc && make html


# ----- Top-level targets -----

# Default prints a help message
help:
	@make usage

# Run test suite
.PHONY: test
test:
	$(LISP) $(LISPOPTS) \
	--eval "(push (truename #p\"$(ROOT)\") asdf:*central-registry*)" \
	--eval "(asdf:test-system :verilisp)" \
	--eval "(quit)"

# Build the output binaries
.PHONY: bin
bin:
	$(MKDIR) $(BINARIES_BUILD_DIR)
	$(LISP) $(LISPOPTS) \
	--eval "(push (truename #p\"$(ROOT)\") asdf:*central-registry*)" \
	--eval "(asdf:make :verilisp/cli)" \
	--eval "(quit)"

# Build the API documentation using Sphinx
doc: env $(SOURCES_ASDF) $(SOURCES_DOC) $(SOURCES_DOC_CONF)
	$(ACTIVATE) && $(RUN_SPHINX_HTML)

# Build tool containers
containers:
	for c in $(CONTAINERS); do (cd $$c && make); done

# Build a documentation Python venv
env: $(VENV)

$(VENV):
	$(VIRTUALENV) $(VENV)
	$(ACTIVATE) && $(PIP) install -U pip wheel && $(PIP) install -r requirements.txt

# Make a new release
release: $(SOURCES_GENERATED) main-only commit upload

# Check we're on the main branch before uploading
main-only:
	if [ "$(GIT_BRANCH)" != "main" ]; then echo "Can only release from main branch"; exit 1; fi

# Update the remote repos on release
# (This will trigger .github/workflows/release.yaml to create a GitHib release, and
# .github/workflows/ci.yaml to run the full integration test suite)
commit: check-local-repo-clean
	$(GIT) push origin main
	$(GIT) tag -a v$(VERSION) -m "Version $(VERSION)"
	$(GIT) push origin v$(VERSION)

.SILENT: check-local-repo-clean
check-local-repo-clean:
	if [ "$(GIT_DIRTY)" ]; then echo "Uncommitted files: $(GIT_DIRTY)"; exit 1; fi

# Clean up the distribution build and documentation
clean:
	$(RM) $(SOURCES_GENERATED) $(SOURCES_GENERATED_EXTRA) $(SOURCES_DOC_BUILD_DIR) $(OBJECTS) $(BINARIES)

# Clean up everything, including the documentation venv
reallyclean: clean
	$(RM) $(VENV)


# ----- Generated files -----

# The tags file
TAGS:
	$(ETAGS) -o TAGS $(SOURCES)


# ----- Usage -----

define HELP_MESSAGE
Available targets:
   make bin          build the binaries
   make containers   build the tool containers
   make doc          build the API documentation using Sphinx
   make release      make a release
   make clean        clean-up the build
   make reallyclean  clean up the virtualenv as well

endef
export HELP_MESSAGE

usage:
	@echo "$$HELP_MESSAGE"

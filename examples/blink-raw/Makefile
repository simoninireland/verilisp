# Makefile for blink in Verilisp
#
# Copyright (C) 2024--2026, Simon Dobson
#
# This is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this software. If not, see <http://www.gnu.org/licenses/gpl.html>.

VERILISP_ROOT = ../..


# ---------- Target code ----------

# Bitstream
TARGET = blink.bin
#TOPMODULE =

# Sources
SOURCES = blink.lisp
CONFIG = blink.pcf
#CONFIG = blink.xdf
OBJECT = blink.v


# ---------- Tools ----------

include $(VERILISP_ROOT)/examples/icestick.mk
#include $(VERILISP_ROOT)/examples/digilent-cmod-a7.mk


# ---------- Top-level targets ----------

# Build the target bitstream
$(TARGET): $(OBJECT) # $(CONFIG)

$(OBJECT): $(SOURCES)


# Print a help message
help:
	@make usage


# ----- Usage -----

define HELP_MESSAGE
Available targets:
   make              make the target
   make upload       upload the bitstream to the FPGA
   make clean        clean-up the build directory

endef
export HELP_MESSAGE

usage:
	@echo "$$HELP_MESSAGE"

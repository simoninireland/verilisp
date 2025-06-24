# Makefile includes for RISC-V assembly and compilation using the gcc toolchain
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

# ---------- Target ----------

# Base address for code to be located
BASE_ADDRESS = 0x000000

# Docker container tag
# Leave this blank to run the toolchain native; provide a tag to run in a container
RISCV_CONTAINER_TAG = risc-v


# ---------- Tools ----------

# Tools
AS = $(CROSS_COMPILER_PREFIX)-as
LD = $(CROSS_COMPILER_PREFIX)-ld
GCC =$(CROSS_COMPILER_PREFIX)-gcc
OBJDUMP = $(CROSS_COMPILER_PREFIX)-objdump
GREP = grep
BASH = bash
SED = sed
AWK = awk
DD = dd
OD = od

# Tool options
AS_OPTS = -march=rv32i -mabi=ilp32 -mno-relax -fPIC
LD_OPTS = -T bram.ld -m elf32lriscv -nostdlib
GCC_OPTS = -nostdlib $(AS_OPTS) -static -Wl,--section-start=.text=$(BASE_ADDRESS)

# Command to run tools in a container (if requested)
ifneq ($(RISCV_CONTAINER_TAG),)
RISCV_IN_CONTAINER = $(DOCKER) run -it --rm --mount type=bind,source=`pwd`,target=/work $(RISCV_CONTAINER_TAG)
CROSS_COMPILER_PREFIX=riscv-none-elf
else
# Change to reflect the prefix of the locally-installed toolchain
CROSS_COMPILER_PREFIX=riscv64-linux-gnu
endif


# ---------- Implicit rules ----------

.SUFFIXES: .s .o .hex

.s.o:
	$(RISCV_IN_CONTAINER) $(GCC) $(GCC_OPTS) $*.s -o $*.o

.o.hex:
	$(RISCV_IN_CONTAINER) $(OBJDUMP) -h $*.o | \
	$(GREP) .text |\
	$(SED) -e 's|\s\+|\t|g' -e 's|^\s\+||g'| \
	$(AWK) '{print "$(DD) if=$*.o bs=1 count=$$((0x"$$3")) skip=$$((0x"$$6")) status=none";}' | \
	$(BASH) | \
	$(OD) -Anone -tx4 >$*.hex

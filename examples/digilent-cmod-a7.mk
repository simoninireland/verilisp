# Makefile includes for Digilent CMOD A7 FPGA programming with the OpenXC7 toolchain
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

# ---------- Device ----------

# Target device
FAMILY  = artix7
PART    = xc7a35tcpg236-1
BOARD   = cmoda7_35t

# Docker container tag
# Leave this blank to run the toolchain native; provide a tag to run in a container
#OPENXC7_CONTAINER_TAG = openxc7-docker


# ---------- Tools ----------

# Tools
VERILISPC = $(VERILISP_ROOT)/bin/verilispc
SYNTH = yosys
PNR = nextpnr-xilinx
BBASM = bbasm
FRAME = fasm2frames
PACK = xc7frames2bit
PROGRAM = openFPGALoader
PYPY3 = pypy3
DOCKER = docker
RM = rm -fr
CHDIR = cd
NIX = nix
MKDIR = mkdir -p
FIND = find

# Tool options
VERILISPC_OPTS = -e elaborated.lisp --debug -W all --verbose
SYNTH_OPTS =
PNR_OPTS =
BBASM_OPTS =
FRAME_OPTS =
PACK_OPTS =
PROGRAM_OPTS =

# Top module defaults to the stem of the name of the target bitstream
TARGET_BASENAME = $(shell basename $(TARGET) .bin)
ifeq ($(TOPMODULE),)
TOPMODULE = $(TARGET_BASENAME)
endif

# Generated files
FPGA_STEMS = $(foreach fn,$(SOURCES), $(shell basename $(fn) .lisp))
GENERATED += $(foreach stem,$(FPGA_STEMS),$(stem).v $(stem).fasm $(stem).bin $(stem).json)

# Command to run tools in a container (if requested)
# (Container must run privileged to be able to upload to the device.)
ifneq ($(OPENXC7_CONTAINER_TAG),)
OPENXC7_IN_CONTAINER = $(DOCKER) run -it --rm --privileged --mount type=bind,source=`pwd`,target=/work $(OPENXC7_CONTAINER_TAG)
endif

# Chip databases
# These are populated within the container already; they need to be
# built for bare metal
ifneq ($(OPENXC7_CONTAINER_TAG),)
CHIP_DB = $(shell ls -d /nix/store/*-nextpnr-xilinx-chipdb*)
XRAY_DB = $(shell ls -d /nix/store/*-nextpnr-xilinx-[0-9]*/share/nextpnr/external/prjxray-db)/${FAMILY}
else
CHIP_DB = $(VERILISP_ROOT)/lib/chipdb
$(shell if [ ! -d $(CHIP_DB) ]; then $(MKDIR) $(CHIP_DB); fi)
NEXTPNR_DIR = $(shell echo $$NEXTPNR_XILINX_DIR)
NEXTPNR_PYTHON_DIR = $(NEXTPNR_DIR)/share/nextpnr/python
XRAY_DB = $(NEXTPNR_DIR)/share/nextpnr/external/prjxray-db/${FAMILY}
endif
DBPART = $(shell echo ${PART} | sed -e 's/-[0-9]//g')
FPGA_DB = $(shell $(FIND) $(CHIP_DB) -name $(DBPART).bin -print)

# JTAG link
JTAG_LINK ?= --board $(BOARD)


# ---------- Explicit targets ----------

# Make the target bitstream
target: ${CHIP_DB}/${DBPART}.bin $(TARGET)


# Upload the target bitstream to the device
upload: $(TARGET)
	$(OPENXC7_IN_CONTAINER) $(PROGRAM) $(PROGRAM_OPTS) ${JTAG_LINK} --bitstream $(TARGET)


# Clean the build
clean:
	$(RM) $(GENERATED)


# Clean the databases too
reallyclean: clean
	$(RM) $(CHIP_DB)


# ---------- Implicit rules ----------

.SUFFIXES: .v .xdf .json .fasm .frames .bin

%.v: $(SOURCES) $(CONFIG)
	$(VERILISPC) $(VERILISPC_OPTS) -o $*.v $(SOURCES)

.v.json:
	$(OPENXC7_IN_CONTAINER) $(SYNTH) $(SYNTH_OPTS) -p "synth_xilinx -flatten -abc9 ${SYNTH_OPTS} -arch xc7 -top ${TOPMODULE}; write_json $*.json" $< ${ADDITIONAL_SOURCES}

${CHIP_DB}/${DBPART}.bin:
	$(OPENXC7_IN_CONTAINER) ${PYPY3} ${NEXTPNR_PYTHON_DIR}/bbaexport.py --device ${PART} --bba ${DBPART}.bba && bbasm -l ${DBPART}.bba ${CHIP_DB}/${DBPART}.bin && rm -f ${DBPART}.bba

.json.fasm:
	$(OPENXC7_IN_CONTAINER) $(PNR) $(PNR_OPTS) --chipdb $(FPGA_DB) --xdc ${CONFIG} --json $*.json --fasm $@

.fasm.frames:
	$(OPENXC7_IN_CONTAINER) $(FRAME) $(FRAME_OPTS) --part ${PART} --db-root ${XRAY_DB} $< >$@

.frames.bin:
	$(OPENXC7_IN_CONTAINER) $(PACK) --part_file ${XRAY_DB}/${PART}/part.yaml --part_name ${PART} --frm_file $< --output_file $@

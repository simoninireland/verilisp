# Docker file for RISC-V cross-compiler tools container
#
# Copyright (C) 2024--2025, Simon Dobson
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
ARG BASE_IMAGE="alpine:3.17"

FROM $BASE_IMAGE
LABEL description="RISC-V cross-compiler tools"
LABEL maintainer="Simon Dobson <simoninireland@gmail.com>"

USER root

RUN apk update
RUN apk upgrade

# Install the tooling
RUN apk add binutils-riscv-none-elf gcc-riscv-none-elf

# Create a working directory
RUN mkdir /work
WORKDIR /work

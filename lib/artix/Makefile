# Makefile for the Artix-7 tools container
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

OPENXC7_URL = https://github.com/openXC7/toolchain-nix.git
OPENXC7_REPO = $(basename $(notdir $(OPENXC7_URL)))

TAG = openxc7-docker
NIX = nix
GIT = git
DOCKER = docker
SED = sed
CHDIR = cd
RMDIR = rm -fr


build:
	$(GIT) clone $(OPENXC7_URL)
	$(CHDIR) $(OPENXC7_REPO)
	$(NIX) build -L ".#dockerImage.x86_64-linux"
	$(DOCKER) image load --input result | \
	$(SED) -nr 's/^Loaded image: [^:]+:(.*)$/\1/p' | \
	$(DOCKER) image tag $(TAG):3547cbmp8mbrjlchlbs41w51vfn9p8iv $(TAG):latest

clean:
	$(DOCKER) image rm $(TAG)
	$(RMDIR) $(OPENXC7_REPO)

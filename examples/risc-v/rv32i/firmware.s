/*
	Simple loop incrementing the x1 register, used to drive LEDs

	Copyright (C) 2024--2025 Simon Dobson

	This file is part of verilisp, a Common Lisp DSL for hardware design

	verilisp is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	verilisp is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.
*/

.section .text

.global	_start

_start:
	add  x1, x0, x0
.L0:
	addi x1, x1, 1
	jal  x0, .L0

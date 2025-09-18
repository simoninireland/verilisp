/*
	Countdown subroutine decrementing the x11 register, used to drive LEDs
	and exercise subroutine instructions and load/stores.

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
	li	x10, 8
	sw	x10, 100(x0)
	call	_countdown
	j	_start

_countdown:
	lw	x11, 100(x0)
	beqz	x11, _return
	addi	x11, x11, -1
	sw	x11, 100(x0)
	j	_countdown
_return:
	ret
	ebreak

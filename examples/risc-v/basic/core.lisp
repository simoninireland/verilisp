;; 32-bit integer-only RISC-V core
;;
;; Copyright (C) 2024--2025 Simon Dobson
;;
;; This file is part of verilisp, a Common Lisp DSL for hardware design
;;
;; verilisp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; verilisp is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

(defmodule SOC ((clk-in   :as :wire :type (unsigned-byte 1) :direction :in)
		;; (reset-in :as :wire :type (unsigned-byte 1) :direction :in)
		(leds-out :as :wire :type (unsigned-byte 5) :direction :out)
		(rxd      :as :wire :type (unsigned-byte 1) :direction :in)
		(txd      :as :wire :type (unsigned-byte 1) :direction :out))

  (let ((clk   0 :as :wire :type (unsigned-byte 1))
	(reset 0 :as :wire :type (unsigned-byte 1))

	;; plug in to the output to visualise
	(leds 0 :type (unsigned-byte 5) :as :register)

	;; core state
	(mem   (make-array '(256) :element-type (unsigned-byte 32)
				  :initial-contents (:file "firmware.hex")))
	(pc    0 :type (unsigned-byte 32) :as :register)
	(instr 0 :type (unsigned-byte 32) :as :register)

	;; clock management
	(cw (make-instance 'clockworks :clk-in clk-in
				       :reset-in 0
				       :clk clk
				       :reset reset
				       :slow 19)))

    ;; wire the LED wires to the output register
    (setq leds-out leds)

    ;; instruction decoding
    (let-wires ((isALUreg (= (bref instr 6 :end 0) #2r0110011))
		(isALUimm (= (bref instr 6 :end 0) #2r0010011))
		(isBranch (= (bref instr 6 :end 0) #2r1100011))
		(isJALR   (= (bref instr 6 :end 0) #2r1100111))
		(isJAL    (= (bref instr 6 :end 0) #2r1101111))
		(isAIUPC  (= (bref instr 6 :end 0) #2r0010111))
		(isLUT    (= (bref instr 6 :end 0) #2r0110111))
		(isLoad   (= (bref instr 6 :end 0) #2r0000011))
		(isStore  (= (bref instr 6 :end 0) #2r0100011))
		(isSystem (= (bref instr 6 :end 0) #2r1110011))

		;; intermediate formats
		(Uimm (make-bitfields (bref instr 31)
				      (bref instr 30 :end 12)
				      (extend-bits 0 12)))
		(Iimm (make-bitfields (extend-bits (bref instr 31) 21)
				      (bref instr 30 :end 20)))
		(Simm (make-bitfields (extend-bits (bref instr 31) 21)
				      (bref instr 30 :end 25)
				      (bref instr 11 :end 7)))
		(Bimm (make-bitfields (extend-bits (bref instr 31) 20)
				      (bref instr 7)
				      (bref instr 30 :end 25)
				      (bref instr 11 :end 8)
				      (extend-bits 0 1)))
		(Jimm (make-bitfields (extend-bits (bref instr 31) 12)
				      (bref instr 19 :end 12)
				      (bref instr 20)
				      (bref instr 30 :end 21)
				      (extend-bits 0 1)))

		;; source and destination registers
		(rs1Id (bref instr 19 :end 15))
		(rs2Id (bref instr 24 :end 20))
		(rdId  (bref instr 11 :end 7))

		;; function codes
		(funct3 (bref instr 14 :end 12))
		(funct7 (bref instr 31 :end 25)))

      ;; register bank
      (let ((RegisterBank  (make-array '(32) :element-type (unsigned-byte 32)))
	    (rs1           0)
	    (rs2           0))

	;; the ALU
	(let-wires ((aluIn1 rs1 :type (unsigned-byte 32)) ;; CHECK THIS
		    (aluIn2 (if isALUreg
				rs2
				Iimm))
		    (shamt (if isALUreg
			       (bref rs2 4 :end 0)
			       (bref instr 24 :end 20))))

	  (let ((aluOut 0 :type (unsigned-byte 32)))

	    (@ (*)
	       (case funct3
		 (#2r000
		  (if (logand (bref funct7 5)
			      (bref instr 5))
		      (setq aluOut (- aluIn1 aluIn2))
		      (setq aluOut (+ aluIn1 aluIn2))))

		 (#2r001
		  (setq aluOut (<< aluIn1 shamt)))

		 (#2r010
		  (setq aluOut (< aluIn1 aluIn2))) ;; signed

		 (#2r011
		  (setq aluOut (< aluIn1 aluIn2))) ;; unsigned

		 (#2r100
		  (setq aluOut (logxor aluIn1 aluIn2)))

		 (#2r101
		  (if (bref funct7 5)
		      (setq aluOut (>> aluIn1 shamt)) ;; sign-extended
		      (setq aluOut (>> aluIn1 shamt)))) ;; unsigned

		 (#2r110
		  (setq aluOut (logior aluIn1 aluIn2)))

		 (#2r111
		  (setq aluOut (logand aluIn1 aluIn2)))))

	    ;; the state machine
	    (@ (posedge clk)
	       (tagbody
		fetch-instruction
		  ;; State 0: fetch the next instruction
		  (if reset
		      (setq pc 0))

		  (setq instr (aref mem (bref pc 31 :end 2)))

		fetch-registers
		  ;; State 1: load from registers
		  (setq rs1 (aref RegisterBank rs1Id))
		  (setq rs2 (aref RegisterBank rs2Id))

		execute-writeback
		  ;; State 3: execute the instruction and write back results

		  ;; writeback data to registers
		  (when (and (or isALUReg
				 isALUImm
				 isJAL
				 isJALR)
			     (/= rdId 0))
		    (setf (aref RegisterBank rdId) (if (or isJAL isJALR)
						       (+ pc 4)
						       aluOut))

		    ;; update the LEDS if writing to R1
		    (when (= rdId 1)
		      (setq leds aluOut)))

		  ;; update PC
		  (when (not isSystem)
		    (setq pc (cond (isJAL
				    (+ pc Jimm))
				   (isJALR
				    (+ rs1 Iimm))
				   (t
				    (+ pc 4)))))))))))))

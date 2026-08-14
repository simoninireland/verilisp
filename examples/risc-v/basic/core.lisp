;;;; 32-bit integer-only basic RISC-V core
;;;;
;;;; Copyright (C) 2024--2026 Simon Dobson
;;;;
;;;; This file is part of verilisp, a Common Lisp DSL for hardware design
;;;;
;;;; verilisp is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; verilisp is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with verilisp. If not, see <http://www.gnu.org/licenses/gpl.html>.

;;; This core does not provide all the operations of a "proper" RV32I.
;;; Specifically it can't store values to memory.


(defmodule/vl SOC (clk-in
		   ;; reset-in
		   leds-out
		   rxd txd)
  (declare (type bit clk-in rxd txd)
	   (type (unsigned-byte 5) leds-out)
	   (ignorable rxd txd))

  (let* (clk reset

	     ;; plug in to the output to visualise
	     leds

	     ;; core state
	     (mem (make-array '(256) :element-type (unsigned-byte 32)
				     :initial-contents (:file "firmware.hex")))
	     pc instr

	     ;; clock management
	     (cw (make-instance 'clockworks :clk-in clk-in
					    :reset-in 0
					    :clk clk
					    :reset reset
					    :slow 19)))
    (declare (type bit clk reset)
	     (type (unsigned-byte 5) leds)
	     (type (unsigned-byte 32) pc instr))

    ;; wire the LED wires to the output register
    (setq leds-out leds)

    ;; instruction decoding
    (with-bitfields ((funct7 7) (rs2Id 5) (rs1Id 5) (funct3 3) (rdId 5) (opcode 7))
      instr

      (let (;; instruction classes
	    (ALUreg-p (= opcode #2r0110011))
	    (ALUimm-p (= opcode #2r0010011))
	    (branch-p (= opcode #2r1100011))
	    (JALR-p   (= opcode #2r1100111))
	    (JAL-p    (= opcode #2r1101111))
	    (AIUPC-p  (= opcode #2r0010111))
	    (LUT-p    (= opcode #2r0110111))
	    (load-p   (= opcode #2r0000011))
	    (store-p  (= opcode #2r0100011))
	    (system-p (= opcode #2r1110011))

	    ;; immediate formats
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
				  (extend-bits 0 1))))
	(declare (type bit ALUreg-p ALUimm-p branch-p
			   JALR-p JAL-p
			   AIUPC-p LUT-p
			   load-p store-p
			   system-p))

	;; register file and working registers
	(let ((register-file (make-array '(32) :element-type (unsigned-byte 32)
					       :initial-element 0))
	      (rs1           0)
	      (rs2           0))

	  ;; the ALU
	  (let ((aluIn1 rs1)
		(aluIn2 rs2)
		(shamt (if ALUreg-p
			   (bref rs2 4 :end 0)
			   (bref instr 24 :end 20)))
		aluOut)

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
		  (setq aluOut (< (the 'signed-byte aluIn1)
				  (the 'signed-byte aluIn2)))) ;; signed

		 (#2r011
		  (setq aluOut (< aluIn1 aluIn2))) ;; unsigned

		 (#2r100
		  (setq aluOut (logxor aluIn1 aluIn2)))

		 (#2r101
		  (if (bref funct7 5)
		      (setq aluOut (>> (the 'signed-byte aluIn1) shamt)) ;; sign-extended
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
		  (setq rs1 (aref register-file rs1Id))
		  (setq rs2 (aref register-file rs2Id))

		execute-writeback
		  ;; State 2: execute the instruction and write back results

		  ;; writeback data to registers
		  (when (and (or ALUReg-p
				 ALUImm-p
				 JAL-p
				 JALR-p)
			     (/= rdId 0)) ; don't update R0, which is constantly 0
		    (setf (aref register-file rdId) (if (or JAL-p JALR-p)
							(+ pc 4)
							aluOut))

		    ;; update the LEDS if writing to R1
		    (when (= rdId 1)
		      (setq leds aluOut)))

		  ;; update PC
		  (when (not system-p)
		    (setq pc (cond (JAL-p
				    (+ pc Jimm))
				   (JALR-p
				    (+ rs1 Iimm))
				   (t
				    (+ pc 4)))))

		  (go fetch-instruction)))))))))

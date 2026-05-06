;;;; 32-bit integer-only RISC-V core
;;;;
;;;; Copyright (C) 2024--2025 Simon Dobson
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

(use-package :alexandria)

;;; This is a full RV32I core with arithmetic and memory access.


;;; ---------- Components and macros ----------

(defmacro/vl flip32 (v)
  (let ((es (mapcar (lambda (i)
		      `(bref ,v ,i))
		    (iota 32))))
    `(make-bitfields ,@es)))


(defmodule/vl ram (clk rd/wr
		       addr read-data write-mask write-data
		       &key (words 256))
  (declare (type (unsigned-byte 32) addr read-data write-data)
	   (type bit clk rd/wr)
	   (type (unsigned-byte 4) write-mask)
	   (as register read-data))

  (let ((mem (make-array (words) :element-type (unsigned-byte 32)
				 :initial-contents (:file "firmware.hex")))
	(word-addr (bref addr 31 :width 30)))

    (@ (posedge clk)
       (if (asserted-p rd/wr)
	   ;; writing
	   (with-bitfields ((b3 8) (b2 8) (b1 8) (b0 8))
	     (aref mem word-addr)

	     (with-bitfields ((d3 8) (d2 8) (d1 8) (d0 8))
	       write-data

	       (when (asserted-p (bref write-mask 0))
		 (setf b0 d0))
	       (when (asserted-p (bref write-mask 1))
		 (setf b1 d1))
	       (when (asserted-p (bref write-mask 2))
		 (setf b2 d2))
	       (when (asserted-p (bref write-mask 3))
		 (setf b3 d3))))

	   ;; reading
	   (setf read-data (aref mem word-addr))))))


(defmodule/vl rv32i (clk reset
			 rd/wr addr read-data write-mask write-data
			 status)
  (declare (type (unsigned-byte 32) addr read-data write-data)
	   (type bit clk reset rd/wr)
	   (type (unsigned-byte 4) write-mask)
	   (type (unsigned-byte 2) status))

  ;; state
  (let ((pc    0)
	(instr 0))
    (declare (type (unsigned-byte 32) pc instr))

    ;; instruction decoding
    (with-bitfields ((funct7 7) (rs2Id 5) (rs1Id 5) (funct3 3) (rdId 5) (opcode 7))
      instr

      (let (;; instruction class predicates
	    (ALUreg-p (= opcode #2r0110011))
	    (ALUimm-p (= opcode #2r0010011))
	    (branch-p (= opcode #2r1100011))
	    (JALR-p   (= opcode #2r1100111))
	    (JAL-p    (= opcode #2r1101111))
	    (AUIPC-p  (= opcode #2r0010111))
	    (LUI-p    (= opcode #2r0110111))
	    (load-p   (= opcode #2r0000011))
	    (store-p  (= opcode #2r0100011))
	    (system-p (= opcode #2r1110011))

	    ;; immediate values
	    (Uimm (the '(unsigned-byte 32) (make-bitfields (bref instr 31 :end 12)
							   (extend-bits 0 12))))
	    (Iimm (the '(signed-byte 32) (make-bitfields (extend-bits (bref instr 31) 21)
							 (bref instr 30 :end 20))))
	    (Simm (the '(signed-byte 32) (make-bitfields (extend-bits (bref instr 31) 21)
							 (bref instr 30 :end 25)
							 (bref instr 11 :end 7))))
	    (Bimm (the '(signed-byte 32) (make-bitfields (extend-bits (bref instr 31) 20)
							 (bref instr 7)
							 (bref instr 30 :end 25)
							 (bref instr 11 :end 8)
							 0)))
	    (Jimm (the '(signed-byte 32) (make-bitfields (extend-bits (bref instr 31) 12)
							 (bref instr 19 :end 12)
							 (bref instr 20)
							 (bref instr 30 :end 21)
							 0)))

	    ;; these are more type-based, but we need to fix coercion to not
	    ;; introduce subscripts on subscripts
	    ;; (Uimm (the '(unsigned-byte 32) (make-bitfields (bref instr 31 :end 12)
	    ;;						   (extend-bits 0 12))))
	    ;; (Iimm (coerce (the '(signed-byte 12) (bref instr 31 :end 20))
	    ;;		  '(signed-byte 32)))
	    ;; (Simm (coerce (the '(signed-byte 12) (make-bitfields (bref instr 31 :end 25)
	    ;;							 (bref instr 11 :end 7)))
	    ;;		  '(signed-byte 32)))
	    ;; (Bimm (coerce (the '(signed-byte 13) (make-bitfields (bref instr 31)
	    ;;							 (bref instr 7)
	    ;;							 (bref instr 30 :end 25)
	    ;;							 (bref instr 11 :end 8)
	    ;;							 0))
	    ;;		  '(signed-byte 32)))
	    ;; (Jimm (coerce (the '(signed-byte 20) (make-bitfields (bref instr 31)
	    ;;							 (bref instr 19 :end 12)
	    ;;							 (bref instr 20)
	    ;;							 (bref instr 30 :end 21)
	    ;;							 0))
	    ;;		  '(signed-byte 32)))

	    ;; register file and working registers
	    (register-file    (make-array '(32) :element-type (unsigned-byte 32)
						:initial-element 0))
	    (rs1              0)
	    (rs2              0))
	(declare (type (unsigned-byte 32) rs1 rs2))

	;; ALU
	(let* ((aluIn1 rs1)
	       (aluIn2 (if (or ALUreg-p branch-p)
			   rs2
			   Iimm))
	       aluOut
	       (alu-plus (+ aluIn1 aluIn2))
	       (alu-minus (+ (make-bitfields 1 (lognot aluIn2))
			     (make-bitfields 0 aluIn1)
			     1))

	       ;; shifters
	       (shifter-in (if (= funct3 1)
			       (flip32 aluIn1)
			       aluIn1))
	       (shifter (>> (the '(signed-byte 33) (make-bitfields (logand (bref instr 30)
									   (bref aluIn1 31))
								   shifter-in))
			    (bref aluIn2 4 :end 0)))
	       (left-shift (flip32 shifter))

	       ;; comparator
	       (LT (if (logxor (bref aluIn1 31)
			       (bref aluIn2 31))
		       (bref aluIn1 31)
		       (bref alu-minus 32)))
	       (LTU (bref alu-minus 32))
	       (EQ (0= (coerce alu-minus '(unsigned-byte 32))))
	       take-branch-p)
	  (declare (type (unsigned-byte 32) alu-plus aluOut)
		   (type (unsigned-byte 33) alu-minus))

	  ;; ALU operations
	  (@ (*)
	     (case funct3
	       (#2r000
		(setq aluOut (if (and (bref funct7 5)
				      (bref instr 5))
				 (coerce alu-minus '(unsigned-byte 32))
				 alu-plus)))
	       (#2r001
		(setq aluOut left-shift))
	       (#2r010
		(setq aluOut (coerce LT '(unsigned-byte 32))))
	       (#2r011
		(setq aluOut (coerce LTU '(unsigned-byte 32))))
	       (#2r100
		(setq aluOut (logxor aluIn1 aluIn2)))
	       (#2r101
		(setq aluOut shifter))
	       (#2r110
		(setq aluOut (logior aluIn1 aluIn2)))
	       (#2r111
		(setq aluOut (logand aluIn1 aluIn2)))))

	  ;; branch-taking predicate
	  (@ (*)
	     (case funct3
	       (#2r000
		(setq take-branch-p EQ))
	       (#2r001
		(setq take-branch-p (not EQ)))
	       (#2r100
		(setq take-branch-p LT))
	       (#2r101
		(setq take-branch-p (not LT)))
	       (#2r110
		(setq take-branch-p LTU))
	       (#2r111
		(setq take-branch-p (not LTU)))
	       (t
		(setq take-branch-p 0))))

	  ;; memory operations
	  (let* ((byte-access-p      (= (bref funct3 1 :width 2) 0))
		 (half-word-access-p (= (bref funct3 1 :width 2) 1))

		 (load-half-word     (if (bref load-store-addr 1)
					 (bref read-data 31 :width 16)
					 (bref read-data 15 :width 16)))
		 (load-byte          (if (bref load-store-addr 0)
					 (bref load-half-word 15 :width 8)
					 (bref load-half-word  7 :width 8)))

		 (load-sign-extend-p (not (bref funct3 2)))
		 (load-data (cond (byte-access-p
				   (if load-sign-extend-p
				       (coerce (the (signed-byte 8) load-byte) (signed-byte 32))
				       (coerce load-byte (unsigned-byte 32))))

				  (half-word-access-p
				   (if load-sign-extend-p
				       (coerce (the (signed-byte 16) load-half-word) (signed-byte 32))
				       (coerce load-half-word (unsigned-byte 32))))

				  (t
				   read-data)))

		 (store-write-mask (cond (byte-access-p
					  (if (bref load-store-addr 1)
					      (if (bref load-store-addr 0)
						  #2r1000
						  #2r0100)
					      (if (bref load-store-addr 0)
						  #2r0010
						  #2r0001)))

					 (half-word-access-p
					  (if (bref load-store-addr 1)
					      #2r1100
					      #2r0011))

					 (t
					  #2r1111)))

		 (load-store-addr (+ rs1 (if store-p
					     Simm
					     Iimm))))
	    (declare (type (unsigned-byte 32) load-store-addr))

	    ;; store assignments
	    (with-bitfields ((w3 8) (w2 8) (w1 8) (w0 8))
	      write-data

	      (with-bitfields ((d3 8) (d2 8) (d1 8) (d0 8))
		rs2

		(setf w0 d0)
		(setf w1 (if (bref load-store-addr 0)
			     d0
			     d1))
		(setf w2 (if (bref load-store-addr 1)
			     d0
			     d2))
		(setf w3 (if (bref load-store-addr 0)
			     d0
			     (if (bref load-store-addr 1)
				 d1
				 d3)))))

	    ;; main state machine
	    (@ (posedge clk)
	       (tagbody
		instruction-fetch
		  ;; check for reset
		  ;; (when (not (asserted-p reset))
		  ;;   (setq pc 0)
		  ;;   (go instruction-fetch))
		  (setq rd/wr 0)
		  (setq addr pc)

		instruction-wait
		  ;; read the instruction

		instruction-load
		  ;; load and decode the instruction
		  (setq instr read-data)

		register-fetch
		  ;; fetch registers
		  (setq rs1 (aref register-file rs1Id))
		  (setq rs2 (aref register-file rs2Id))

		execute
		  (if system-p
		      ;; system instructions stop the core
		      (go stop)

		      ;; set up load/store memory accesses
		      (cond (load-p
			     (setq rd/wr 0)
			     (setq addr load-store-addr))

			    (store-p
			     (setq rd/wr 1)
			     (setq addr load-store-addr)
			     (setq write-mask store-write-mask))

			    (t
			     (go writeback))))

		load/store-wait
		  ;; perform any load/store operation

		writeback
		  ;; write-back data to registers
		  (when (and (not (or branch-p store-p))
			     (0/= rdId))
		    (setf (aref register-file rdId) (cond ((or JAL-p JALR-p)
							   (+ pc 4))
							  (LUI-p
							   Uimm)
							  (AUIPC-p
							   (+ pc Uimm))
							  (load-p
							   load-data)
							  (t
							   aluOut))))

		  ;; increment the PC according to the instruction class
		  (let ((next-pc (cond ((and branch-p take-branch-p)
					(+ pc Bimm))
				       (JAL-p
					(+ pc Jimm))
				       (JALR-p
					(make-bitfields (bref alu-plus 31 :end 1) 0))
				       (t
					(+ pc 4)))))
		    (declare (type (unsigned-byte 32) next-pc))

		    (setq pc next-pc))

		display
		  ;; write the contents of X11 to the status LEDs
		  (when (= rdId 11)
		    (setq status (bref (aref register-file 11) 4 :width 5)))

		  ;; next cycle
		  (go instruction-fetch)

		stop
		  ;; system instructions come here and halt the core
		  (go stop)))))))))


;;; ---------- SoC ----------

(defmodule/vl soc (system-clk system-reset
			      leds
			      rxd txd)
  (declare (type bit system-clk system-reset rxd txd)
	   (type (unsigned-byte 2) leds)
	   (ignorable rxd txd))

  (let (clk reset
	addr rd/wr
	read-data
	write-data write-mask)

    (let ((soc-clock (make-instance 'clockworks :clk-in system-clk :clk clk
						:reset-in 0 :reset reset
						:slow 18))
	  (soc-ram (make-instance 'ram :clk clk
				       :addr addr :rd/wr rd/wr
				       :read-data read-data
				       :write-data write-data :write-mask write-mask
				       :words 256))
	  (soc-core (make-instance 'rv32i :clk clk :reset reset
					  :addr addr :rd/wr rd/wr
					  :read-data read-data
					  :write-data write-data :write-mask write-mask
					  :status leds)))

      ;; UART not used for now
      (setq txd 0))))

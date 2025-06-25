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


;; ---------- Supporting modules ----------

;; RAM
(defmodule/vl ram ((addr       :direction :in)
		   (rd/wr      :direction :in :width 1)
		   (write-mask :direction :in :width 4)
		   (data       :direction :inout)
		   &key
		   (words 256))

  (let ((mem (make-array (words) :element-type (unsigned-byte 32)
				 :initial-element 0)))

    (@ (*)
       (let ((word-addr (>> addr 2)))
	 (if rd/wr
	     ;; writing
	     (let ((updated (aref mem word-addr)))
	       (with-bitfields (b3 b3 b3 b3 b3 b3 b3 b3
				b2 b2 b2 b2 b2 b2 b2 b2
				b1 b1 b1 b1 b1 b1 b1 b1
				b0 b0 b0 b0 b0 b0 b0 b0)
		   updated
					;TODO: Is the mask being interpreted correctly?
		 (when (asserted-p (bref write-mask 0))
		   (setf b0 (bref data 7 :width 8)))
		 (when (asserted-p (bref write-mask 1))
		   (setf b1 (bref data 15 :width 8)))
		 (when (asserted-p (bref write-mask 2))
		   (setf b2 (bref data 23 :width 8)))
		 (when (asserted-p (bref write-mask 3))
		   (setf b3 (bref data 31 :width 8))))

	       ;; write the update data through
	       (setf (aref mem word-addr) updated))

	     ;; reading
	     (setf data (aref mem word-addr)))))))


;; Simple ALU
(defmodule/vl rv321-alu ((a                :direction :in  :type (unsigned-byte 32))
			 (b                :direction :in  :type (unsigned-byte 32))
			 (add/sub          :direction :in  :type (unsigned-byte 1))
			 (sign-extending-p :direction :in  :type (unsigned-byte 1))
			 (op               :direction :in  :type (unsigned-byte 3))
			 (shift            :direction :in  :type (unsigned-byte 5))
			 (c                :direction :out :type (unsigned-byte 32)))
  (@ (*)
     (case op
       (#2r000
	;; add or subtract
	(setq c (if (asserted-p add/sub)
		    (coerce (- a b) '(unsigned-byte 32))
		    (coerce (+ a b) '(unsigned-byte 32)))))

       (#2r001
	;; left shift
	(setq c (coerce (<< a b) '(unsigned-byte 32))))

       (#2r010
	;; less-than unsigned
	(setq c  (< (the '(unsigned-byte 32) a) ;TODO: TEST ME
		    (the '(unsigned-byte 32) b))))

       (#2r011
	;; less-than signed
	(setq c  (< (the '(signed-byte 32) a) ;TODO: TEST ME
		    (the '(signed-byte 32) b))))

       (#2r100
	;; exclusive or
	(setq c (logxor a b)))

       (#2r101
	;; right shift, logical or arithmetic
	(setq c (if (asserted-p sign-extending-p)
		    (>> a b) ;TODO: FIX ME: get the sign extension right
		    (>> a b))))

       (#2r110
	;; or
	(setq c (logior a b)))

       (#2r100
	;; and
	(setq c (logand a b))))))


;; Simple comparator
(defmodule/vl rv32i-comparator ((a  :direction :in  :width 32)
				(b  :direction :in  :width 32)
				(op :direction :in  :width 3)
				(c  :direction :out :width 32))
  (@ (*)
     (case op
       (#2r000
	;; equality
	(setq c (= a b)))

       (#2r001
	;; inequality
	(setq c (/= a b)))

       (#2r100
	;; less-than (signed)
	(setq c (< a b)))

       (#2r101
	;; greater-than or equal-to (signed)
	(setq c (>= a b)))

       (#2r110
	;; less-than (unsigned)
	(setq c (< a b)))		;TODO:  FIX ME

       (#2r111
	;; greater-than or equal-to (unsigned)
	(setq c (>= a b)))		;TODO:  FIX ME

       (t
	(setq c 0)))))


;; ---------- Core ----------

(defmodule/vl rv32i ((clk   :direction :in :width 1)
		     (reset :direction :in :width 1))

  ;; state
  (let ((pc 0 :width 32)
	(instr 0 :width 32)

	;; working registers
	(rs1             0 :width 32)
	(rs2             0 :width 32)
	(write-back-data 0 :width 32)
	(next-pc         0 :width 32)

	;; registers
	(register-file (make-array (32) :element-type (unsigned-byte 32)
					:initial-element 0)))

    ;; wiring
    (let-wires (a b c compare
		(op      0 :width 3)
		(add/sub 0 :width 1)

		;; memory access
		(addr       0 :width 32)
		(data       0 :width 32)
		(rd/wr      0 :width 1)
		(write-mask 0 :width 4))

      (let ((mem (make-instance 'ram :addr addr :data data
				     :rd/wr rd/wr :write-mask write-mask))
	    (alu (make-instance 'rv32i-alu :a a :b b :c c
					   :op op
					   :shift shift-amount))
	    (comparator (make-instance 'rv32i-comparator :a a :b b :c compare
							 :op op)))

	;; decoding
	(with-bitfields ((funct7 7) (rs2id 5) (rs1id 5) (funct3 3) (rdid 5) (opcode 7))
	    instr

	  (let-wires ((Uimm (coerce (the '(signed-byte 12) (bref instr 31 :end 12))
				    '(signed-byte 32)))
		      (Iimm (coerce (the '(signed-byte 12) (bref instr 31 :end 20))
				    '(signed-byte 32)))
		      (Simm (coerce (the '(signed-byte 12) (make-bitfields (bref instr 31)
									   (bref instr 30 :end 25)
									   (bref instr 11 :end 7)))
				    '(signed-byte 32)))
		      (Bimm (coerce (the '(signed-byte 12) (make-bitfields (bref instr 31)
									   (bref instr 7)
									    (bref instr 30 :end 25)
									    (bref instr 11 :end 8)
									    0))
				    '(signed-byte 32)))
		      (Jimm (coerce (the '(signed-byte 22) (make-bitfields (bref instr 31)
									   (bref instr 19 :end 12)
									   (bref instr 20)
									   (bref instr 30 :end 21)
									   0))
				    '(signed-byte 32))))

	    ;; state machine
	    (@ (posedge clk)

	       (if (asserted-p reset)
		   ;; reset line asserted, perform a reset
		   (progn
		     (setf pc 0)
		     (setf instr 0))

		   ;; run main behaviour
		   (tagbody
		    instruction-fetch
		      ;; fetch next instruction
		      (setq addr pc)
		      (setq rd/wr 0)
		      (setq instr data)
		      (setf next-pc (+ pc 4))

		    data-fetch
		      ;; fetch registers
		      (setq rs1 (aref register-file rs1id))
		      (setq rs2 (aref register-file rs2id))

		    execute
		      ;; execute the behaviour for the current instruction
		      (case opcode
			;; ALU register-with-register arithmetic (ALUreg)
			(#2r0110011
			 (setq a rs1)
			 (setq b rs2)
			 (setq op funct3)
			 (setq add/sub  (logand (bref funct7 5)
						(bref instr 5)))
			 (setf shift-amount (bref rs2 4 :end 0))
			 (setf write-back-data c))

			;; ALU register-with-immediate arithmetic (ALUimm)
			(#2r0010011
			 (setq a rs1)
			 (setq b Iimm)
			 (setq op funct3)
			 (setq add/sub (logand (bref funct7 5)
					       (bref instr 5)))
			 (setf shift-amount (bref instr 24 :end 20))
			 (setf write-back-data c))

			;; conditional branch (BR)
			(#2r1100011
			 (setq a rs1)
			 (setq b rs2)
			 (setq op funct3)
			 (if compare
			     (setf next-pc (+ pc Bimm))))

			;; jump and link relative to register (JALR)
			(#2r1100111
			 (setf write-back-data (+ pc 4))
			 (setf next-pc (+ rs1 Iimm) ))

			;; jump and link relative to PC (JAL)
			(#2r1101111
			 (setf write-back-data (+ pc 4))
			 (setf next-pc (+ pc Jimm)))

			;; add upper immediate to PC (AUIPC)
			(#2r0010111
			 (setf write-back-data (+ pc Uimm)))

			;; load upper immediate (LUI)
			(#2r0110111
			 (setf write-back-data Uimm))

			;; load relative to register (L)
			(#2r0000011
			 (let* ((read-type (bref funct3 1 :width 2))
				(sign-extending (= (bref funct3 2) 1)))

			   ;; load data
			   (setq addr (+ rs1 Iimm))
			   (setq rd/wr 0)

			   ;; extract loaded data from read data
			   (cond ((= read-type #2r00)
				  ;; load byte
				  (setq write-back-data
					(if (asserted-p (bref addr 0))
					    (if (asserted-p (bref addr 1))
						;; upper byte of upper half-word
						(bref (bref data 31 :end 16) 15 :end 8)

						;; lower byte of upper half-word
						(bref (bref data 31 :end 16) 7 :end 0))

					    (if (asserted-p (bits addr 1))
						;; upper byte of lower half-word
						(bref (bref data 15 :end 0) 15 :end 8)

						;; lower byte of lower half-word
						(bref (bref data 15 :end 0) 7 :end 0))))

				  ;; sign-extend if requested
				  (if sign-extending
				      (setf write-back-data (coerce write-back-data '(signed-byte 8)))))

				 ((= read-type #2r01)
				  ;; load half-word
				  (setq write-back-data
					(if (asserted-p (bref addr 1))
					    ;; upper half-word
					    (bref data 31 :end 16)

					    ;; lower half-word
					    (bref data 15 :end 0)))

				  ;; sign-extend if requested
				  (if sign-extending
				      (setf write-back-data (coerce write-back-data '(signed-byte 16)))))

				 (t
				  ;; load word
				  (setf write-back-data data)))))

			;; store relative to register (S)
			(#2r0100011
			 (let ((read-type (bref funct3 1 :width 2)))
			   (setq write-mask (cond ((= read-type #2r00)
						   ;; store byte
						   (if (asserted-p (bref addr 1))
						       ;TODO: Check these masks are correct
						       ;; writing to byte in upper half-word
						       (if (asserted-p (bref addr 0))
							   #2r1000
							   #2r0100)

						       ;; writing to byte in lower half-word
						       (if (asserted-p (bref addr 0))
							   #2r0010
							   #2r0001)))

						  ((= read-type #2r01)
						   ;; store half-word
						   (if (asserted-p (bref addr 1))
						       ;; writing to upper half-word
						       #2r1100

						       ;; writing to lower half-word
						       #2r0011))

						  (t
						   ;; store word
						   #2r1111)))

			   ;; store the value
			   (setq addr (+ rs1 Simm))
			   (setq rd/wr 1)
			   (setq data rs2)))

			;; system (SYSTEM)
			(#2r1110011
			 ;; for now just stop execution here
			 (setf next-pc pc)))

		    write-back
		      ;; write-back register
		      (when (and (/= rdid 0)
				 (or (= opcode #2r0110011) ; ALUreg
				     (= opcode #2r0010011) ; ALUimm
				     (= opcode #2r1100111) ; JALR
				     (= opcode #2r1101111) ; JAL
				     (= opcode #2r0010111) ; AUIPC
				     (= opcode #2r0110111) ; LUI
				     (= opcode #2r0000011) ; L
				     ))
			(setf (aref register-file rdid) write-back-data))

		      ;; update PC
		      (setf pc next-pc))))))))))

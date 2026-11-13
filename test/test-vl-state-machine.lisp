;;;; Tests of state machine construction
;;;;
;;;; Copyright (C) 2024--2026 Simon Dobson
;;;;
;;;; This file is part of verilisp, a very Lisp approach to hardware synthesis
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

(in-package :verilisp/test)
(in-suite verilisp/vl)


;;; ---------- State machine construction ----------

(test test-tagbody-simple
  "Test we can create a simple TAGBODY."
  (let ((sm (vl::build-state-machine (cdr '(tagbody
						init
						(setq a 10))))))
    (is (= (length (vl::state-machine-states sm)) 3))))


(test test-tagbody-spin
  "Test we can create a spinning state machine."
  (let ((sm (vl::build-state-machine (cdr '(tagbody
					    init)))))

    ;; initial, looping, and body state that's not eliminated
    (is (= (length (vl::state-machine-states sm)) 3))))


(test test-tagbody-empty
  "Test we can create an empty (also spinning) state machine."
  (let ((sm (vl::build-state-machine (cdr '(tagbody)))))

    ;; like above but no body state
    (is (= (length (vl::state-machine-states sm)) 2))))


(test test-tagbody-unreachable
  "Test we signal unreachable code."
  (signals (vl:unreachable-code)
    (vl::build-state-machine (cdr '(tagbody
				    init
				      (setq a 10)
				    pre
				      (setq b 20)
				      (go init)

				      ;; this code is unreachable
				      (setq a 0))))))


;;; ---------- Typechecking ----------

(test test-tagbody-type
  "Test we can typecheck a TAGBODY."
  (let ((p (vl:expand/vl
	    '(let (b c d)
	      (tagbody
		 (case b
		   (1
		    (setq c 1)
		    (setq d 12))

		   (2
		    (setq c 2))

		   (3
		    (setq c 4)
		    (go skip)))

		 (setq b 0)
	       skip
		 (setq d 999))))))

    (is (vl:typecheck p))))


;;; ---------- Transformation ----------

(test test-tagbody-simple-infinite
  "Test we can generate the simplest infinite loop."
  (let ((p (vl::expand/vl '(let (out
				 (counter 1))
			    (declare (type (unsigned-byte 8) out counter))
			    (tagbody
			     initial
			       (go initial))))))

    (vl::typecheck p)
    (is (vl::elaborate-state-machines p))))


(test test-tagbody-simple-looping
  "Test we can generate a loop manually."
  (let ((p (vl::expand/vl '(let (out
				 (counter 1))
			    (declare (type (unsigned-byte 8) out counter))
			    (tagbody
			     count
			       ;; wait while the counter increments around the counter
			       (setq counter (+ counter 1))
			       (if (> counter 0)
				   (go count))

			     blink
			       ;; update the LED
			       (setq out (+ out 1))
			       (go count))))))

    (vl::typecheck p)
    (let ((q (vl::elaborate-state-machines p)))
      (is (contains-form-p '(setq counter (+ counter 1)) q))
      (is (contains-form-p '(setq out (+ out 1)) q)))))


(test test-tagbody-while-looping
  "Test we can generate a loop using WHILE (a nested TAGBODY)."
  (let ((p (vl::expand/vl '(let (out
				 (counter 1))
			    (declare (type (unsigned-byte 8) out counter))
			    (tagbody
			     looping
			       (while (> counter 0)
				      (setq counter (+ counter 1)))

			       (setq out (+ out 1))
			       (go looping))))))

    (vl::typecheck p)
    (let ((q (vl::elaborate-state-machines p)))
      (is (contains-form-p '(setq counter (+ counter 1)) q))
      (is (contains-form-p '(setq out (+ out 1)) q)))))


(test test-tagbody-forever-looping
  "Test we can generate a loop using FOREVER (a nested TAGBODY)."
  (let ((p (vl::expand/vl '(let (out
				 (counter 1))
			    (declare (type (unsigned-byte 8) out counter))
			    (tagbody
			       (forever
				(while (> counter 0)
				       (setq counter (+ counter 1)))

				(setq out (+ out 1))))))))
    (vl::typecheck p)
    (let ((q (vl::elaborate-state-machines p)))
      (is (contains-form-p '(setq counter (+ counter 1)) q) )
      (is (contains-form-p '(setq out (+ out 1)) q)))))


(test test-tagbody-following-if
  "Test we pick up forms after an IF."

  (let ((p (vl::expand/vl '(let (a b c)
			    (tagbody
			     rx-idle
			       (setq a 0)
			       (setq b 0)
			       (if (0/= c)
				   (go rx-idle))

			       (setq a 1)
			       (setq b 1)

			     rx-active
			       (setq c 1)
			       (go rx-idle))))))

    (vl::typecheck p)
    (let ((q (vl::elaborate-state-machines p)))
      ;; first state
      (is (contains-form-p '(setq a 0) q))

      ;; continuation of first state
      (is (contains-form-p '(setq a 1) q))

      ;; second state
      (is (contains-form-p '(setq c 1) q)))))


(test test-tagbody-nested-if
  "Test we can handle nested IFs."
  (let ((p (vl::expand/vl '(let (a b c)
			    (declare (type (unsigned-byte 8) a b c))
			    (tagbody
			     rx-check-start
			       (if (0= a)
				   (if (0= b)
				       (progn
					 ;; starting pulse is still zero
					 (setq c 1)
					 (go rx-sample-bits))

				       ;; starting pulse has disappeared, error
				       (go rx-error))

				   (go rx-check-start))

			     rx-sample-bits
			       (setq c 2)
			       (go rx-check-start)

			     rx-error
			       (go rx-error))))))

    (vl::typecheck p)
    (is (contains-form-p '(setq c 1) p))
    (is (contains-form-p '(setq c 2) p))))


(test test-tagbody-coalesce-conditional
  "Test we coalesce singleton-state branches of a conditional."
  (with-new-frame
    (let ((p (vl::expand/vl '(let (a b)
			      (declare (type (unsigned-byte 8) a b))
			      (tagbody
			       initial-state
				 (if a
				     (setq b 1)
				     (progn
				       (setq b 2)
				       (go jump-state)))
				 (setq a (+ a 1))
			       return-state
				 (go initial-state)
			       jump-state
				 (setq a (+ b 1))
				 (if (> a b)
				     (go return-state)))))))

      (vl::typecheck p)
      (let ((q (vl::elaborate-state-machines p)))
	(is (contains-form-p '(setq a (+ a 1)) p))
	(is (contains-form-p '(setq a (+ b 1)) p))))))


(test test-tagbody-nested-in-then-conditional
  "Test we can compile a state machine within the then arm of a conditional."
  (with-new-frame
    (let ((p (vl::expand/vl '(let (a b)
			      (declare (type (unsigned-byte 8) a b))
			      (tagbody
			       initial-state
				 (if a
				     ;; (while (> a b)
				     ;; 	    (incf b))
				     (tagbody
				      s1
					(if (> a b)
					    (progn
					      (setf b (+ b 1))
					      (go s1))
					    (go s2)))
				     )
			       s2
				 (setq a (+ a 12))
			       return-state
				 (incf a)
				 (go initial-state)
			       jump-state
				 (setq a (+ b 1))
				 (if (> a b)
				     (go return-state)))))))

      (vl::typecheck p)
      (let ((q (vl::elaborate-state-machines p)))
	(is (contains-form-p '(setq a (+ a 12)) p))q))))


(test test-tagbody-nested-in-else-conditional
  "Test we can compile a state machine within the else arm of a cnditional."
  (with-new-frame
    (let ((p (vl::expand/vl '(let (a b)
			      (declare (type (unsigned-byte 8) a b))
			      (tagbody
			       initial-state
				 (if a
				     (setq a 14)
				     (while (> a b)
					    (incf b)))
				 (setq a (+ a 12))
			       return-state
				 (incf a)
				 (go initial-state)
			       jump-state
				 (setq a (+ b 1))
				 (if (> a b)
				     (go return-state)))))))

      (vl::typecheck p)
      (let ((q (vl::elaborate-state-machines p)))
	(is (contains-form-p '(setq a (+ a 12)) p))
	(is (contains-form-p '(setq a (+ b 1)) p))))))


(test test-no-state-machine
  "Test elaborating state machines can handle the case of there not being one."
  (is (vl::build-state-machine '((progn
				   (setf a 123)
				   (setf b 0))
				 (setf c 56)))))


(test test-no-state-machine-if
  "Test we handle something that could appear inside a state machine."
  (let ((sm (vl::build-state-machine '((if d (setf a 123) (setf b 0))
				       (setf c 56)))))
    (is (vl::synthesise-state-machine sm))))


(test test-no-state-machine-loop
  "Test we handle a machine with an explicit loop that will be optimised out."
  (let ((sm (vl::build-state-machine '(start
				       (if d (setf a 123))
				       (go start)))))
    (is (vl::synthesise-state-machine sm))))


(test test-state-machine-explicit-case
  "Test we can compile a state macine that itself contains a CASE form."
  (let ((sm (vl::build-state-machine '((progn
					 (case d
					   (1 (setf a 123))
					   (2 (setf b 23))
					   (t (setf a 0)))
					 (setf c 56))))))
    (vl::improve-state-machine sm)
    (is (vl::synthesise-state-machine sm))))


(test test-state-machine-jumps
  "Test we can synthesise a state machine with some looping."
  (let ((sm (vl::build-state-machine '(start
				       (if d
					   (go one)
					   (go two))
				       one
				       (setq a 26)
				       two
				       (setq b 67)
				       (setf c 56)))))
    (vl::improve-state-machine sm)
    (is (vl::synthesise-state-machine sm))))


(test test-state-machine-loops-etc
  "Test we can synthesise a machine with loops and other things,"
  (let ((sm (vl::build-state-machine '(start
				       (setq m 0)
				       (case d
					 (1 (go one))
					 (2 (setq b 999) (go two) (setq b 45))
					 (t (setq a 10)
					  (setq b 76)))
				       one
				       (setq a 26)
				       two
				       (setq b 67)
				       (setf c 56)))))
    (vl::improve-state-machine sm)
    (is (vl::synthesise-state-machine sm))))


(test test-machine-nested-tagbody
  "Test we can handle amchines within machine."
  (let ((sm (vl::build-state-machine '(start
				       (setq m 0)
				       (case d
					 (1 (go one))
					 (2 (setq b 999) (go two) (setq b 45))
					 (3 (setq b 100))
					 (t (setq a 10)
					  (setq b 76)))
				       (setq d 0)
				       one
				       (setq a 26)
				       two
				       (tagbody
					three
					  (setq b 67)
					  (setf c 56)
					  (go three))))))
    (vl::remove-simple-jump-states sm)
    (is (vl::synthesise-state-machine sm))))


(test test-tagbody-alu
  "Test whether an ALU gets synthesised properly.

This is a RISC-V ALU that should synthesise to a combinatorial
block (i.e., a single state) that keeps running."
  (let* ((p (vl::expand/vl '(tagbody
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
			       (setq aluOut (logand aluIn1 aluIn2)))))))
	 (p-bound (vl::expand/vl `(let (funct3 funct7
					instr
					aluIn1 aluIn2 aluOut shamt)
				    (declare (width 32
						    funct3 funct7
						    instr
						    aluIn1 aluIn2 aluOut shamt))
				    ,p))))

    ;; (let ((sm (vl::build-state-machine (cdr p))))
    ;;   (vl::improve-state-machine sm)
    ;;   (vl::synthesise-state-machine sm)
    ;;   ;sm
    ;;   )

    (with-new-frame
      (vl::declare-variable 'vl::default-variable-type
			    '((kind vl::compiler-flag)
			      (type (unsigned-byte 32))))

      (vl::typecheck p-bound)
      (is (vl::elaborate-state-machines p-bound)))))

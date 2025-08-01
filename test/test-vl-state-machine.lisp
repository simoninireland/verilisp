;; Tests of state machine construction
;;
;; Copyright (C) 2024--2025 Simon Dobson
;;
;; This file is part of verilisp, a very Lisp approach to hardware synthesis
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

(in-package :verilisp/test)
(in-suite verilisp/vl)


;; ---------- State machine construction ----------

(test test-tagbody-simple
  "Test we can create a simple TAGBODY."
  (let ((states (vl::build-state-machine (cdr '(tagbody
						init
						(setq a 10))))))
    (is (= (length states) 2))
    (let ((to-passive (car (last (vl::body (car states))))))
      ;; first state jumps to second
      (is (eql (car to-passive) 'go))
      (is (eql (cadr to-passive) (vl::label (cadr states))))

      ;; second is passivating
      (is (null (vl::body (cadr states)))))))


(test test-tagbody-spin
  "Test we can create a spinning state machine."
  (let ((states (vl::build-state-machine (cdr '(tagbody
						init)))))

    ;; last state is passivating
    (is (null (vl::body (car (last states)))))))


(test test-tagbody-empty
  "Test we can create an empty (also spinning) state machine."
  (let ((states (vl::build-state-machine (cdr '(tagbody)))))

    ;; last state is passivating
    (is (null (vl::body (car (last states)))))))


(test test-tagbody-jumps
  "Test we can create a TAGBODY with jumps."
  (let ((states (vl::build-state-machine (cdr '(tagbody
						init
						(setq a 10)
						pre
						(setq b 20)
						(go init))))))

    (is (= (length states) 3))

    ;; first state jumps to second
    (let ((j0 (car (last (vl::body (elt states 0))))))
      (is (eql (car j0) 'go))
      (is (eql (cadr j0) (vl::label (elt states 1)))))

    ;; second state jumps to first
    (let ((j1 (car (last (vl::body (elt states 1))))))
      (is (eql (car j1) 'go))
      (is (eql (cadr j1) (vl::label (elt states 0)))))

    ;; last state is passivating
    (is (null (vl::body (elt states 2))))))


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


(test test-tagbody-if-two-arms-following
  "Test we can generate an IF with two arms and following code."
  (let ((states (vl::build-state-machine (cdr '(tagbody
					  init
					    (setq a 10)
					    (if b
					      (setq b 32)
					      (setq b 0))
					    (setq d 45)
					  pre
					    (setq b 20))))))

    (is (= (length states) 6))

    ;; first state ends in an if
    (let ((j0 (car (last (vl::body (elt states 0))))))
      (is (eql (car j0) 'if))
      (is (eql (cadr (elt j0 2)) (vl::label (elt states 1))))
      (is (eql (cadr (elt j0 3)) (vl::label (elt states 2)))))

    ;; second and third states jump to fourth
    (let ((j1 (car (last (vl::body (elt states 1))))))
      (is (eql (car j1) 'go))
      (is (eql (cadr j1) (vl::label (elt states 3)))))
    (let ((j2 (car (last (vl::body (elt states 2))))))
      (is (eql (car j2) 'go))
      (is (eql (cadr j2) (vl::label (elt states 3)))))

    ;; last state is passivating
    (is (null (vl::body (elt states 5))))))


(test test-tagbody-if-one-arm-following
  "Test we can handle IFs with a single arm"
  (let ((states (vl::build-state-machine (cdr '(tagbody
					  (if b
					      (setq b 32))
					  (setq b 0))))))

    (is (= (length states) 4))

    ;; first state links to arm and trailing
    (let ((j0 (car (last (vl::body (elt states 0))))))
      (is (eql (car j0) 'if))
      (is (eql (cadr (elt j0 2)) (vl::label (elt states 1))))
      (is (eql (cadr (elt j0 3)) (vl::label (elt states 2)))))

    ;; arm also links to trailing
    (let ((j1 (car (last (vl::body (elt states 1))))))
      (is (eql (car j1) 'go))
      (is (eql (cadr j1) (vl::label (elt states 2)))))

    ;; last state is passivating
    (is (null (vl::body (elt states 3))))))


(test test-tagbody-if-one-arm-no following
  "Test we can handle IFs with a single arm"
  (let ((states (vl::build-state-machine (cdr '(tagbody
						(if b
						    (setq b 32)))))))

    (is (= (length states) 3))

    ;; first state links to arm and trailing
    (let ((j0 (car (last (vl::body (elt states 0))))))
      (is (eql (car j0) 'if))
      (is (eql (cadr (elt j0 2)) (vl::label (elt states 1))))
      (is (eql (cadr (elt j0 3)) (vl::label (elt states 2)))))

    ;; arm also links to trailing
    (let ((j1 (car (last (vl::body (elt states 1))))))
      (is (eql (car j1) 'go))
      (is (eql (cadr j1) (vl::label (elt states 2)))))

    ;; last state is passivating
    (is (null (vl::body (elt states 2))))))


(test test-tagbody-nested-if
  "Test we can construct nested IFs."
  (let ((states (vl::build-state-machine (cdr '(tagbody
						(if b
						    (setq b 32)
						    (if (> c 12)
							(progn
							  (setq a 0)
							  (setq b 1))
							(setq c 0)))
						(setq b 0))))))
    (is (= (length states) 7))

    ;; first state links to arm and trailing
    (let ((j0 (car (last (vl::body (elt states 0))))))
      (is (eql (car j0) 'if))
      (is (eql (cadr (elt j0 2)) (vl::label (elt states 1))))
      (is (eql (cadr (elt j0 3)) (vl::label (elt states 2)))))

    ;; third state links to arm and trailing
    (let ((j2 (car (last (vl::body (elt states 2))))))
      (is (eql (car j2) 'if))
      (is (eql (cadr (elt j2 2)) (vl::label (elt states 3))))
      (is (eql (cadr (elt j2 3)) (vl::label (elt states 4)))))

    ;; last state is passivating
    (is (null (vl::body (elt states 6))))))


(test test-tagbody-case
  "Test we can handle CASE."
  (let ((states (vl::build-state-machine (cdr '(tagbody
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

    (is (= (length states) 7))

    ;; first state links to arms
    (let ((j0 (car (last (vl::body (elt states 0))))))
      (is (= (length j0) 5))
      (is (eql (car j0) 'case))
      (is (eql (cadadr (elt j0 2)) (vl::label (elt states 1))))
      (is (eql (cadadr (elt j0 3)) (vl::label (elt states 2))))
      (is (eql (cadadr (elt j0 4)) (vl::label (elt states 3)))))

    ;; second state links to trailing
    (let ((j2 (car (last (vl::body (elt states 1))))))
      (is (eql (car j2) 'go))
      (is (eql (cadr j2) (vl::label (elt states 4)))))

    ;; fourth state links to sixth
    (let ((j3 (car (last (vl::body (elt states 3))))))
      (is (eql (car j3) 'go))
      (is (eql (cadr j3) (vl::label (elt states 5)))))

    ;; last state is passivating
    (is (null (vl::body (elt states 6))))))


;; ---------- Typechecking ----------

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


;; ---------- Transformation ----------

(test test-tagbody-simple-infinite
  "Test we can generate the simplest infinite loop."
  (let ((p (vl::expand/vl '(let (out
				 (counter 1))
			    (declare (type (unsigned-byte 8) counter))
			    (tagbody
			     initial
			       (go initial))))))
    (vl::typecheck p)
    (is (vl::transform p))))


(test test-tagbody-simple-looping
  "Test we can generate a loop manually."
  (let ((p (vl::expand/vl '(let (out
				 (counter 1))
			    (declare (type (unsigned-byte 8) counter))
			    (tagbody
			     count
			       ;; wait while the counter increments around the counter
			       (incf counter)
			       (if (> counter 0)
				   (go count))

			     blink
			       ;; update the LED
			       (setq out (1+ out))
			       (go count))))))
    (vl::typecheck p)
    (is (vl::transform p))))


(test test-tagbody-while-looping
  "Test we can generate a loop using WHILE (a nested TAGBODY)."
  (let ((p (vl::expand/vl '(let (out
				 (counter 1))
			    (declare (type (unsigned-byte 8) counter))
			    (tagbody
			     looping
			       (while (> counter 0)
				      (incf counter))

			       (setq out (1+ out))
			       (go looping))))))
    (vl::typecheck p)
    (is (vl::transform p))))


(test test-tagbody-forever-looping
  "Test we can generate a loop using FOREVER (a nested TAGBODY)."
  (let ((p (vl::expand/vl '(let (out
			       (counter 1))
			    (declare (type (unsigned-byte 8) counter))
			    (tagbody
			       (forever
				(while (> counter 0)
				       (incf counter))

				(setq out (1+ out))))))))
    (vl::typecheck p)
    (is (vl::transform p))))

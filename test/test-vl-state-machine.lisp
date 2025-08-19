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
			       (setq counter (+ counter 1))
			       (if (> counter 0)
				   (go count))

			     blink
			       ;; update the LED
			       (setq out (+ out 1))
			       (go count))))))

    (vl::typecheck p)
    (let ((q (vl::transform p)))
      (is (contains-form-p '(setq counter (+ counter 1)) q))
      (is (contains-form-p '(setq out (+ out 1)) q)))))


(test test-tagbody-while-looping
  "Test we can generate a loop using WHILE (a nested TAGBODY)."
  (let ((p (vl::expand/vl '(let (out
				 (counter 1))
			    (declare (type (unsigned-byte 8) counter))
			    (tagbody
			     looping
			       (while (> counter 0)
				      (setq counter (+ counter 1)))

			       (setq out (+ out 1))
			       (go looping))))))

    (vl::typecheck p)
    (let ((q (vl::transform p)))
      (is (contains-form-p '(setq counter (+ counter 1)) q))
      (is (contains-form-p '(setq out (+ out 1)) q)))))


(test test-tagbody-forever-looping
  "Test we can generate a loop using FOREVER (a nested TAGBODY)."
  (let ((p (vl::expand/vl '(let (out
				 (counter 1))
			    (declare (type (unsigned-byte 8) counter))
			    (tagbody
			       (forever
				(while (> counter 0)
				       (setq counter (+ counter 1)))

				(setq out (+ out 1))))))))
    (vl::typecheck p)
    (let ((q (vl::transform p)))
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
    (let ((q (vl::transform p)))

      ;; first state
      (is (contains-form-p '(setq a 0) q))

      ;; continuation of first state
      (is (contains-form-p '(setq a 1) q))

      ;; second state
      (is (contains-form-p '(setq c 1) q)))))


(test test-tagbody-nested-if
  "Test we can handle nested IFs."
  (let ((p (vl::expand/vl '(let (a b c)
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
  "Testw e coalesce singleton-state branches of a conditional."
  (with-new-frame
    (let ((p (vl::expand/vl '(let (a b)
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
      (let* ((q (vl::synthesise-state-machine (cdr (elt p 2))))
	     (states (cddr (elt (elt q 3) 3))))
	(is (<= (length states) 3))))))

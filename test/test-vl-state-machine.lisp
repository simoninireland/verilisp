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

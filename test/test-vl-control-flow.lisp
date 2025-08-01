;; Tests of control flow
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


;; ---------- PROGN ----------

(test test-progn-type
  "Test we can typecheck a PROGN correctly."
  (vl::with-new-frame
    (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a 12)
							 (b 34))
						    (+ a b))))
		      '(unsigned-byte 8)))

    (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a 12)
							 (b 34))
						    (+ a b)
						    (- 8))))
		      '(signed-byte 8)))

    ;; the larger of the two arms
    (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((a 7)
							 (b 34))
						    (if a
							(progn
							  256
							  (+ a 1))
							(progn
							  512
							  (+ a 8))))))
		      '(unsigned-byte 5)))))


(test test-synthesise-progn
  "Test we can synthesise PROGN forms."
  (is (vl::synthesise '(progn
		       (setf a 5)
		       (setf b 34)))))


(test test-progn-empty-body
  "Test we can handle (and ignore) an empty-bodied PROGN."
  (vl::with-new-frame
    (is (vl::typecheck (vl::expand/vl '(let ((a 12))))))))


(test test-progn-dependencies
  "Test we can extract dependencies from PROGN bodies, specifically circular updates."
  (vl::with-new-frame
    (vl::declare-variable 'a '((type (unsigned-byte 8))
			       (initial-value 12)))
    (vl::declare-variable 'b '((type (unsigned-byte 8))
			       (initial-value 1)))
    (vl::declare-variable 'c '((type (unsigned-byte 8))
			       (initial-value 45)))
    (vl::declare-variable 'd '((type (unsigned-byte 8))
			       (initial-value 0)))
    (let ((p (vl::expand/vl '(progn
			      (setq a 8)
			      (setq a b)
			      (setq b (+ b c))))))
      (vl::typecheck p)

      ;; b depends on itself
      (is (set-equal (vl::variable-property 'b 'depends-on)
		     '(b c)))

      ;; b as direct dependency...
      (is (set-equal (vl::variable-property 'a 'depends-on)
		     '(b)))

      ;; ... and c when traversed
      (is (set-equal (vl::traverse-dependencies 'a)
		     '(b c))))))


;; ---------- @ ----------

(test test-typecheck-at
  "Test we can type-check triggered blocks."
  ;; single wire sensitivity
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((clk 0)
						       (a 0))
						     (declare (as wire clk))
						     (@ ((posedge clk))
						      (setf a 1)))))
		    '(unsigned-byte 1)))

  ;; multiple wires sensitivity
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((clk 0)
						       (rst 0)
						       (a 0))
						     (declare (as wire clk rst))
						     (@ ((posedge clk) (negedge rst))
						      (setf a 1)))))
		    '(unsigned-byte 1))))


(test test-typecheck-edges
  "Test we can type-check edge trigger expressions."
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((clk 0)
						       (a 0))
						     (declare (as wire clk))
						     (@ ((posedge clk))
						      (setq a clk)))))
		    '(unsigned-byte 1))))


(test test-typecheck-wire-singleton
  "Test we can type-check single-wire triggers."
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((clk 0)
						       (a 0))
						     (declare (as wire clk))
						     (@ clk
						      (setq a clk)))))
		    '(unsigned-byte 1))))


(test test-typecheck-wire-trigger
  "Test we can type-check an edge trigger as a singleton, not in a list."
  (is (vl::subtype-p (vl::typecheck (vl::expand/vl '(let ((clk 0)
						       (a 0))
						     (declare (as wire clk))
						     (@ (posedge clk)
						      (setq a clk)))))
		    '(unsigned-byte 1))))


(test test-synthesise-edges
  "Test synthesis of sensitivies using edges."
  (let ((p (vl::expand/vl '(let ((clk 0)
				(a 0))
			    (declare (as wire clk))
			    (@ ((posedge clk) a)
			     (setq a clk))))))
    (vl::with-new-frame
      (vl::typecheck p)
      (is (vl::synthesise p)))))


(test test-synthesise-wire-singleton
  "Test synthesis of sensitivies using edges."
  (let ((p (vl::expand/vl '(let ((clk 0)
				(a 0))
			    (declare (as wire clk))
			    (@ clk
			     (setq a clk))))))
    (vl::with-new-frame
      (vl::typecheck p)
      (is (vl::synthesise p)))))


(test test-synthesise-wire-trigger
  "Test synthesis of sensitivies using a single trigger."
  (let ((p (vl::expand/vl '(let ((clk 0)
				(a 0))
			    (declare (as wire clk))
			    (@ (posedge clk)
			     (setq a clk))))))
    (vl::with-new-frame
      (vl::typecheck p)
      (is (vl::synthesise p)))))


(test test-at-dependencies
  "Test we can extract dependencies from @ bodies."
  (vl::with-new-frame
    (vl::declare-variable 'a '((type (unsigned-byte 8))
			       (initial-value 12)))
    (vl::declare-variable 'b '((type (unsigned-byte 8))
			       (initial-value 1)))
    (vl::declare-variable 'c '((type (unsigned-byte 8))
			       (initial-value 45)))
    (vl::declare-variable 'd '((type (unsigned-byte 8))
			       (initial-value 0)))
    (vl::declare-variable 'clk '((type (unsigned-byte 1))
				 (initial-value 0)))

    (let ((p (vl::expand/vl '(@ (posedge clk)
			      (setq a 8)
			      (setq a (+ b clk))
			      (setq b (+ b c))))))
      (vl::typecheck p)

      ;; b depends on itself
      (is (set-equal (vl::variable-property 'b 'depends-on)
		     '(b c)))

      ;; direct dependencies...
      (is (set-equal (vl::variable-property 'a 'depends-on)
		     '(b clk)))

      ;; ... and a should also depend on c,after b's later update
      (is (set-equal (vl::traverse-dependencies 'a)
		     '(b c clk))))))


;; ---------- Accesses ----------

(test test-progn-accesses
  "Test we can extract variable accesses from a PROGN."
  (let ((p (vl::read-variables '(progn
				 (setf a (+ 1 2 b))
				 (setf b 23)))))
    (is (equal p '(b)))))


(test test-at-accesses
  "Test we can extract variable accesses from an @."
  (let ((p (vl::read-variables '(@ (posedge clk)
				 (setf a (+ 1 2 b))
				 (setf b 23)))))
    (is (set-equal p '(b clk))))

  (let ((p (vl::read-variables '(@ (*)
				 (setf a (+ 1 2 b))
				 (setf b 23)))))
    (is (equal p '(b)))))

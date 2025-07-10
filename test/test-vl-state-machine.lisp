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


;; ---------- State extraction ----------

(test test-tagbody-states
  "Test we can extract state definitions from a TAGBODY."
  (let ((s0 (vl::extract-tagbody-states '((setq a 1)
					  pre
					  first
					  (setq b 1) (setq s 29)
					  second (go first)))))

    ;; initial label
    (is (vl::initial-p s0))

    ;; states, links, and labels
    (let* ((ss (vl::machine-states s0))
	   (sls (vl::machine-state-labels s0)))
      (is (equal (length ss) 4))

      ;; all labels included
      (dolist (l '(first second pre))
	(is (member l sls)))

      ;; all labels unique
      (is (vl::set-p sls))

      ;; all states have one link except the last
      (let ((all-but-last (remove-if #'(lambda (s)
					 (eql (vl::label s) 'second))
				     ss)))
	(is (every (lambda (s)
		     (= (length (vl::exit-states s)) 1))
		   all-but-last)))
      (is (null (vl::exit-states (find-if #'(lambda (s)
					      (eql (vl::label s) 'second))
					  ss)))))))


(test test-tagbody-no-labels
  "Test we can handle a TAGBODY with no state labels at all."
  (let ((s0 (vl::extract-tagbody-states '((setq a 1)
					  (if a
					      (setq b 23)
					      (setq 4 45))
					  (setq c 1)))))
    (is (not (null s0)))
    (is (not (null (vl::label s0))))))


(test test-tagbody-splt-states
  "Test we can split the states of a machine into single-form states."
  (let* ((s0 (vl::extract-tagbody-states '((setq a 1)
					   pre
					   first
					   (setq b 1) (setq s 29)
					   second (go first))))
	 (s1 (vl::singlify-machine-states s0)))

    ;; returned state preserved, and is still initial
    (is (eql s0 s1))
    (is (vl::initial-p s1))

    (let ((ss (vl::machine-states s1))
	  (sls (vl::machine-state-labels s1)))
      (is (equal (length ss) 5))

      ;; all labels included
      (dolist (l '(first second pre))
	(is (member l sls)))

      ;; all states have one link except the last
      (let ((all-but-last (remove-if #'(lambda (s)
					 (eql (vl::label s) 'second))
				     ss)))
	(is (every (lambda (s)
		     (= (length (vl::exit-states s)) 1))
		   all-but-last)))
      (is (null (vl::exit-states (find-if #'(lambda (s)
					      (eql (vl::label s) 'second))
					  ss)))))))

(let ((s0 (vl::extract-tagbody-states '((setq a 1)
					(if a
					    (setq b 23)
					    (setq 4 45))
					(setq c 1)))))

s0
  )

;; ---------- Basic machines  ----------

(test test-tagbody-compled-form
  "Test we can construct a compiled form of TAGBODY."
  (let* ((p '(tagbody
	      one
	      (setq a 1)
	      (setq b 2)
	      two
	      (setq b 0)))
	 (q (vl::expand/vl `(let (a b)
			      ,p))))

    (setq q (vl::expand-macros-in-environment q))
    (is (not (null q)))))


(test test-go-outside-tagbody
  "Test we can catch a GO out of context."
  (signals (vl::syntax-error)
    (vl::expand/vl '(let ((one 1))
		    (go one)))))


(test test-synthesise-tagbody
  "Teat we can synthesise a TAGBODY."
  (let* ((p '(tagbody
	      one
	      (setq a 1)
	      (setq b 2)
	      two
	      (setq b 0)
	      (go one)))
	 (q (vl::expand/vl `(let (a b)
			     ,p))))

    (vl::typecheck q)
    (is (vl::synthesise q))))


(test test-tagbody-float
  "Test we float let blocks successfully when synthesising."
  (let* ((p (vl::expand/vl '(module test/456 (clk)
			     (declare (direction in clk))
			     (let ((a 0)
				   (b 0))
			       (setq a 1)
			       (setq b 34)

			       (@ (posedge a)
				  (let ((c 0))
				    (tagbody
				     one
				       (setq a 1)
				       (setq b 2)
				       (go two)
				     two
				       (setq b 0)
				       (go one)
				     three
				       (go two)))))))))

    (is (vl::elaborate/vl p))))


;; ---------- Nested machines ----------

(test test-tagbody-simple-nested
  "Test we can escape from a nested machine."
  (let* ((p (vl::expand/vl '(let (a b c)
			    (tagbody
			     one
			       (setq a 1)
			       (setq b 2)
			     two
			       (let (d)
				 (tagbody
				  inner-one
				    (setq d 10)

				  inner-two
				    (decf d)
				    (if (= d 0)
					(go three)
					(go inner-two))))

			     three
			       (setq b 0)
			       (go one))))))

    (setq p (vl::expand-macros-in-environment p))
    (vl::typecheck p)
    (is (vl::synthesise p))))

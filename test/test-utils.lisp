;; Tests of utility functions
;;
;; Copyright (C) 2023--2025 Simon Dobson
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
(in-suite verilisp/utils)
(declaim (optimize (space 0)))


;; ---------- Looking for non-nil elements of lists  ----------

(test test-index-non-nil
  "Test w can find non-nil elements of lists."
  (is (null (index-non-nil '())))
  (is (null (index-non-nil '(nil))))
  (is (null (index-non-nil '(nil nil nil nil))))

  (is (equal (index-non-nil '(1)) 0))
  (is (equal (index-non-nil '(1 nil)) 0))
  (is (equal (index-non-nil '("str" nil)) 0))
  (is (equal (index-non-nil '(nil 1)) 1))
  (is (equal (index-non-nil '(1 nil)) 0))
  (is (equal (index-non-nil '(nil 1 nil)) 1))
  (is (equal (index-non-nil '(nil nil 1 nil)) 2)))


(test test-non-nil-subseq
  "Test we can extract non-nil sub-sequences of lists."
  (is (null (non-nil-subseq '())))
  (is (null (non-nil-subseq '(nil))))
  (is (null (non-nil-subseq '(nil nil nil))))

  (is (equal (non-nil-subseq '(nil 1 nil)) '(1)))
  (is (equal (non-nil-subseq '(nil 1)) '(1)))
  (is (equal (non-nil-subseq '(nil 1 2)) '(1 2)))
  (is (equal (non-nil-subseq '(nil 1 nil 1 2)) '(1)))
  (is (equal (non-nil-subseq '(nil 1 2 3 nil 1 2)) '(1 2 3))))


;; ---------- Changing list depths ----------

(test test-listify
  "Test we can increase depth."
  (is (null (listify '())))
  (is (equal (listify '(1 2 3))
	     '((1) (2) (3))))
  (is (equal (listify '(1 (2 3) 4))
	     '((1) ((2 3)) (4)))))


(test test-delistify
  "Test we can decrease depth."
  (is (null (delistify '())))
  (is (equal (delistify '((1) (2) (3)))
	     '(1 2 3)))
  (is (equal (delistify '((1) ((2 3)) (4)))
	     '(1 (2 3) 4))))


;; ---------- Zipping while ignoring nulls ----------

(test test-zip-straight
  "Test we can zip lists without nulls."
  (is (equal (zip-without-null '(1 2 3) '(4 5 6))
	     '((1 4) (2 5) (3 6))))
  (is (equal (zip-without-null '(1 (2 3) 4) '(5 6 7))
	     '((1 5) ((2 3) 6) (4 7)))))


(test test-zip-null
  "Test we can zip lists with nulls."
  (is (equal (zip-without-null '(1 nil 3) '(4 5 6))
	     '((1 4) (3 6))))
  (is (equal (zip-without-null '(1 2 3) '(4 5 nil))
	     '((1 4) (2 5)))))


(test test-zip-unequal
  "Test we can zip unequal-length lists with nulls."
  (is (equal (zip-without-null '(1 nil 3) '(4 5 6 7 8))
	     '((1 4) (3 6)))))


(test test-zip-all-null
  "Test we can zip lists that are all nulls"
  (is (null (zip-without-null '(nil nil nil) '(1 2 3))))
  (is (null (zip-without-null '(1 2 3) '(nil nil nil)))))


(test test-zip-empty
  "Test we can zip one empty list."
  (is (null (zip-without-null '(1 2 3) nil)))
  (is (null (zip-without-null nil '(1 2 3)))))


;; ---------- safe-cadr etc ----------

(test test-safe-cadr
  "Test we can safely extract the second element of a pair or list."
  (is (equal (safe-cadr (cons 1 2)) 2))
  (is (equal (safe-cadr (cons 1 (cons 2 3))) 2))
  (is (equal (safe-cadr (list 1 2)) 2))
  (is (equal (safe-cadr (list 1 2 3)) 2))
  (is (null (safe-cadr (list 1))))
  (is (null (safe-cadr (cons 1 nil)))))


(test test-safe-car
  "Test we can safely extact the heads of (non-)lists."
  (is (equal (safe-car (cons 1 2)) 1))
  (is (equal (safe-car (list 1 2)) 1))
  (is (equal (safe-car 1) 1)))


(test test-safe-car-cdr
  "Test we can safely split (non-)lists."
  (is (equal (safe-car-cdr (list 1 2 3)) (list 1 (list 2 3))))
  (is (equal (safe-car-cdr (list 1 (list 2 3))) (list 1 (list (list 2 3)))))
  (is (equal (safe-car-cdr (cons 1 2)) (list 1 (list 2))))
  (is (equal (safe-car-cdr 1) (list 1 nil)))
  (is (null (safe-cadr (safe-car-cdr 1))))
  (is (null (cadr (safe-car-cdr 1)))))


;; ---------- Flat maps ----------

(test test-mapappend-as-mapcar
  "Test mapappend defaults to working like mapcar"
  (let ((l (list 1 2 3 4 5)))
    (is (equal (mapcar #'1+ l)
	       (mapappend #'1+ l)))))


(test test-mapappend-flattens
  "Test that mapappend flattens its results."
  (let ((l (list 1 2 (list 3 4) 5)))
    (is (equal (mapcar #'1+ (flatten l))
	       (mapappend #'1+ l)))))


(test test-mapappend-flattens-leading-list
  "Test that mapappend flattens its results with a leading list."
  (let ((l (list (list 1 2) (list 3 4) 5)))
    (is (equal (mapcar #'1+ (flatten l))
	       (mapappend #'1+ l)))))


;; ---------- Last maps ----------

(test test-mapn-one
  "Test we can extract the last value of mapping over a single list."
  (is (= (mapn (lambda (a) (1+ a)) '(1 2 3))
	 4))
  (is (null (mapn (lambda (a) (1+ a)) '()))))


(test test-mapn-two
  "Test we can extract the last value of mapping over two lists."
  (is (= (mapn #'+ '(1 2 3) '(4 5 6))
	 9)))


;; ---------- Pairwise applications ----------

(test test-union2-empty
  "Test we can union empty pairs."
  (is (equal (union2 '(() ()) '(() ())) '(() ()))))


(test test-union2-one-side
  "Test we can union empty and non-empty pairs."
  (is (equal (union2 '((1 2) (3)) '(() ())) '((1 2) (3)))))


(test test-union2
  "Test we can union a pair of pairs of sets."
  (let ((p (union2 '((1 2) (3)) '((4) (5 6)))))
    (destructuring-bind (p1 p2)
	p
      (is (set-equal p1 '(1 2 4)))
      (is (set-equal p2 '(3 5 6)))))

  ;; no duplicates
  (let ((p (union2 '((1 2 3) (3)) '((4 3) (5 6)))))
    (destructuring-bind (p1 p2)
	p
      (is (set-p p1))
      (is (set-p p2))
      (is (set-equal p1 '(1 2 3 4)))
      (is (set-equal p2 '(3 5 6))))))


;; ---------- Repetition ----------

(test test-n-copies-atom
  "Test we can replicate atoms."
  (is (equal (n-copies 5 3)
	     '(5 5 5)))
  (is (equal (n-copies 'var 3)
	     '(var var var)))
  (is (equal (n-copies "bish" 5)
	     '("bish" "bish" "bish" "bish" "bish"))))


(test test-n-copies-list
  "Test we can replicate lists, and they're independent."
  (let ((l '(1 2 3)))
    (is (equal (n-copies l 3)
	       '((1 2 3) (1 2 3) (1 2 3))))

    (is (equal (n-copies nil 4)
	       '(() () () ())))

    (let* ((l2 (copy-list '(1 2 3)))
	   (rep  (n-copies l2 3)))
      ;; changing the original doesn't change the replicas
      (setf (car l2) 4)
      (is (equal rep
		 '((1 2 3) (1 2 3) (1 2 3))))

      ;; changing one of the replicas doesn't change the others
      (setf (car (car rep)) 4)
      (is (equal rep
		 '((4 2 3) (1 2 3) (1 2 3))))

      ;; adding an element to a list doesn't change the others
      (appendf (car rep) (list 5 6))
      (is (equal rep
		 '((4 2 3 5 6) (1 2 3) (1 2 3))))

      ;; we can add an element to one repeated nil
      (let ((repnil (n-copies nil 5)))
	(appendf (cadr repnil) (list 1 2))
	(is (equal repnil
		   '(() (1 2) () () ())))))))


;; ---------- Filtering by predicates ----------

(test test-filter
  "Test we can filter by predicates."
  (is (equal (filter-by-predicates '(1 2 3 4 5 6 7)
				   #'oddp #'evenp)
	     '((1 3 5 7) (2 4 6))))

  ;; repetition
  (is (equal (filter-by-predicates '(1 2 3 4 1 5 6 7)
				   #'oddp #'evenp)
	     '((1 3 1 5 7) (2 4 6))))

  ;; one predicate never satisfied
  (is (equal (filter-by-predicates '(1 3 5 7)
				   #'oddp #'evenp)
	     '((1 3 5 7) ())))

  ;; one value satisfying multiple predicates (0 is even and zero)
  (is (equal (filter-by-predicates '(1 3 0 5 7)
				   #'oddp #'evenp #'zerop)
	     '((1 3 5 7) (0) (0)))))


;; ---------- Sub-lists ----------

(test test-sublist
  "Test we can extract sub-lists."
  (is (equal (sublist '(1 2 3 4 5) 1 3)
	     '(2 3 4)))
  (is (equal (sublist '(1 2 3 4 5) 1)
	     '(2 3 4 5)))
  (is (equal (sublist '(1 2 3 4 5) 0)
	     '(1 2 3 4 5)))
  (is (null (sublist '(1 2 3 4 5) 5))))


(test test-successive-pairs
  "Test we correctly form successive pairs."
  (is (null (successive-pairs nil)))

  (is (equal (successive-pairs '(a b))
	     '((a b))))
  (is (equal (successive-pairs '(a b c d))
	     '((a b) (b c) (c d))))

  (is (equal (successive-pairs '((a b) (c d) (d e)))
	     '(((a b) (c d)) ((c d) (d e)))))
  (is (equal (successive-pairs '((a b) (c d) d))
	     '(((a b) (c d)) ((c d) d))))

  (signals (error)
    (successive-pairs '(a))))


(test test-adjacent-pairs
  "Test we can form adjacent pairs."
  (is (null (adjacent-pairs '())))
  (is (equal (adjacent-pairs '(1 2))
	     '((1 2))))
  (is (equal (adjacent-pairs '(1 2 3 4))
	     '((1 2) (3 4))))

  (is (equal (adjacent-pairs '((1) 2 3 (4)))
	     '(((1) 2) (3 (4)))))

  (signals (error)
    (adjacent-pairs '(1 2 3))))


;; ---------- Merging alists ----------

(test test-merge-alists-empty
  "Test we can merge alists where one or both are empty."
  (is (set-equal (merge-alists '((a 1) (b 2) (c 3))
			       nil)
		 '((a 1) (b 2) (c 3))
		 :test #'equal))
  (is (set-equal (merge-alists nil
			       '((a 1) (b 2) (c 3)))
		 '((a 1) (b 2) (c 3))
		 :test #'equal))
  (is (null (merge-alists nil nil))))


(test test-merge-alist-unique
  "Test merging two alists without shared keys."
  (is (set-equal (merge-alists '((a 1) (b 2) (c 3))
			       '((d 4) (e 5) (f 6)))
		 '((a 1) (b 2) (c 3) (d 4) (e 5) (f 6))
		 :test #'equal)))


(test test-merge-alists-default-override
  "Test merging alists with the default overide behaviour."
  (is (set-equal (merge-alists '((a 1) (b 2) (c 3))
			       '((d 4) (b 5) (f 6)))
		 '((a 1) (b 5) (c 3) (d 4) (f 6))
		 :test #'equal)))


(test test-merge-alists-override
  "Test merging alists with a specific overide behaviour."
  (flet ((merge-by-adding (v1 v2)
	   (list (+ (car v1) (car v2)))))

    (is (set-equal (merge-alists '((a 1) (b 2) (c 3))
				 '((d 4) (b 5) (f 6))
				 :merge #'merge-by-adding)
		   '((a 1) (b 7) (c 3) (d 4) (f 6))
		   :test #'equal))))


;; ---------- Folding over null values ----------

(test test-max-null
  "Test we can do a max in the presence of nulls."
  (is (null (max-null '())))
  (is (= (max-null 1 2 3) 3))
  (is (= (max-null 1 2 nil 4 3) 4)))


(test test-min-null
  "Test we can do a min in the presence of nulls."
  (is (null (min-null '())))
  (is (= (min-null 1 2 3) 1))
  (is (= (min-null 5 2 nil 4 1) 1)))


;; ---------- Assoc over decls ----------

(test test-assoc-decls
  "Test we can perform ASSOC over decls."
  ;; null list
  (is (null (assoc-decls 'a '())))

  ;; normal assoc
  (is (equal (assoc-decls 'a '((a 1) (b 2) (c 3)))
	     '(a 1)))
  (is (equal (assoc-decls 'b '((a 1) (b 2) (c 3)))
	     '(b 2)))
  (is (null (assoc-decls 'd '((a 1) (b 2) (c 3)))))

  ;; assoc with singletons
  (is (equal (assoc-decls 'a '((a 1) b (c 3)))
	     '(a 1)))
  (is (equal (assoc-decls 'b '((a 1) b (c 3)))
	     'b))
  (is (null (assoc-decls 'd '((a 1) b (c 3))))))


;;; ---------- Maths operations ----------

(test test-bit-array-to-integer
  "Test we can convert bit arrays to integers properly."
  (let ((a (make-array '(8) :element-type 'bit)))
    (is (= (bit-array-to-integer a) 0))

    (setf (aref a 3) 1)
    (is (= (bit-array-to-integer a) 8))

    (setf (aref a 0) 1)
    (is (= (bit-array-to-integer a) 9))

    (setf (aref a 0) 0)
    (is (= (bit-array-to-integer a) 8))))


(test test-integer-to-bit-array
  "Test we can convert integers to bit arrays."
  ;; natural widths
  (is (equal (integer-to-bit-array 0) #*0))
  (is (equal (integer-to-bit-array 1) #*1))
  (is (equal (integer-to-bit-array 10) #*0101))

  ;; fixed widths
  (is (equal (integer-to-bit-array 0 3) #*000))
  (is (equal (integer-to-bit-array 1 3) #*100))
  (is (equal (integer-to-bit-array 9 6) #*100100))

  ;; precision
  (is (equal (integer-to-bit-array 9 2) #*10)))


(test tes-bit-array-integer-duality
  "Test the coercions are reversible."
  (is (equal (bit-array-to-integer (integer-to-bit-array 9)) 9))

  (let ((a (make-array '(8) :element-type 'bit)))
    (setf (aref a 1) 1)
    (setf (aref a 3) 1)
    (is (equal (integer-to-bit-array (bit-array-to-integer a) 8) a))))

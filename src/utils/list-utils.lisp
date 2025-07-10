;; Helper functions and macros
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

(in-package :verilisp/utils)

;; ---------- Searching lists for non-nil ----------

(defun index-non-nil (l)
  "Return the index (0-based) of the first non-nil element of sequnce L."
  (dolist (i (iota (length l)))
    (if (not (null (elt l i)))
	(return i))))


(defun non-nil-subseq (l)
  "Return the non-nil sub-sequence of L.

This will extract the first such sub-sequence, beginning from the index
extracted by INDEX-NOT-NIL and containing all elements up to the
next nil element of the end of the sequence."
  (when-let ((i (index-non-nil l)))
    (dolist (j (iota (- (length l) (1+ i)) :start 1))
      (if (null (elt l (+ i j)))
	  (return-from non-nil-subseq (subseq l i (+ i j)))))

    ;; if we get here, the rest of the list is non-nil
    (subseq l i)))


;; ---------- Remove duplicates and nil values from a sequence ----------

(defun uniquify (s)
  "Remove duplicates and nils from sequence S."
  (remove-if #'null (remove-duplicates s)))


;; ---------- Increasing and decreasing list nesting ----------

(defun listify (l)
  "Wrap each element of L in a singleton list."
  (mapcar #'list l))


(defun delistify (l)
  "Remove a level of nesting from each element of L."
  (mapcar #'car l))


(defun flatten1 (l)
  "Flatten one level of lists embededed in L.

So (FLATTEN1 '((1 2) (3 (4)))) evaluates to (1 2 3 (4)), and
(FLATTEN1 '((1 2) 3 (4 5))) evaluates to (1 2 3 4 5)."
  (flet ((append1 (l1 l2)
	   (append l1 (if (atom l2)
			  (list l2)
			  l2))))

    (foldr #'append1 l '())))


;; ---------- Zipping in the presence of null ----------

(defun zip (l1 l2)
  "Zip corresponding elements of L1 and L2.

The lists must have equal lengths."
  (cond ((null l1)
	 (if (null l2)
	     '()
	     (error "Lists have unequal lengths whe zipping (~a and ~a)" l1 l2)))

	((null l2)
	 (error "Lists have unequal lengths whe zipping (~a and ~a)" l1 l2))

	(t
	 (cons (list (car l1) (car l2))
	       (zip (cdr l1) (cdr l2))))))

(defun zip-without-null (xs ys)
  "Zip lists XS and YS when elements are not null.

If either element is null, the pair is omitted."
  (when (not (or (null xs)
		 (null ys)))
    (if (or (null (car xs))
	    (null (car ys)))
	(zip-without-null (cdr xs) (cdr ys))

	(cons (list (car xs) (car ys))
	      (zip-without-null (cdr xs) (cdr ys))))))


;; ---------- Filtering nulls ----------

(defun remove-nulls (l)
  "Remove all sub-lists of L that are nil."
  (remove-if #'null l))


;; ---------- Set testing ----------

(defun set-p (s &key (test #'eql) (key #'identity))
  "Test whether S is a set, composed of elaments unequal to any other.

If TEST is set, it should designate a function of two variables that
test those values for equality, The default is EQL. If KEY is
set, it is applied to each element before applying TEST. The default
is IDENTITY, testing the values themselves."
  (every (lambda (e)
	   (= (count (funcall key e) s :test test :key key) 1))
	 s))


;; ---------- Pairwise application across several sets ----------

(defun pairwise (f l1 l2)
  "Apply F pairwise to the elements of L1 and L2.

F should be a function of two variables. L1 and L2 must
have the same length."
  (mapcar (curry #'funcall f) l1 l2))


(defun union2 (p1 p2)
  "Take two pairs of sets and return the pair of their union."
  (pairwise #'union p1 p2))


;; ---------- Repetition ----------

(defun n-copies (l n)
  "Return a list consisting of N copies of L.

L may be an atom or a list, including NIL."
  (mapcar (lambda (i)
	    (declare (ignore i))
	    (if (listp l)
		(copy-tree l)
		l))
	  (iota n)))


;; ---------- Filtering on multiple predicates ----------

(defun filter-by-predicates (l &rest predicates)
  "Return sub-lists of L matching PREDICATES.

The return value is a list of sub-lists, one per predicate.
Each sub-list consists of all the elements of L that match
the corresponding predicate.

If an element of L satisfies several of PREDICATES it
will appear several times, in each list. If elekents of L
are duplicated, each suplicate will appear."
  (mapcar (lambda (pred)
	    (remove-if (lambda (v)
			 (not (funcall pred v)))
		       l))
	  predicates))


;; ---------- Second element of pair or list ----------

;; Neither of `elt' or `cadr' ar safe when applied to pairs.

(defun safe-cadr (l)
  "Return the second element of L.

L can be a list or a pair."
  (if (consp (cdr l))
      (cadr l)
      (cdr l)))


(defun safe-car (l)
  "Return the car of L if it is a list, or L itself if it is an atom."
  (if (listp l)
      (car l)
      l))


(defun safe-car-cdr (l)
  "Return a list of the car of L and its cdr (as a list).

If L is not a list, return a list of L and nil. If L is
a pair, its cdr is returned as a list anyway."
  (if (listp l)
      (list (car l) (let ((ls (cdr l)))
		      (cond ((null ls)
			     (list '()))
			    ((listp ls)
			     ls)
			    (t
			     (list ls)))))
      (list l)))


;; ---------- Flat maps ----------

(defun mapappend (f &rest ls)
  "Apply F to all elements of lists LS at whatever depth, returning a flat list of results."
  (flatten (mapcar f (flatten ls))))


;; ---------- A map returning only the last result ----------

;; this could be a lot more optimised
(defun mapn (fun &rest lists)
  "Map FUN across the cars of LISTS, returning the value of the last application.

If the lists are empty, the result is NIL.

The name MAPN is supposed to bring to mind the behaviour of PROGN."
  (let ((res (apply #'mapcar (cons fun lists))))
    (if (not (null res))
	(car (last res)))))


;; ---------- Folds ----------

;; These are just wrappers around REDUCE, but I find them easier to remember.

(defun foldl (fun l init)
  "Fold the values of L left through FUN, starting with initial value INIT."
  (reduce fun l :from-end t :initial-value init))


(defun foldr (fun l init)
  "Fold the values of L rightwards through FUN, starting with INIT."
  (reduce fun l :initial-value init))


(defun foldr-over-null (fun l init)
  "Fold FUN right over L starting with INIT, ignoring nulls.

This is like FOLDR except that a null in either the accumulated total
or one of the values automatically returns the other value."
  (flet ((fun-null (a v)
	   (cond ((null a)
		  v)
		 ((null v)
		  a)
		 (t
		  (funcall fun a v)))))

    (foldr #'fun-null l init)))


;; ---------- max and min in the presence of null ----------

(defun max-null (&rest vs)
  "Return the maximum of values VS, where NIL is less than any value."
  (foldr-over-null #'max vs nil))


(defun min-null (&rest vs)
  "Return the minimum of values VS, where NIL is greater than any value."
  (foldr-over-null #'min vs nil))


;; ---------- Sub-lists ----------

(defun sublist (l start &optional end)
  "Extract the sub-list of L starting at START and ending at END or the end."
  (let ((n (length l)))
    (unless end
      (setq end (1- n)))
    (let ((l1 (butlast l (1- (- n end)))))
      (last l1 (1+ (- end start))))))


(defun successive-pairs (l)
  "Turn a list L into a list of successive pairs.

The list (a b c d) transforms to the list ((a b) (b c) (c d))."
  (labels ((pairup (l)
	     (if (null l)
		 nil
		 (cons (list (car l) (cadr l))
		       (if (> (length l) 2)
			   (pairup (cdr l))
			   nil)))))

    (cond ((null l)
	   nil)
	  ((= (length l) 1)
	   (error "No pairs to pair up in ~a" l))
	  (t
	   (pairup l)))))


(defun adjacent-pairs (l)
  "Turn a list L into a list of adjacent pairs.

The list (a b c d) transforms to the list ((a b) (c d))."
  (labels ((pairup (l)
	     (cond ((null l)
		    nil)
		   ((= (length l) 1)
		    (error "Uneven list ~a" l))
		   (t (cons (list (car l) (cadr l))
			    (pairup (cddr l)))))))

    (if (null l)
	nil
	(pairup l))))


(defun find-if-list (pred l)
  "Find the first element of L matching PRED.

PRED should be a function accepting a list argument. Return the
sub-list whose head is the first match.

This function is like FIND-IF, but works with only one list and allows
PRED access to each sub-list rather than each element for making its
decision.
"
  (labels ((fil (l)
	     (cond ((null l)
		    nil)

		   ((funcall pred l)
		    l)

		   (t
		    (fil (cdr l))))))

    (fil l)))


;; ---------- Alist access, updating, and merging ----------

(defun alist-keys (al)
  "Return the keys in alist AL."
  (mapcar #'car al))


(defun merge-alists (a1 a2 &key merge)
  "Merge the values in A1 and A2 to form a new composite alist.

Keys unique to either alist are added unchanged. Common keys have their
CDRs passed to the MERGE function, which should return the new value.
By default MERGE simply returns the value from A2, so common keys in
get their value in A2."
  (flet ((default-merge (v1 v2)
	   "Return V2 as the merged value."
	   (declare (ignore v1))
	   v2))

    (let* ((ks1 (alist-keys a1))
	   (ks2 (alist-keys a2))
	   (ks1only (remove-if #'(lambda (k)
				   (member k ks2))
			       ks1))
	   (ks2only (remove-if #'(lambda (k)
				   (member k ks1))
			       ks2))
	   (ks (remove-if #'(lambda (k)
			      (null (member k ks2)))
			  ks1)))

      (append (mapcar (rcurry #'assoc a1) ks1only)
	      (mapcar (rcurry #'assoc a2) ks2only)
	      (mapcar #'(lambda (k)
			  (cons k
				(funcall (or merge #'default-merge)
					 (cdr (assoc k a1))
					 (cdr (assoc k a2)))))
		      ks)))))

;; Package definition for utilities
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

(in-package :common-lisp-user)

(defpackage verilisp/utils
  (:use :cl :alexandria)
  (:export
   ;; list utilities
   #:index-non-nil
   #:non-nil-subseq
   #:uniquify
   #:listify
   #:delistify
   #:flatten1
   #:zip
   #:zip-without-null
   #:remove-nulls
   #:set-p
   #:duplicates
   #:pairwise
   #:union2
   #:foldl
   #:foldr
   #:foldr-over-null
   #:max-null
   #:min-null
   #:mapn
   #:n-copies
   #:filter-by-predicates
   #:sublist
   #:find-if-list
   #:alist-keys
   #:successive-pairs
   #:adjacent-pairs

   ;; data structure manipulations
   #:safe-car
   #:safe-cadr
   #:safe-car-cdr
   #:mapappend
   #:merge-alists

   ;; string functions
   #:string-times
   ))

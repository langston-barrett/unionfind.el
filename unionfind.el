;;; unionfind.el --- Union-find (disjoint-set) data structure -*- lexical-binding: t -*-

;; Copyright © 2021 Langston Barrett <langston.barrett@gmail.com>

;; Author: Langston Barrett <langston.barrett@gmail.com>
;; URL: https://github.com/langston-barrett/unionfind.el
;; Keywords: lisp
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides a union-find (disjoint set) data structure in Emacs
;; Lisp.

;; API:
;;
;; The following operations are exposed:
;;
;; - `unionfind-of': Create a new union-find data structure containing given values
;; - `unionfind-count': Count the number of elements in this union-find structure
;; - `unionfind-insert': Add a new value to the union-find structure
;; - `unionfind-find': Get the canonical representative of a given value
;; - `unionfind-equiv-p': Check if two values share a canonical representative
;; - `unionfind-merge': Merge the equivalence classes of two values
;; - `unionfind-merge-canonical': Merge the equivalence classes of two canonical
;;   representatives
;; - `unionfind-to-hash-set': Create a two-level nested hash set representing the
;;   partition/equivalence classes.

;; Performance:
;;
;; This library implements an imperative (pointer-based) union-find structure,
;; the complexity of the operations are as follows:
;;
;; - `unionfind-of': O(n) in the number of values
;; - `unionfind-insert': O(1) (amortized)
;; - `unionfind-count': O(1) (amortized)
;; - `unionfind-find': O(1) (amortized)
;; - `unionfind-equiv-p': O(1) (amortized)
;; - `unionfind-merge': O(1) (amortized)
;; - `unionfind-to-hash-set': O(n^2)

;; Dependencies:
;;
;; This library has no dependencies, but will use contract.el if it's available.

;; Development:
;;
;; PRs and issues welcome! Develop using Cask. Here are a few Makefile targets:
;;
;;     make .cask/
;;     make build
;;     make test
;;
;; Tests are provided using doctest, buttercup, and propcheck.

;;; Code:

;;;; Imports

(eval-and-compile
  (require 'cl-lib))

;; Use contracts if they're available, otherwise ignore them.
(eval-and-compile
  (if (require 'contract nil 'noerror)
      (progn
        (defconst unionfind--have-contracts t)
        (defalias 'unionfind--contract-defun 'contract-defun))
    (defconst unionfind--have-contracts nil)
    (defmacro unionfind--contract-defun (name arguments &rest forms)
      (when (equal (car forms) :contract)
        (pop forms)
        (pop forms))
      `(defun ,name ,arguments ,@forms))))

(cl-defstruct (unionfind-datum
               (:constructor unionfind--datum))
  "Data in a union-find structure."
  (parent
   nil
   :documentation "Pointer to parent, or self if this is a root.")
  (rank
   0
   :documentation "Rank.")
  (value
   nil
   :read-only t
   :documentation "Underlying value."))

(when unionfind--have-contracts
  (defconst
    unionfind-datum-c
    (contract-predicate
     #'unionfind-datum-p
     "Expected a `unionfind-datum', but got %s"
     'unionfind-datum-c
     nil
     t)))

(cl-defstruct (unionfind-unionfind
               (:constructor unionfind--new))
  "The union-find datastructure."
  (data
   nil
   :documentation "Hash-set of data."))

(when unionfind--have-contracts
  (defconst
    unionfind-unionfind-c
    (contract-predicate
     #'unionfind-unionfind-p
     "Expected a `unionfind-unionfind', but got %s"
     'unionfind-unionfind-c
     nil
     t)))

(defsubst unionfind--maphash (func ht)
  "Map FUNC over HT, collecting the results."
  (let ((result nil))
    (maphash
     (lambda (k v) (push (funcall func k v) result))
     ht)
    result))

(defsubst unionfind--data (uf)
  "An abbreviation for `unionfind-unionfind-data' on UF."
  (unionfind-unionfind-data uf))

(unionfind--contract-defun
 unionfind-count
 (uf)
 :contract (contract-> unionfind-unionfind-c contract-nat-number-c)
 "Count the number of elements in UF.

The complexity of this operation is O(1)."
 (hash-table-count (unionfind--data uf)))

(unionfind--contract-defun
 unionfind--canonicalize
 (datum)
 :contract (contract-> unionfind-datum-c contract-any-c)
 "Canonicalize DATUM."
 (while (not (equal datum (unionfind-datum-parent datum)))
   (setq datum (unionfind-datum-parent datum)))
 datum)

(when unionfind--have-contracts
  (defconst
    unionfind-canonical-c
    (contract-predicate
     (lambda (datum)
       (and
        (unionfind-datum-p datum)
        (equal datum (unionfind--canonicalize datum))))
     "Expected a canonical representative datum, but got %s"
     'unionfind-canonical-c
     nil
     t)))

(unionfind--contract-defun
 unionfind-insert
 (uf value)
 :contract (contract->
            unionfind-unionfind-c
            contract-any-c
            unionfind-canonical-c)
 "Add VALUE to the union-find UF and return its canonical representative.

This operation is idempotent.

The complexity of this operation is O(1) (amortized)."
 (let ((got (gethash value (unionfind--data uf))))
   (if (unionfind-datum-p got)
       (unionfind--canonicalize got)
     (let ((datum (unionfind--datum :value value)))
       (puthash value datum (unionfind--data uf))
       (setf (unionfind-datum-parent datum) datum)))))

(unionfind--contract-defun
 unionfind-of
 (&optional test &rest values)
 :contract contract-any-c
 "Create a new union-find data structure with equality test TEST and containing
VALUES.

The default for TEST is `eql'.

The complexity of this operation is O(n) in the number of values."
 (let ((uf (unionfind--new
            :data (if (not test)
                      (make-hash-table)
                    (make-hash-table :test test)))))
   (cl-loop
    for value in values
    do (unionfind-insert uf value))
   uf))

(unionfind--contract-defun
 unionfind-find
 (uf value)
 :contract (contract-> unionfind-unionfind-c contract-any-c unionfind-canonical-c)
 "Get the canonical representative of VALUE in UF.

The canonical representatives of two different values in the same equivalence
classes will be `equal'.

Returns nil if VALUE is not in UF.

The complexity of this operation is O(1) (amortized)."
 (let ((got (gethash value (unionfind--data uf))))
   (if (unionfind-datum-p got)
       (unionfind--canonicalize got)
     nil)))

(unionfind--contract-defun
 unionfind-equiv-p
 (uf value1 value2)
 :contract (contract->
            unionfind-unionfind-c
            contract-any-c
            contract-any-c
            contract-any-c)
 "Check if VALUE1 and VALUE2 share a canonical representative in UF.

The complexity of this operation is O(1) (amortized)."
 (eq
  (unionfind-find uf value1)
  (unionfind-find uf value2)))

(unionfind--contract-defun
 unionfind-merge-canonical
 (_uf repr1 repr2)
 :contract (contract->
            unionfind-unionfind-c
            unionfind-canonical-c
            unionfind-canonical-c
            contract-nil-c)
 "Merge the equivalence classes represented by REPR1 and REPR2 in UF.

The complexity of this operation is O(1) (amortized)."
 (unless (eq repr1 repr2)
   (when (< (unionfind-datum-rank repr1) (unionfind-datum-rank repr2))
     (let ((tmp repr1))
       (setq repr1 repr2)
       (setq repr2 tmp)))
   (setf (unionfind-datum-parent repr2) repr1)
   (when (equal (unionfind-datum-rank repr1) (unionfind-datum-rank repr2))
     (cl-incf (unionfind-datum-rank repr1))
     nil)))

(unionfind--contract-defun
 unionfind-merge
 (uf value1 value2)
 :contract (contract->
            unionfind-unionfind-c
            contract-any-c
            contract-any-c
            contract-nil-c)
 "Merge the equivalence classes of VALUE1 and VALUE2 in UF.

The complexity of this operation is O(1) (amortized)."
 (unionfind-merge-canonical
  uf
  (unionfind-find uf value1)
  (unionfind-find uf value2)))

(unionfind--contract-defun
 unionfind--show
 (uf)
 :contract (contract-> unionfind-unionfind-c contract-string-c)
 "Show UF; for debugging purposes only."
 (string-join
  (unionfind--maphash
   (lambda (value datum)
     (format
      "%s -> %s"
      value
      (unionfind-datum-value
       (unionfind-datum-parent datum))))
   (unionfind--data uf))
  "\n"))

(unionfind--contract-defun
 unionfind-to-hash-set
 (uf)
 :contract (contract-> unionfind-unionfind-c contract-any-c)
 "Create a two-level nested hash set containing each equivalence class in UF.

The complexity of this operation is O(n^2)."
 (let ((ret (make-hash-table)))
   (unionfind--maphash
    ;; For each datum,
    (lambda (_ val)
      (let ((canon (unionfind-datum-value
                    (unionfind--canonicalize val)))
            (inserted nil))
        (unionfind--maphash
         ;; look through each equivalence class;
         (lambda (class _t)
           ;; if it contains the datum's representative,
           (when (gethash canon class)
             ;; then insert that datum there too;
             (puthash val t class)
             (setq inserted t)))
         ret)
        ;; and if none of the classes contains it, make a new class.
        (unless inserted
          (puthash
           (make-hash-table
            :test (hash-table-test (unionfind--data uf)))
           t
           ret))))
    (unionfind--data uf))
   ret))

(provide 'unionfind)

;;; unionfind.el ends here

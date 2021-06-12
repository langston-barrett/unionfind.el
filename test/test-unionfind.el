;;; test-unionfind.el --- Buttercup tests for unionfind -*- lexical-binding: t -*-

;;; Code:

(require 'unionfind)
(require 'buttercup)

(describe
 "Using different equality tests"
 (it
  "collapses `eq' strings"
  (expect
   (unionfind-count
    (let ((s "str")) (unionfind-of #'eq s s)))
   :to-equal 1))
 (it
  "separates non-`eq' strings"
  (expect
   (unionfind-count
    (unionfind-of #'eq (concat "str") (concat "str")))
   :to-equal 2))
 (it
  "collapses `equal' strings"
  (expect
   (unionfind-count
    (unionfind-of #'equal (concat "str") (concat "str")))
   :to-equal 1)))

(describe
 "unionfind-to-hash-set"
 (it
  "doesn't fail completely"
  (expect
   (hash-table-p
    (unionfind-to-hash-set
     (unionfind-of nil (concat "str") (concat "str"))))
   :to-equal t)))

;;; test-unionfind.el ends here

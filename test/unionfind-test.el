;;; unionfind-test.el --- ERT tests for unionfind -*- lexical-binding: t -*-

;;; Code:

(require 'unionfind)
(require 'propcheck)

(defmacro should (&rest forms)
  "Propcheck weirdly doesn't print out errors on its own; FORMS."
  `(propcheck-should
    (condition-case err
        (progn ,@forms)
      (t (progn (message "Error: %s" err)
                (error "Error!"))))))

(propcheck-deftest
 insert-one-to-count ()
 (should
  (equal
   1
   (unionfind-count
    (unionfind-of nil (propcheck-generate-integer "i"))))))

(propcheck-deftest
 insert-to-count ()
 (should
  (let* ((how-many (abs (propcheck-generate-integer "how-many" :min 0 :max 128)))
         (uf (let ((uf (unionfind-of)))
               (dotimes (i how-many)
                 (unionfind-insert
                  uf
                  (propcheck-generate-integer
                   (concat "i" (number-to-string i)))))
               uf)))
    (>= how-many (unionfind-count uf)))))

;; (propcheck-deftest
;;  insert-to-eq-string ()
;;  (should
;;   (let* ((how-many (abs (propcheck-generate-integer "how-many" :min 0 :max 16)))
;;          ;; TODO: If I remove strs, this test fails. Why?
;;          ;; TODO: Why does this pass if I replace `eq' with `equal'?
;;          (strs nil)
;;          (uf (let ((uf (unionfind-new #'eq)))
;;                (dotimes (s how-many)
;;                  (unionfind-insert
;;                   uf
;;                   (push
;;                    (let ((s
;;                           (propcheck-generate-string
;;                            (concat "s" (number-to-string s)))))
;;                      ;; Make collisions more likely:
;;                      (substring s 0 (min 1 (length s))))
;;                    strs)))
;;                uf)))
;;     (equal how-many (unionfind-count uf)))))

(propcheck-deftest
 insert-one-to-equiv-p ()
 (let ((i (propcheck-generate-integer "i")))
   (should
    (unionfind-equiv-p (unionfind-of nil i) i i))))

(propcheck-deftest
 insert-two-to-equiv-p ()
 (let ((i (propcheck-generate-integer "i"))
       (j (propcheck-generate-integer "j")))
   (should
    (equal
     (equal i j)
     (let ((uf (unionfind-of nil i j)))
       (unionfind-equiv-p uf i j))))))

(propcheck-deftest
 insert-two-merge-to-equiv-p ()
 (let ((i (propcheck-generate-integer "i"))
       (j (propcheck-generate-integer "j")))
   (should
     (let ((uf (unionfind-of nil i j)))
       (unionfind-merge uf i j)
       (unionfind-equiv-p uf i j)))))

(propcheck-deftest
 insert-to-hash-count ()
 (let ((i (propcheck-generate-integer "i")))
   (should
    (equal
     1
     (hash-table-count (unionfind-to-hash-set (unionfind-of nil i)))))))

;;; unionfind-test.el ends here

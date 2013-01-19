(cl:in-package :ai.lang.lisp.code.ext.tables)


;;;; Test Code
(eval-when (:compile-toplevel)
  (deftable sqr :type hash-table :size 15))


(defun test-it ()
  (every #'test-one '(hash-table alist prop vector prop-list)))


(defun test-one (type)
  (format t "~2&;; For ~a" type)
  (eval (copy-tree `(deftable sqr :type ,type :size 15)))
  (labels ((show-table (msg)
	     (format t "~&;; ~a:" msg)
	     (write *sqr-table* :array t)
	     (map-sqr #'print-kv))
	   (print-kv (k v) (format t "~&~a:~8T~a" k v))
	   (key (x) (if (eq type 'vector) x (mksymbol 'x x))))

    (setf *sqr-table* (new-sqr-table))
    (loop for i from 1 to 10
	  do (setf (get-sqr (key i)) (* i i))
	  do (assert (eql (get-sqr (key i)) (* i i))))
    (show-table "After 10 entries")
    (clear-sqr)
    (show-table "After clearing")
    (put-sqr (key 1) 'one)
    (put-sqr (key 2) 'two)
    (put-sqr (key 3) 'three)
    (rem-sqr (key 1))
    (show-table "After adding 3 and rem-ing 1")
    (let ((new-tab (new-sqr-table)))
      (put-sqr (key 10) 'ten new-tab)
      (put-sqr (key 11) 'eleven new-tab)
      (put-sqr (key 12) '??? new-tab)
      (rem-sqr (key 12) new-tab)
      (format t "~&;; New-tab should have 10 and 11:")
      (map-sqr #'print-kv new-tab)
      (show-table "Now back to the original table")))
  t)


;;; eof

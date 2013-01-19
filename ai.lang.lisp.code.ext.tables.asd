;;;; ai.lang.lisp.code.ext.tables.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)

(defsystem :ai.lang.lisp.code.ext.tables
  :serial t
  :depends-on (:fiveam)
  :components ((:file "package")
               (:file "ai.lang.lisp.code.ext.tables")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :ai.lang.lisp.code.ext.tables))))
  (load-system :ai.lang.lisp.code.ext.tables)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
        (funcall (_ :ai.lang.lisp.code.ext.tables :test-it)))
      (error "test-op failed") ))


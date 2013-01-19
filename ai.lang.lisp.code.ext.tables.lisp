;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User; -*-

#||                          DEFTABLE

===========================================================================
Copyright 1992 by Sun Microsystems Inc. (Sun)

Permission to use, copy, modify, and distribute this software and
its documentation for any purpose and without fee is hereby granted, 
provided that this copyright and permission notice appear in all
copies and supporting documentation.

Sun makes no representations about the suitability of this software
for any purpose.  It is provided "as is" without express or implied
warranty.  Sun disclaims all warranties, including all implied
warranties of merchantability and fitness. In no event shall Sun be
liable for any special, indirect or consequential damages or any
damages whatsoever resulting from loss of use, data or profits,
whether in an action of contract, negligence or other tortious
action, arising out of or in connection with the use or performance
of this software.
===========================================================================

The DEFTABLE macro provides a concise, lightweight notation for defining
tables: things you can put key/value pairs into, and retrieve the value
from the key.  For example, after specifying:

(deftable state
   :init '((AL Alabama) (AK Alaska) ...))

You can say (get-state 'AL) to get Alabama.  If the need ever arises, you 
can add to the table with (put-state 'PR 'Puerto-Rico), or equivalenty,
(setf (get-state 'PR) 'Puerto-Rico).  You can delete one or clear all
entries, and you can use (map-state #'(lambda (abbrev name) ...)) to
map a function over all entries.

Finally, you can keep track of multiple tables of the same type.  For example,
you could arrange to have a table for each country, so that:
(get-state 'AZ *USA*) ==> Arizona
(get-state 'AZ *CIS*) ==> Azerbaijan

When it comes time to performance-tune your program, you can implement
each table type as a hash table, property list, alist, or vector (if the keys
are dense small integers), or define your own implementation.

Note that if you evaluate two deftables with the same name, the table
created for the first one will not be replaced (because the table is
defined with defvar, not defparameter.  The default value and any initial
values will take effect the second (and subsequent) times, however.

See the article "DEFTABLE: A Macro for Implementing Tables"
by Peter Norvig, to appear in  Lisp Pointers, for more details.

;;;; Sample Expansion

(deftable state
   :init '((AL Alabama) (AK Alaska)))    ===>

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun new-state-table (&key (size 100) (test #'equal))
    "Create a new table."
    (make-hash-table :test test :size size))
  (defparameter *state-table-default* nil)
  (defvar *state-table* (new-state-table))
  (declaim (notinline new-state-table get-state put-state rem-state map-state))
  (defsetf get-state (key &optional (table '*state-table*)) (val)
    (list 'put-state key val table))
  (defun get-state (key &optional (table *state-table*))
    "Return 2 values: the val for this key and t, or the default and nil."
    (gethash key table *state-table-default*))
  (defun put-state (key val &optional (table *state-table*))
    "Store val as the value of key in the table."
    (setf (gethash key table) val))
  (defun rem-state (key &optional (table *state-table*))
    "Remove key and it's value from the table."
    (remhash key table))
  (defun clear-state (&optional (table *state-table*))
    "Remove all key/value pairs from the table."
    (clrhash table))
  (defun map-state (fn &optional (table *state-table*))
    "Apply fn to each key and value, in any order."
    (maphash fn table))
  (initialize-table *state-table* #'put-state '((al alabama) (ak alaska)))
  'state)

||#


(cl:in-package :ai.lang.lisp.code.ext.tables)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mksymbol (&rest args)
    (intern (format nil "~{~A~}" args))))


;;;; DEFTABLE Macro
 
(defmacro deftable (name &rest args &key (type 'hash-table) inline default init
			 (tab (mksymbol '* name '-table*))
			 (def (mksymbol '* name '-table-default*))
			 (new (mksymbol 'new- name '-table))
			 (put (mksymbol 'put- name))
			 (get (mksymbol 'get- name))
			 (clr (mksymbol 'clear- name))
			 (map (mksymbol 'map- name))
			 (rem (mksymbol 'rem- name)) &allow-other-keys)
  "Define a table.  You can specify the type, whether the functions produced
  should be compiled inline or not, and the default value for the table.
  You can also specify the name of each function, and of the table and
  default-value variables (although we recommend using the default values:
  new-x, put-x, get-x, clrar-x, map-x rem-x, *x-table* and *x-table-default*).
  You can also initialize the table with the :init keyword, which takes an
  alist of (key value) [NB: not (key . value}] pairs."
  (let ((code-list (apply (get-table-implementation type) name :def def args)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
      (defun ,new (&key ,@(make-deftable-arglist code-list args))
	"Create a new table."
	,(code-for :new code-list))
      (defparameter ,def ,default)
      (defvar ,tab (,new))
      (declaim (,(if inline 'inline 'notinline) ,new ,get ,put ,rem ,map))
      (defsetf ,get (key &optional (table ',tab)) (val)
	(list ',put key val table))
      (defun ,get (key &optional (table ,tab)) 
	"Return 2 values: the val for this key and t, or the default and nil."
	,(code-for :get code-list))
      (defun ,put (key val &optional (table ,tab))
	"Store val as the value of key in the table."
	,(code-for :put code-list))
      (defun ,rem (key &optional (table ,tab))
	"Remove key and it's value from the table."
	,(code-for :rem code-list))
      (defun ,clr (&optional (table ,tab))
	"Remove all key/value pairs from the table."
	,(code-for :clr code-list))
      (defun ,map (fn &optional (table ,tab)) 
	"Apply fn to each key and value, in any order."
	,(code-for :map code-list))
      ,(when init
	 `(initialize-table ,tab #',put ,init))
      ',name)))

(defvar *table-implementations* nil)

(defmacro define-table-implementation (type arglist &body body)
  "Define an implementation of tables, giving the bodies of the
  six primitive operations."
  `(setf (getf *table-implementations* ',type)
    #'(lambda (name &key def ,@arglist &allow-other-keys)
	(list* :args ',arglist (progn ,@body)))))

(defun initialize-table (table put-fn initial-value-alist)
  "Initialize table (using the specified put-fn) with the
  alist of (key val) entries.  Note this is NOT (key . val)."
  (dolist (pair initial-value-alist)
    (funcall put-fn (first pair) (second pair) table))
  table)

;;;; Auxiliary Functions

(defun code-for (keyword code-list)
  "Return the code associated with this keyword, or signal an error."
  (if (get-member keyword code-list)
      (getf code-list keyword)
      (error "No ~a keyword supplied." keyword)))

(defun get-table-implementation (type)
  "Return the table implementation function for this type, or signal an error."
  (or (getf *table-implementations* type)
      (error "The type ~a should be one of ~{~s~*~^, ~}."
	     type *table-implementations*)))

(defun make-deftable-arglist (code-list deftable-args)
  "Given (deftable table :type type :size (* 10 n) :default 0) 
  and (define-table-implementation type ((test #'eql) (size 100))), 
  return ((test #'eql) (size (* 10 n)))."
  (labels ((var (pair) (if (symbolp pair) pair (first pair)))
	   (val (pair) (if (symbolp pair) nil (second pair)))
	   (arg-and-initval (pair)
	     (let* ((deftable-arg
			(get-member (intern (string (var pair)) :keyword)
				    deftable-args))) 
	       (list (var pair) (val (or deftable-arg pair))))))
    (mapcar #'arg-and-initval (getf code-list :args))))

(defun get-member (key plist)
  "Like member, returns a sublist of plist starting with key.
  But like get or getf, will only look for key in every other position.
  Example: (get-member 'c '(a one b two c three)) => (c three),
  Example: (get-member 'two '(a one b two c three)) => nil."
  (cond ((null plist) nil)
	((eql key (first plist)) plist)
	(t (get-member key (nthcdr 2 plist)))))


(defun getf2 (plist indicator &optional (default nil))
  "Like getf, but returns 2 values like gethash."
  (let ((rest-plist (get-member indicator plist)))
    (if rest-plist
	(values (second rest-plist) t)
	(values default nil))))

;;;; Table Implementations

;;; In addition to defining application-specific tables, you may want
;;; to specify the way to implement each table.  Do that by giving the
;;; :type keyword to deftable, with one of the values listed below:
;;; hash-table, vector, prop-list, alist or prop.  Or make up your own
;;; implementation with define-table-implementation.

(define-table-implementation hash-table ((size 100) (test #'equal))
  "The simplest implementation: hash tables.  O(1) access and update."
  `(:new (make-hash-table :test test :size size)
    :get (gethash key table ,def)
    :put (setf (gethash key table) val)
    :rem (remhash key table)
    :clr (clrhash table)
    :map (maphash fn table)))

(define-table-implementation vector ((size 100))
  "Requires that all keys are non-negative integers less than SIZE.
  Does not handle removing properly: all entries are present, and
  removing one just restores the default value.  O(1) access and update."
  `(:new (make-array size :initial-element ,def)
    :get (values (svref table key) t)
    :put (setf (svref table key) val)
    :rem (setf (svref table key) ,def)
    :clr (fill table ,def)
    :map (dotimes (i (length table))
	   (funcall fn i (svref table i)))))

(define-table-implementation prop-list ()
  "Keeps the table as a headed list, using getf.  O(n) access and update.
  Keys are compared with EQ."
  `(:new (cons ',name nil)
    :get (getf2 (rest table) key ,def)
    :put (setf (getf (rest table) key) val)
    :rem (remf (rest table) key)
    :clr (setf (rest table) nil)
    :map (loop for (key val) on (rest table) by #'cddr
	   do (funcall fn key val))))

(define-table-implementation alist ((test #'eql))
  "Very similar to prop-list, but in association list format."
  `(:new (cons ',name nil)
    :get (let ((pair (assoc key (rest table) :test ,test)))
	   (if pair
	       (values (cdr pair) t)
	       (values ,def nil)))
    :put (let ((pair (assoc key (rest table) :test ,test)))
	   (if pair
	       (setf (cdr pair) val)
	       (push (cons key val) (rest table)))
	   val)
    :rem (setf (rest table)
	  (delete key (rest table) :test ,test :key #'car))
    :clr (setf (rest table) nil)
    :map (dolist (pair (rest table))
	   (funcall fn (car pair) (cdr pair)))))

(define-table-implementation prop ((package *package*))
  "Keys must be symbols; values are stored under the name property.
  O(1) access (if no other properties), but MAP is O(#symbols in package)."
  `(:new package
    :get (getf2 (symbol-plist key) ',name ,def)
    :put (setf (get key ',name) val)
    :rem (remprop key ',name)
    :clr (do-symbols (key table)
	   (remprop key ',name))
    :map (do-symbols (key table)
	   (multiple-value-bind (val found?)
	       (getf2 (symbol-plist key) ',name)
	     (when found?
	       (funcall fn key (get key ',name)))))))

;;; eof

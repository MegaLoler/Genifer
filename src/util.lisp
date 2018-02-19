(defpackage :genifer.util
  (:use :cl)
  (:export substitute-tree-if
	   quoted?
	   unquote))
(in-package :genifer.util)

(defun substitute-tree-if (tree predicate replace-func)
  "Search for items in a tree that meet predicate and replace them with the result of applying the old item to a replacement function."
  (if (funcall predicate tree)
      (funcall replace-func tree)
      (if (and tree (typep tree 'list))
	  (cons (substitute-tree-if (car tree) predicate replace-func)
		(substitute-tree-if (cdr tree) predicate replace-func))
	  tree)))

(defun quoted? (expression)
  "Is an expression quoted?"
  (and (typep expression 'list)
       (equal (car expression) 'quote)))

(defun unquote (expression)
  "Unquote a quoted expression."
  (if (quoted? expression)
      (cadr expression)
      (error "Expression is not quoted!")))

(defpackage :genifer.javascript
  (:use :cl :genifer)
  (:export generate-js))
(in-package :genifer.javascript)

;;; javascript
;; just a toy subset of javascript atm
;; gen type patterns are checked in reverse order
;; so the newest defined ones override previous ones

;; misc expression
(defgen (js stream) atom expression
  (format stream "~(~S~)" expression))

;; general function call
(defgen (js stream) (symbol . list) (name . args)
  (format stream "~(~A~)(~{~A~^, ~})" name (gen-exprs args)))

;; named function definition
(defgen (js stream) ('function symbol list . list) (_ name args . body)
  (declare (ignore _))
  (format stream "function ~(~A~)(~{~(~A~)~^, ~}) { ~A }"
	  name args (gen-expr `(block ,@body))))

;; anonymous function definition
(defgen (js stream) ('function list . list) (_ args . body)
  (declare (ignore _))
  (format stream "(~{~(~A~)~^, ~}) => { ~A }"
	  args (gen-expr `(block ,@body))))

;; code block
(defgen (js stream) ('block . list) (_ . expressions)
  (declare (ignore _))
  (format stream "~{~A;~^ ~}" (gen-exprs expressions)))

;; return statement
(defgen (js stream) ('return t) (_ value)
  (declare (ignore _))
  (format stream "return ~A" (gen-expr value)))

;; variable declaration and assignment
(defmacro defgen-js-var-specifier (keyword)
  "Define a keyword used for variable declaration."
  `(progn
     ;; declaration without assignment
     (defgen (js stream) (',keyword symbol) (var-specifier name)
       (format stream "~(~A ~A~)" var-specifier name))
     ;; declaration with assignment
     (defgen (js stream)
	 (',keyword symbol t) (var-specifier name expression)
       (format stream "~(~A ~A~) = ~A"
	       var-specifier name (gen-expr expression)))))
(defgen-js-var-specifier var)
(defgen-js-var-specifier const)
(defgen-js-var-specifier let)

;; infix operators
(defmacro defgen-js-operator (operator &optional (enclose t))
  "Define an infix operator in JS."
  (let ((open-paren (if enclose "(" ""))
	(close-paren (if enclose ")" "")))
  `(defgen (js stream) (',operator . list) (operator . args)
     (format stream
	     (concatenate 'string
			  ,open-paren
			  "~{~A~^ "
			  (format nil "~(~A~)" operator)
			  " ~}"
			  ,close-paren)
	     (gen-exprs args)))))
(defgen-js-operator +)
(defgen-js-operator -)
(defgen-js-operator *)
(defgen-js-operator /)
(defgen-js-operator = nil)
(defgen-js-operator ==)
(defgen-js-operator !=)
(defgen-js-operator >)
(defgen-js-operator >=)
(defgen-js-operator <)
(defgen-js-operator <=)
(defgen-js-operator and)
(defgen-js-operator or)

;; not
(defgen (js stream) ('not t) (_ expression)
  (declare (ignore _))
  (format stream "!~A" (gen-expr expression)))

;; object.child dot notation
(defgen (js stream) ('prop t . list) (_ initial . vars)
  (declare (ignore _))
  (format stream "~A~{~A~}" (gen-expr initial)
	  (mapcar (lambda (var)
		    (cond ((typep var 'symbol)
			   (format nil ".~(~A~)" var))
			  (t (format nil "[~S]" var))))
		  vars)))

;; js constants
(defmacro defgen-js-constant (constant string)
  `(defgen (js stream) ',constant _
     (declare (ignore _))
     (format stream ,string)))
(defgen-js-constant t "true")
(defgen-js-constant nil "null")
(defgen-js-constant true "true")
(defgen-js-constant false "false")

(defmacro generate-js (stream &body body)
  "Generate javascript given S-expressions."
  `(generate ,stream 'js ,@body))

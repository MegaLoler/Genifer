(defpackage :genifer
  (:use :cl :genifer.util)
  (:export generate
	   gen-expr
	   gen-exprs
	   defgen))
(in-package :genifer)

;; a function to generate code given a specific pattern of code
(defstruct (generator (:constructor make-generator
				    (arg-type-spec arg-name-spec function)))
  arg-type-spec
  arg-name-spec
  (function nil :type function))

;; a global hash table for storing genspecs
(defvar *gen-specs* (make-hash-table))

(defun get-gen-spec (name)
  "Make sure a gen-spec exists in the global hash table and return it."
  (if (not (gethash name *gen-specs*))
      (setf (gethash name *gen-specs*)
	    (make-array 0
			:element-type 'generator
			:fill-pointer 0
			:adjustable t))
      (gethash name *gen-specs*)))

(defun get-matching-generator (generator gen-spec)
  "Does a generator with a matching arg-type-spec exists in gen-spec?"
  (find-if (lambda (existing)
	     (equalp (generator-arg-type-spec generator)
		     (generator-arg-type-spec existing)))
	   gen-spec))

(defun generator-push (generator gen-spec-name)
  "Add a generator to a global gen-spec."
  (let* ((gen-spec (get-gen-spec gen-spec-name))
	 (existing (get-matching-generator generator gen-spec)))
    (if existing
	(warn (format nil
		      "redefining generator for type-spec ~A in gen-spec ~A"
		      (generator-arg-type-spec generator)
		      gen-spec-name)))
    (delete existing gen-spec)
    (vector-push-extend generator gen-spec)))

(defun make-bindings (name-spec values &optional bindings)
  "List of bindings of destructured name-spec names to corresponding values in values."
  (if name-spec
      (if (typep name-spec 'list)
	  (if (typep values 'list)
	      (make-bindings (cdr name-spec) (cdr values)
			     (make-bindings (car name-spec) (car values)
					    bindings))
	      (error "Attempt to destructure a non-list!"))
	  (cons (list name-spec `(quote ,values))
		bindings))
      (progn
	(if values
	    (warn "Too few names to bind to all given values."))
	bindings)))

(defmacro bind-arg-names (arg-name-spec expression &body body)
  "Destructure and bind names in arg-name-spec to the corresponding destructured values in expression."
  `(let ,(make-bindings arg-name-spec expression)
     ,@body))

;; that eval in here bothers me a lot????
(defmacro defgen (gen-spec-name-and-stream-sym arg-type-spec arg-name-spec
		  &body body)
  "Create a new generator and add it to a global gen-spec."
  (let* ((gen-spec-name (car gen-spec-name-and-stream-sym))
	 (stream-sym (cadr gen-spec-name-and-stream-sym))
	 (body (substitute-tree-if
		body
		(lambda (e)
		  (and (typep e 'list)
		       (or (equal (car e) 'gen-expr)
			   (equal (car e) 'gen-exprs))))
		(lambda (e)
		  `(,(car e)
		     (get-gen-spec ',gen-spec-name)
		     ,(cadr e)
		     ;; ,stream-sym
		     )))))	
    `(generator-push (make-generator ',arg-type-spec ',arg-name-spec
				     (lambda (expression ,stream-sym)
				       (let ((body ',body)
					     (stream ,stream-sym))
					 (eval
					  `(let ((stream ,stream))
					     (bind-arg-names
						 ,',arg-name-spec
						 ,expression
					       ,@body))))))
		     ',gen-spec-name)))

(defun type-spec-match-ls? (type-spec expression)
  "Does the list expression match the list type-spec?"
  (if (and type-spec expression)
      (and (type-spec-match? (car type-spec)
			     (car expression))
	   (type-spec-match? (cdr type-spec)
			     (cdr expression)))
      (equalp type-spec expression)))

(defun type-spec-match? (type-spec expression)
  "Does expression match the type-spec?"
  (cond ((quoted? type-spec)
	 (equalp expression (unquote type-spec)))
	((typep type-spec 'list)
	 (if (typep expression 'list)
	     (type-spec-match-ls? type-spec expression)))
	((typep type-spec 'symbol)
	 (typep expression type-spec))
	(t (equalp expression type-spec))))

(defun find-matching-generator (gen-spec expression)
  "Find the first generator in gen-spec whose arg-type-spec matches expression."
  (find-if (lambda (generator)
	     (type-spec-match? (generator-arg-type-spec generator)
			       expression))
	   gen-spec :from-end t))

(defun gen-expr (gen-spec expression &optional stream)
  "Generate code expressions given a code generation specification."
  (let ((generator (find-matching-generator gen-spec expression)))
    (if generator
	(funcall (generator-function generator) expression stream))))

(defun gen-exprs (gen-spec expressions &optional stream)
  "Generate a list of code expressions given a gen-spec."
  (mapcar (lambda (expression)
	    (gen-expr gen-spec expression stream))
	  expressions))

(defmacro generate (stream gen-spec-name &body expressions)
  "Wrap expressions in a block and auto get genspec by name symbol."
  `(gen-expr (get-gen-spec ,gen-spec-name)
	     '(block ,@expressions)
	     ,stream))

;;; javascript
;; gen type patterns are checked in reverse order
;; so the newest defined ones override previous ones

;; misc expression
(defgen (js stream) t expression
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
			  (format nil "~A" operator)
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
(defgen-js-operator >)
(defgen-js-operator <=)
(defgen-js-operator &&)
(defgen-js-operator ||)

;; object.child dot notation
(defgen (js stream) ('prop t . list) (_ initial . vars)
  (declare (ignore _))
  (format stream "~A~{~A~}" (gen-expr initial)
	  (mapcar (lambda (var)
		    (cond ((typep var 'symbol)
			   (format nil ".~(~A~)" var))
			  (t (format nil "[~S]" var))))
		  vars)))

;; js code generation examples
(generate t 'js
  (function first (func) (return (func)))
  (function second () (return "second"))
  (var result (first second))
  (= result (first (function () (return "third")))))
;; function first(func) { return func(); }; function second() { return "second"; }; var result = first(second); result = first(() => { return "third"; });

(generate t 'js
  (prop dog cat 1 2 "hello"))
;; dog.cat[1][2]["hello"];

(gen-expr (get-gen-spec 'js) '(- (- 2 4) 3 9 9 9))
(gen-expr (get-gen-spec 'js) 1)

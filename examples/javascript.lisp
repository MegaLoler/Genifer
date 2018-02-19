(defpackage :javascript-example
  (:use :cl :genifer.javascript))
(in-package :javascript-example)

;; js code generation examples
(generate-js t
  (function first (func) (return (func)))
  (function second () (return "second"))
  (var result (first second))
  (= result (first (function () (return "third")))))
;; function first(func) { return func(); }; function second() { return "second"; }; var result = first(second); result = first(() => { return "third"; });

(generate-js t
  (prop dog cat 1 2 "hello"))
;; dog.cat[1][2]["hello"];

(generate-js t
  (not (and (< 7 3) (< 1 2))))
;; !((7 < 3) and (1 < 2));

(generate-js t
  (console.log (+ "hello" " " "world")))
;; console.log(("hello" + " " + "world"));

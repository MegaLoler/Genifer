# Genifer

A Common Lisp framework for defining S-expression syntax for generating code in other languages.

## Example

Define a Javascript function call as an S-expression of the form `(function-name ... arguments)` for the language `js`. Generate the corresponding Javascript code as `function_name(arg, arg, ...)` generating the strings for the args first with `gen-exprs`.

```lisp
(defgen (js stream) (symbol . list) (name . args)
  (format stream "~(~A~)(~{~A~^, ~})" name (gen-exprs args)))
```

And then for the Javascript `+` operator with the S-expression form of `(+ arg arg ...)` and the generated Javascript code as `arg + arg + arg ...`.

```lisp
(defgen (js stream) ('+ . list) (_ . args)
  (declare (ignore _))
  (format stream "~{~A~^ + ~}" (gen-exprs args)))
```

And specify how to generate a sequence of expressions in Javascript. Sequences of expressions are automatically prepended with `block` like this: `(block expr expr expr ...)`

```lisp
(defgen (js stream) ('block . list) (_ . expressions)
  (declare (ignore _))
  (format stream "~{~A;~^ ~}" (gen-exprs expressions)))
```

Now generate and print some Javascript to standard output!

```lisp
(generate t 'js
  (console.log (+ "hello" " " "world")))
```

Output:

```javascript
console.log("hello" + " " + "world");
```

There's a (slightly) more developed example in `lang/javascript.lisp` and `examples/javascript.lisp`.

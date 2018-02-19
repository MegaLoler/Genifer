# Genifer

A Common Lisp framework for defining S-expression syntax for generating code in other languages.

## Example

The `defgen` macro defines a generation function for some S-expression syntax for some language:

```lisp
(defgen (lang-identifier output-stream) type-structure structured-bindings
  generation-code*)
```

The `type-structure` specifies a type template that, when matched against an S-expression, uses the following generation function to translate the S-expression into a string. The `structured-bindings` are corresponding variable names which are binded to the corresponding values in the S-expression.

Define a Javascript function call as an S-expression of the form `(function-name ... arguments)` for the language `js`. Generate the corresponding Javascript code as `function_name(arg, arg, ...)` generating the sub-expressions first with `gen-exprs`. The type structure `(symbol . list)` matches any list whose first element is a symbol. The structured bindings `(name . args)` binds `name` to the `car` of the S-expression and `args` to the `cdr` of the S-expression.

```lisp
(defgen (js stream) (symbol . list) (name . args)
  (format stream "~(~A~)(~{~A~^, ~})" name (gen-exprs args)))
```

And then for the Javascript `+` operator with the S-expression form of `(+ arg arg ...)` and the generated Javascript code as `arg + arg + arg ...`. The type structure with its quoted `'+` indicates a match against an list expression whose first element is the symbol `+`. We don't need a binding for the `+` so we can name it `_` and ignore it.

```lisp
(defgen (js stream) ('+ . list) (_ . args)
  (declare (ignore _))
  (format stream "~{~A~^ + ~}" (gen-exprs args)))
```

And specify how to generate a sequence of expressions in Javascript. Sequences of expressions are automatically prepended with `block` like this: `(block expr expr expr ...)`.

```lisp
(defgen (js stream) ('block . list) (_ . expressions)
  (declare (ignore _))
  (format stream "~{~A;~^~%~}" (gen-exprs expressions)))
```

We need to specify the syntax for for atomic values in S-expressions which'll represent variable names and literals:

```lisp
(defgen (js stream) atom value
  (format stream "~(~S~)" value))
```

And lastly let's define simple variable declaration and assignment syntax:

```lisp
(defgen (js stream) ('var symbol t) (_ name expr)
  (declare (ignore _))
  (format stream "var ~(~A~) = ~A" name (gen-expr expr)))
```

Now generate and print some Javascript to standard output!

```lisp
(generate t 'js
  (var last_word "world")
  (console.log (+ "hello" " " last_word)))
```

Output:

```javascript
var last_word = "world";
console.log("hello" + " " + last_word);
```

There's a (slightly) more developed example in `lang/javascript.lisp` and `examples/javascript.lisp`.

Paul's Parser for Tom's Own Minimal Language
====

Common Lisp [TOML](https://github.com/mojombo/toml) parser.

Compliance notes
---

* Supports the TOML v0.1.0 with the following exceptions:

* Complications arising from escaping quote & comment syntax are known
and deferred until someone gets bit *and* is angry enough to send a
pull request fixing them.

* Not enforcing array homogeneity. Partly this is because of Postel's
dictum, but partly because it might be more convenient to have
heterogenous lists. Future work - might add a *strict* mode which
throws errors here.

* Unicode support may have some issues. It *should* work, but Unicode
on Common Lisp is sometimes dependant on your system's
configuration. Testing should be done before rolling your work to
production.

* Common Lisp is pretty good with numbers. Therefore, instead of
manually differentiating between floats and integers, pp-toml calls
them NUMBERS and lets your Lisp take care of the rest. Again, test
before rolling to production.

Testing
---

To run the tests, do this:

```
* (ql:quickload :pp-toml-tests)
To load "pp-toml-tests":
  Load 1 ASDF system:
    pp-toml-tests
; Loading "pp-toml-tests"
; ... SNIP ...
* (pp-toml-tests:run-tests)

Running test suite PP-TOML-SUITE
 Running test KEYGROUP-TESTS ..
 Running test KEYVALUE-TESTS ..
 Running test PREAMBLE-TESTS .
 Running test COMMENT-TESTS ......
 Running test MULTI-LINE-TESTS ...
 Running test DATETIME-TESTS ...
 Running test VALUE-TESTS .................f.#(4); #(8)
.
 Running test PARSE-TESTS .
 Did 38 checks.
    Pass: 37 (97%)
    Skip: 0 ( 0%)
    Fail: 1 ( 2%)

 Failure Details:
 --------------------------------
 VALUE-TESTS []:

(ESRAP:PARSE 'VALUE "\"XX\\/YY\"")

 evaluated to

(:STRING "XX\\/YY")

 which is not

EQUALP

 to

(:STRING "XX/YY")

..
 --------------------------------
```

Remarks
---

Since Tom thought it'd be a good idea to put his name on the language,
I'm putting my name on this parser.

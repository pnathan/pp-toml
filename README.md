Paul's Parser for Tom's Own Minimal Language (0.1.0)
====

Common Lisp [TOML](https://github.com/mojombo/toml) parser.

[![Build Status](https://travis-ci.org/pnathan/pp-toml.svg?branch=master)](https://travis-ci.org/pnathan/pp-toml)

Needs
---

The library needs to be brought up to 1.0.0 spec.

Compliance notes
---

* Supports the TOML v0.1.0 spec with the following exceptions:

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
 sbcl --script run-sbcl-tests.lisp
```

Which will produce a test report.

Contributions
---

Gratefully accepted. Please add test cases for bug reports you fix or
features you add.

License is LLGPL.

Remarks
---

Since Tom thought it'd be a good idea to put his name on the language,
I'm putting my name on this parser.

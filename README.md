Paul's Parser for Tom's Own Minimal Language
====

TOML is a language that needs to exist.

(https://github.com/mojombo/toml)[https://github.com/mojombo/toml]

This is a Common Lisp parser for TOML v0.1.0. Since Tom thought it'd
be a good idea to put his name on the language, I'm putting my name on
this parser. So there.

A few notes.

* I've decided to not focus on full compliance for now. In particular,
bizzare complications arising from escaping quote & comment syntax are
deferred until someone gets bit *and* is angry enough to send a pull
request fixing them.

* Also of note is that at present I am not enforcing array
homogenaity. Partly this is because of Postel's dictum, but partly
because it might be more convient to have heterogenous lists. I may
add a *strict* mode which throws errors here.

* Unicode support may have some issues. It *should* work, but Unicode
on Common Lisp is sometimes dependant on your system's
configuration. Testing should be done.

* Common Lisp is pretty good with numbers. Therefore, instead of
manually differentiating between floats and integers, pp-toml calls
them NUMBERS and lets Lisp take care of the rest.


Further comments.

In terms of testing, the test suite is somewhat rotted and needs
work. I plan to make another pass on it and get it ready for Quicklisp
& cl-test-grid.

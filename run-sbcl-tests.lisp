#!/usr/local/bin/sbcl

(require "sb-posix")

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defparameter *pwd* (concatenate 'string (sb-posix:getcwd) "/"))

(push *pwd* asdf:*central-registry*)

(ql:quickload :pp-toml-tests)
(let ((result-status (pp-toml-tests:run-tests)))
  (sb-posix:exit (if result-status 0 1) ))

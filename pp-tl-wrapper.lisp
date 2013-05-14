;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrapper for BurntSushi's TOML tester.
(load "pp-tl.lisp")


(defun read-standard-in ()
  (concatenate
   'string
   (loop
      for i = (read-char *standard-input* nil :eof)
      until (eq i :eof)
      collect i)))

(ql:quickload :yason)
(defun run-external-tests ()

  (let ((parsed-result
         (handler-case
             (pp-toml:parse-string (read-standard-in))
           (esrap:esrap-error () :parse-error))))
    ;; bail out crying
    (when (eq parsed-result
           :parse-error)
      (exit :code 1))

    (format t "~a"
            (yason:encode parsed-result))

    ))

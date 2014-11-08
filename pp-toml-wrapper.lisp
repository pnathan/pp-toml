;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrapper for BurntSushi's TOML tester.
(ql:quickload :pp-toml)
(ql:quickload :yason)


(defun read-standard-in ()
  (concatenate
   'string
   (loop
      for i = (read-char *standard-input* nil :eof)
      until (eq i :eof)
      collect i)))

(defun burnt-sushi-ize (table)
  "Fiddle the hash table to the spec found in
https://github.com/BurntSushi/toml-test" table )

(defun run-external-tests ()

  (let ((parsed-result
         (handler-case
             (pp-toml:parse-toml (read-standard-in))
           (esrap:esrap-error () :parse-error))))
    ;; bail out crying
    (when (eq parsed-result
           :parse-error)
      (exit :code 1))
            (yason:encode (burnt-sushi-ize parsed-result))))

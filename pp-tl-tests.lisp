(ql:quickload :fiveam)
(use-package :fiveam)
(ql:quickload :esrap)
(use-package :pp-toml)
(defpackage :pp-toml-tests
  (:use :common-lisp
        :pp-toml
        :fiveam)
  (:export :run-tests))

(in-package :pp-toml-tests)


(def-suite pp-toml-suite
  :description "The tests.")

(in-suite pp-toml-suite)

(defun run-tests ()
  (run! 'pp-toml-suite))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test keygroup-tests

  (is (esrap:parse 'keygroup "[foo.bar]
"))

  (is (esrap:parse 'keygroup "[foo]
")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test keyvalue-tests


  (is (esrap:parse 'keyvalue "title = \"TOML Example\"
" ))
  (is (esrap:parse 'keyvalue "
title = \"TOML Example\"
" )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test preamble-tests
  (is (esrap:parse 'preamble
                "title = \"TOML Example\"
bunco = false
billy = 1
")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test comment-tests
  (let ((basic "Foooo"))
    (is (string= basic
                 (strip-comments basic))))
  (let ((input "Hi #comment")
        (expected "Hi "))
    (is (string= expected
                 (strip-comments input))))
  (let ((input "\"With #comment\"")
        (expected "\"With #comment\"" ))
    (is (string= expected
                 (strip-comments input))))
  (let ((input "\"With #comment\" #trailing")
        (expected "\"With #comment\" " ))
    (is (string= expected
                 (strip-comments input))))
  ;; Let's go hardmode
  (let ((input "\"With #comment\" #trailing #comment")
        (expected "\"With #comment\" " ))
    (is (string= expected
                 (strip-comments input))))

  ;; Challenge mode accepted
  (let ((input "\"With #comment\" #A trailing \"str # ing\" here")
        (expected "\"With #comment\" " ))
    (is (string= expected
                 (strip-comments input))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test parse-tests

  (is
   (parse-file "title = \"TOML Example\"
[foo]
baffle = 1
binky=true
blaq=\"beautiful\"
"))



(parse-file
"
title = \"TOML Example\"

[owner]
name = \"Tom Preston-Werner\"
organization = \"GitHub\"
bio = \"GitHub Cofounder & CEO\nLikes tater tots and beer.\"
dob = 1979-05-27T07:32:00Z # First class dates? Why not?

[database]
server = \"192.168.1.1\"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true

[servers]


  [servers.alpha]
  ip = \"10.0.0.1\"
  dc = \"eqdc10\"

  [servers.beta]
  ip = \"10.0.0.2\"
  dc = \"eqdc10\"
"))

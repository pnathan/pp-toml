(defpackage :pp-toml-tests
  (:use :common-lisp
        :pp-toml
        :fiveam
        :generic-comparability)
  (:export :run-tests))

(in-package :pp-toml-tests)
(use-package :pp-toml)

(def-suite pp-toml-suite
  :description "The tests.")

(in-suite pp-toml-suite)

(defun run-tests ()
  (let ((results (run 'pp-toml-suite)))
    (explain! results)
    (results-status results)))

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
(test multi-line-tests
  (is (esrap:parse 'preamble "thing2 = \"other\nthing\"
"))

  (is (pp-toml:parse-string "thing2 = \"other\nthing\"
"))
  (is (pp-toml:parse-string "bio = \"GitHub Cofounder & CEO\nLikes tater tots and beer.\"
dob=1979-05-27T07:32:00Z
dob2 = 2013-10-22T07:32:00Z
"
))
)

(test datetime-tests
  (is
   (esrap:parse 'datetime "1979-05-27T07:32:00Z"))
  (is
   (esrap:parse 'value "1980-05-27T07:32:00Z"))

  (is
   (esrap:parse 'keyvalue "dt = 1981-05-27T07:32:00Z
")))
(defun collect (&rest chars)
  (format nil "~{~c~}" chars))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test value-tests
  (is (esrap:parse 'value "1983-05-27T07:32:00Z"))
  (is (esrap:parse 'value "1"))
  (is (esrap:parse 'value "10.1"))
  (is (esrap:parse 'value "true"))
  (is (esrap:parse 'value "false"))
  (is (esrap:parse 'value "[1]"))
  (is (esrap:parse 'value "[1,1]"))
  (is (esrap:parse 'value "[1,2,]"))
  (is (esrap:parse 'value "[[3],2,]"))
  (is (esrap:parse 'value "[1,[4],[5,5,],1]"))
  ;; string equality checks
  (is (equalp '(:string "aabb")
              (esrap:parse 'value "\"aabb\"")))

  (is (equalp '(:string "anoto")
            (esrap:parse 'value "\"anoto\"")))

  (is (equalp (list
               :string
               (collect #\X #\X #\Backspace #\Y #\Y))
              (esrap:parse 'value "\"XX\\bYY\"")))

  (is (equalp (list
               :string
               (collect #\X #\X #\Tab #\Y #\Y))
              (esrap:parse 'value "\"XX\\tYY\"")))

  (is (equalp (list
               :string
               (collect #\X #\X #\Newline #\Y #\Y))
              (esrap:parse 'value "\"XX\\nYY\"")))

  (is (equalp (list
               :string
               (collect #\X #\X #\Return #\Y #\Y))
       (esrap:parse 'value "\"XX\\rYY\"")))
  ;; \"
  (is (equalp (list
               :string
               (collect #\X #\X #\" #\Y #\Y))
       (esrap:parse 'value "\"XX\\\"YY\"")))

  ;; Currently failing - the \/ isn't getting translated into /

  ;; (is (equalp (list
  ;;              :string
  ;;              (collect #\X #\X #\/ #\Y #\Y))
  ;;             (esrap:parse 'value "\"XX\\/YY\"")))

  (is (equalp (list
               :string
               (collect #\X #\X #\\ #\Y #\Y))
       (esrap:parse 'value "\"XX\\\YY\"")))

  (is (equalp (list
               :string
               (collect #\X #\X (code-char 3456) #\Y #\Y))
       (esrap:parse 'value "\"XX\\u3456YY\""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test parse-tests
  (is
   (pp-toml:parse-string "title = \"TOML Example\"
[foo]
baffle = 1
binky=true
blaq=\"beautiful\"
"))



(pp-toml:parse-string
"
title = \"TOML Example\"

[owner]
name = \"Tom Preston-Werner\"
organization = \"GitHub\"
bio = \"GitHub Cofounder & CEO\nLikes tater tots and beer.\"
dob = 1979-05-27T07:32:00Z

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test top-level-tests
      (is (equals (pp-toml:parse-toml " ")
                  (make-hash-table :test #'equal)))
      (is (equals (pp-toml:parse-toml "# fooo ")
                  (make-hash-table :test #'equal)))
      (is (equals (pp-toml:parse-toml "# fooo

# bar ")
                  (make-hash-table :test #'equal))))

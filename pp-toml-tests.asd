(asdf:defsystem #:pp-toml-tests
  :depends-on (
               #:alexandria
                #:cl-ppcre
                #:esrap
                #:fiveam
                #:generic-comparability
                #:local-time
                #:parse-number
                #:split-sequence

                #:pp-toml
                )
  :components ((:file "pp-toml-tests"))
  :name "pp-toml-tests"
  :version "1.0"
  :maintainer "Paul Nathan"
  :author "Paul Nathan"
  :licence "LLGPL"
  :description "TOML parser"
  :long-description "Implements a TOML parser. PARSE-TOML is the
  top-level function")

(asdf:defsystem #:pp-tl
  :depends-on ( #:esrap
                #:parse-number
                #:alexandria #:split-sequence #:cl-ppcre
                #:local-time)
  :components ((:file "pp-tl"))
  :name "pp-tl"
  :version "1.0"
  :maintainer "Paul Nathan"
  :author "Paul Nathan"
  :licence "LLGPL"
  :description "TOML parser"
  :long-description "Implements a TOML parser. PARSE-TOML is the
  top-level function")

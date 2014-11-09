(asdf:defsystem #:pp-toml
  :depends-on ( #:esrap
                #:parse-number
                #:alexandria #:split-sequence #:cl-ppcre
                #:local-time)
  :components ((:file "pp-toml"))
  :name "pp-toml"
  :version "1.0"
  :maintainer "Paul Nathan <pnathan@alumni.uidaho.edu>"
  :author "Paul Nathan <pnathan@alumni.uidaho.edu>"
  :licence "LLGPL"
  :description "TOML parser"
  :long-description "Implements a TOML parser. PARSE-TOML is the
  top-level function")

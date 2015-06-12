(asdf:defsystem #:pp-toml
  :depends-on (#:alexandria
               #:cl-ppcre
               #:generic-comparability
               #:local-time
               #:parse-number
               #:split-sequence
               #:esrap)
  :components ((:file "pp-toml"))
  :name "pp-toml"
  :version "1.0.1"
  :maintainer "Paul Nathan <pnathan@alumni.uidaho.edu>"
  :author "Paul Nathan <pnathan@alumni.uidaho.edu>"
  :licence "LLGPL"
  :description "TOML parser"
  :long-description "Implements a TOML parser. PARSE-TOML is the
  top-level function")

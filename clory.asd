;;;; clory.asd

(asdf:defsystem #:clory
  :description "Common Lisp Ory API wrapper."
  :author "K1D77A"
  :license  "MIT"
  :homepage "https://github.com/K1D77A/clory"
  :version "0.0.1"
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "clory")))

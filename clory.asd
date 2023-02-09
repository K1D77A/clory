;;;; clory.asd

(asdf:defsystem #:clory
  :description "Common Lisp Ory API wrapper."
  :author "K1D77A"
  :license  "MIT"
  :homepage "https://github.com/K1D77A/clory"
  :version "0.0.1"
  :depends-on (#:dexador
               #:hu.dwim.defclass-star
               #:closer-mop
               #:shasht
               #:str)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "helpers")
               (:file "conditions")
               (:file "mop")
               (:file "protocol")
               (:file "response")
               (:file "clory")))


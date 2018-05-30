;;;; lbol-macros.asd

(asdf:defsystem #:lbol-macros
  :description "Describe lbol-macros here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (:alexandria :named-readtables)
  :serial t
  :components ((:file "package")
               (:file "lbol-macros")))

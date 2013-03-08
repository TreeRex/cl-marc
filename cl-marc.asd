;;;; cl-marc.asd

(asdf:defsystem #:cl-marc
  :serial t
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
               (:file "cl-marc")))


;;;; package.lisp

(defpackage #:cl-marc
  (:use #:cl)
  (:export #:process-marc-file
           #:read-all-records-in-marc-file
           #:get-leader
           #:extract-raw-field
           #:get-language
           #:get-field
           #:get-unicode-field))


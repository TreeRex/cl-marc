;;;; -*- mode: Lisp; coding: utf-8; -*-

;;;; Copyright 2013 EBSCO Information Services
;;;;
;;;; Licensed under the Apache License, Version 2.0 (the "License");
;;;; you may not use this file except in compliance with the License.
;;;; You may obtain a copy of the License at
;;;;
;;;;   http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing, software
;;;; distributed under the License is distributed on an "AS IS" BASIS,
;;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;; See the License for the specific language governing permissions and
;;;; limitations under the License.

(asdf:defsystem #:cl-marc
  :serial t
  :description "Utilities for processing Z39.2 / MARC format files"
  :author "Tom Emerson <temerson@ebsco.com>"
  :license "Apache 2.0"
  :encoding :utf-8
  ;; thank ${DEITY} for EdiWare.
  :depends-on (#:cl-ppcre #:cl-ppcre-unicode #:cl-unicode #:cl-interpol)
  :components ((:file "package")
               (:file "utils")
               (:file "charset-decoder")
               (:file "marc-8-tables")
               (:file "marc-8-decoder")
               (:file "utf-8-decoder")
               (:file "cl-marc")))


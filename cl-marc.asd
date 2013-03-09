;;;; -*- mode: Lisp; coding: utf-8; -*-

;;;; Copyright 2010-2013 EBSCO Publishing
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
  :author "Tom Emerson <temerson@ebscohost.com>"
  :license "Apache 2.0"
  :encoding :utf-8
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
               (:file "cl-marc")))


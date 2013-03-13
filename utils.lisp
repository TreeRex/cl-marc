;;;; utils.lisp  -*- mode: lisp; coding: utf-8; folded-file:t -*-
;;;;
;;;; Miscellaneous utilities used by cl-marc
;;;; Author: temerson (Tom Emerson)
;;;; Date: 2013-03-09

;;;; Copyright 2013 EBSCO Publishing
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

(in-package #:cl-marc)

(defun string->byte-array (s)
  "Converts a string consisting of 8-bit bytes into a byte array"
  (map '(simple-array (unsigned-byte 8) (*))
       #'(lambda (x) (char-code x)) s))

(defun utf8->string (s)
  "Convert an array of UTF-8 bytes into a string"
  #+:openmcl
  (ccl::decode-string-from-octets s :external-format :UTF-8)
  #-:openmcl
  (error "Need to write a UTF-8 conversion function"))

(defun whitespace-p (c)
  (or (char= c #\space)
      (char= c #\return)
      (char= c #\newline)
      (char= c #\tab)))

(defmacro make-keyword (s)
  `(intern ,s "KEYWORD"))


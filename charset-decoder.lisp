;;;; charset-decoder.lisp  -*- mode: lisp; coding: utf-8; folded-file:t -*-
;;;;
;;;; Abstract character set decoder
;;;; Author: temerson (Tom Emerson)
;;;; Date: 2013-03-09

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

(in-package #:cl-marc)

;; FIXME: does this actually need to exist? Are there any slots that wil be
;; common across decoders?

(defclass charset-decoder ()
  ()
  (:documentation "Common base class for all charset decoders."))

(defgeneric transcode-string (decoder byte-sequence)
  (:documentation "Transcodes BYTE-SEQUENCE into a Unicode string using DECODER.")
  ;; identity-decoder: just convert the BYTE-SEQUENCE back to a string
  (:method ((decoder charset-decoder) byte-sequence)
    (coerce (loop for c across byte-sequence collecting (code-char c)) 'string)))



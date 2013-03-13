;;;; utf-8-decoder.lisp  -*- mode: lisp; coding: utf-8; folded-file:t -*-
;;;;
;;;; UTF-8 Decoder
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

(defclass utf-8-decoder (charset-decoder)
  ())

(defmethod transcode-string ((decoder utf-8-decoder) s)
  (declare (ignore decoder))
  #+:openmcl
  (ccl::decode-string-from-octets s :external-format :UTF-8)
  #-:openmcl
  (error "UTF-8 conversion TBD"))

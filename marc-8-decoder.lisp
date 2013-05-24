;;;; marc-8-decoder.lisp  -*- mode: lisp; coding: utf-8; folded-file:t -*-
;;;;
;;;; MARC-8 to Unicode Transcoder
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

;;;; http://www.loc.gov/marc/specifications/speccharmarc8.html
;;;; http://www.loc.gov/marc/specifications/codetables.xml

(in-package #:cl-marc)

; c0 and c1 are not affected by different graphic set designations
; combining character(s) appear *before* their base character
; Alternate character set designation:
;   Technique 1: (switches into G0 space, locked)
;    ESC g - greek
;    ESC b - subscript
;    ESC p - superscript
;    ESC s - ascii
;
;   Technique 2: (ISO-2022, n character sequence ESC I+ F)
;    ESC ( F - G0 single byte
;    ESC , F - G0 single byte
;    ESC $ F - G0 multi-byte
;    ESC $ , F - G0 multi-byte
;
;    ESC ) F - G1 single byte
;    ESC - F - G1 single byte
;    ESC $ ) F - G1 multi-byte
;    ESC $ - F - G1 multi-byte
;
;    F determines the mapping table to use: can be looked up in a hash or assoclist.
;    Need to determine the number of bytes to read per character depending on the table
;    in use.
;
;    All codes are locking.
;
; Special handling: ordering of combining forms
; Revised September 2004 to change the mapping from MARC-8 to Unicode for the Ligature (M+EB and M+EC) from U+FE20 and U+FE21 to U+0361.

;Revised September 2004 to change the mapping from MARC-8 to Unicode for the Double Tilde (M+FA and M+FB) from U+FE22 and U+FE23 to U+0360.


; need to keep track of the current G0 and G1 table: which is really a stack since
; they can shift in and out. For simple case need to define G0 - ASCII and G1 ANSEL mappings.

(defclass marc-8-decoder (charset-decoder)
  ((c0
    :initform '()
    :accessor c0)
   (g0
    :initform +ascii-mappings+
    :accessor g0)
   (c1
    :initform '()
    :accessor c1)
   (g1
    :initform +ansel-mappings+
    :accessor g1)
   (multibyte-p
    :initform nil
    :accessor multibyte-p
    :documentation "t if the current encoding is multibyte")))

;; (defmethod initialize-instance :after ((decoder marc-8-decoder) &key)
;;   (push g0-ascii-mappings (g0 decoder))
;;   (push g1-ansel-mappings (g1 decoder)))

(defmethod g0-mapping ((decoder marc-8-decoder) val)
  (aref (g0 decoder) (- val #x20)))

(defmethod g1-mapping ((decoder marc-8-decoder) val)
  (aref (g1 decoder) (- val #xa0)))

(defun g0-p (b)
  (<= #x20 b #x7f))

(defun g1-p (b)
  (<= #xa0 b #xff))

;; combining characters /precede/ the base character.
;; Unicode uses the opposite order
(defmethod transcode-string ((decoder marc-8-decoder) s)
  (with-output-to-string (out)
    (let ((slength (length s))
          (i 0)
          byte)
      (tagbody
       map
         (when (= i slength)
           (go done))
         (setq byte (aref s i))
         (cond ((g0-p byte) (write-char (g0-mapping decoder byte)))
               ((g1-p byte) (write-char (g1-mapping decoder byte)))
               (t (error "Unsupported value")))
         (incf i)
         (go map)
       done
         ))))

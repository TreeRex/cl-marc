;;;; marc-8-tables.lisp  -*- mode: lisp; coding: utf-8; folded-file:t -*-
;;;;
;;;; MARC-8 Mapping Tables
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

;;;; http://www.loc.gov/marc/specifications/codetables.xml

;; lookup based on a value in the range 0x20 - 0x7E, so the index into each
;; table is (- c 0x20).

;; Need to know if a particular c is combining or not. Use a bit vector.
;; lookup will again be based on (- c 0x20)

;; when we get a combining character, we start storing in an accumulator
;; when we encounter a base character we need to know whether or not we
;; should stop accumulating or continue. Once we've stopped accumulating,
;; we need to convert the accumulated sequence. This will be similar for
;; EACC, where we want to decode sequences of three octets.



(in-package #:cl-marc)

(defconstant +ascii-mappings+
  #(#\U+0020 #\U+0021 #\U+0022 #\U+0023 #\U+0024 #\U+0025 #\U+0026 #\U+0027
    #\U+0028 #\U+0029 #\U+002A #\U+002B #\U+002C #\U+002D #\U+002E #\U+002F
    #\U+0030 #\U+0031 #\U+0032 #\U+0033 #\U+0034 #\U+0035 #\U+0036 #\U+0037
    #\U+0038 #\U+0039 #\U+003A #\U+003B #\U+003C #\U+003D #\U+003E #\U+003F
    #\U+0040 #\U+0041 #\U+0042 #\U+0043 #\U+0044 #\U+0045 #\U+0046 #\U+0047
    #\U+0048 #\U+0049 #\U+004A #\U+004B #\U+004C #\U+004D #\U+004E #\U+004F
    #\U+0050 #\U+0051 #\U+0052 #\U+0053 #\U+0054 #\U+0055 #\U+0056 #\U+0057
    #\U+0058 #\U+0059 #\U+005A #\U+005B #\U+005C #\U+005D #\U+005E #\U+005F
    #\U+0060 #\U+0061 #\U+0062 #\U+0063 #\U+0064 #\U+0065 #\U+0066 #\U+0067
    #\U+0068 #\U+0069 #\U+006A #\U+006B #\U+006C #\U+006D #\U+006E #\U+006F
    #\U+0070 #\U+0071 #\U+0072 #\U+0073 #\U+0074 #\U+0075 #\U+0076 #\U+0077
    #\U+0078 #\U+0079 #\U+007A #\U+007B #\U+007C #\U+007D #\U+007E))

(defvar +ansel-mappings+
  #(#\U+FFFD #\U+0141 #\U+00D8 #\U+0110 #\U+00DE #\U+00C6 #\U+0152 #\U+02B9
    #\U+00B7 #\U+266D #\U+00AE #\U+00B1 #\U+01A0 #\U+01AF #\U+02BC #\U+FFFD
    #\U+02BB #\U+0142 #\U+00F8 #\U+0111 #\U+00FE #\U+00E6 #\U+0153 #\U+02BA
    #\U+0131 #\U+00A3 #\U+00F0 #\U+FFFD #\U+01A1 #\U+01B0 #\U+FFFD #\U+FFFD
    #\U+00B0 #\U+2113 #\U+2117 #\U+00A9 #\U+266F #\U+00BF #\U+00A1 #\U+00DF
    #\U+20AC #\U+FFFD #\U+FFFD #\U+FFFD #\U+FFFD #\U+FFFD #\U+FFFD #\U+FFFD
    #\U+FFFD #\U+FFFD #\U+FFFD #\U+FFFD #\U+FFFD #\U+FFFD #\U+FFFD #\U+FFFD
    #\U+FFFD #\U+FFFD #\U+FFFD #\U+FFFD #\U+FFFD #\U+FFFD #\U+FFFD #\U+FFFD
    #\U+0309 #\U+0300 #\U+0301 #\U+0302 #\U+0303 #\U+0304 #\U+0306 #\U+0307
    #\U+0308 #\U+030C #\U+030A #\U+FE20 #\U+FE21 #\U+0315 #\U+030B #\U+0310
    #\U+0327 #\U+0328 #\U+0323 #\U+0324 #\U+0325 #\U+0333 #\U+0332 #\U+0326
    #\U+031C #\U+032E #\U+FE22 #\U+FE23 #\U+FFFD #\U+FFFD #\U+0313))


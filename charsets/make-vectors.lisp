;;;; -*- mode: Lisp; coding: utf-8; -*-

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

;;; this uses treerex-utils

(defun create-gn-vector (filename base)
  (flet ((from-hex (s) (parse-integer s :radix 16)))
    (let ((previous (1- base)))
      (tru:with-fields-in-file ((gn 0 #'from-hex)
                                (ucs 1 #'from-hex))
        (filename)
        (when (>= gn base)
          (when (/= gn (1+ previous))
            (dotimes (i (- gn previous 1))
              (format t "#\\U+FFFD ")))
          (format t "#\\U+~4,'0X " ucs)
          (setq previous gn))))))

;;;; cl-marc.lisp  -*- mode: lisp; coding: utf-8; folded-file:t -*-
;;;;
;;;; Support for reading Z39.2 records
;;;; Author: temerson (Tom Emerson)
;;;; Date: 2013-03-08

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

;;;; NISO Z39.2 Specification: http://bit.ly/ePLWK6
;;;; LoC MARC: http://www.loc.gov/marc/marcdocz.html

(in-package #:cl-marc)

(defconstant +record-terminator+ #\Gs)
(defconstant +field-terminator+ #\Rs)

(defclass generic-record ()
  ((raw-record
    :initarg :raw-record
    :initform (error "raw-record must be supplied"))
   base-address
   ;; a list: (tag len start)
   directory
   (charset-decoder :accessor charset-decoder)))

(defclass marc21-record (generic-record)
  ())

(defclass unimarc-record (generic-record)
  ())

(defclass cmarc-record (unimarc-record)
  ()
  (:documentation "A record using CMARC conventions."))

(defclass cnmarc-record (unimarc-record)
  ()
  (:documentation "A record using CNMARC conventions."))

(defun decode-directory-entry (entry)
  "Decode the 12 bytes in a directory entry"
  (list (make-keyword (subseq entry 0 3))
        (parse-integer entry :start 3 :end 7)
        (parse-integer entry :start 7)))

(defun extract-directory (directory directory-length)
  "Extracts the directory entries into an alist"
  (loop for i upto (1- directory-length)
       collect (decode-directory-entry
                (subseq directory (* i 12) (+ (* i 12) 12)))))

(defmethod initialize-instance :after ((record generic-record) &key)
  (let* ((base-address (parse-integer (slot-value record 'raw-record)
                                      :start 12 :end 17))
         (directory-length (/ (- base-address 25) 12)))
    (setf (slot-value record 'base-address) base-address)
    (setf (slot-value record 'directory)
          (extract-directory (subseq (slot-value record 'raw-record) 24) directory-length))))

(defmethod extract-raw-fields ((record generic-record) field-tag)
  "Extracts the raw contents of the field(s) specified by field-tag."
  (with-slots (raw-record base-address directory) record
    (flet ((get-field (length start)
             (let ((base (+ base-address start)))
               (subseq raw-record base (+ base length -1)))))
      (loop for (tag length start) in directory
           if (eq field-tag tag) collect (get-field length start)))))

(defmethod get-leader ((record generic-record))
  (subseq (slot-value record 'raw-record) 0 24))

(defmethod dump-directory ((record generic-record))
  (format t "~a:~%" (get-leader record))
  (dolist (entry (slot-value record 'directory))
    (format t "  ~S~%" (first entry))))

(defmethod get-field ((record generic-record) field-id &optional subfield-id)
  "Returns the field (or fields) identified by field-id. The value of a
control field is returned as a string: no interpretation is performed.
Data fields are returned as a list of lists: each sublist contains the
indicator followed by dotted-pairs of each subfield tag and value.
The values are not processed in any way.
"
  (let ((raw-fields (extract-raw-fields record field-id)))
    (if (string= "00" (subseq (string field-id) 0 2))
        ;; control field
        (first raw-fields)
        ;; data field
        (flet ((extract (field)
                 (loop for subfield in (cl-ppcre:split "\\x1f" (subseq field 3))
                    collecting (when (or (null subfield-id)
                                         (string= subfield-id (subseq subfield 0 1)))
                                 (cons (make-keyword (subseq subfield 0 1))
                                       (transcode-string (charset-decoder record)
                                                         (string->byte-array (subseq subfield 1)))))

                        into subfields
                      ;; add the indicator to the start of the subfields
                      finally (return (cons (subseq field 0 2)
                                            (remove-if #'null subfields))))))
          (loop for raw-field in raw-fields
             collect (extract raw-field))))))

;;{{{ MARC-21

(defmethod character-coding-scheme ((record marc21-record))
  (let ((e (char (get-leader record) 9)))
    (cond ((char= e #\Space) :marc8)
          ((char= e #\a) :utf-8)
          (t (error "Unknown character encoding scheme")))))

(defmethod initialize-instance :after ((record marc21-record) &key)
  ;; for the UNAM data even when the leader says it's MARC-8 it looks to be UTF-8 (!)
  ;; FIXME: we need a restart that lets the client define the decoder to use when
  ;;        the leader has an invalid character at position 9.
  ;; FIXME: need a way to override the decoder used for the records for cases like
  ;;        we see with Thai where the leader encoding is bogus and the data is
  ;;        actually encoded in TIS-620 or something similar.
  (setf (charset-decoder record)
        (let ((e (char (get-leader record) 9)))
          (cond ((char= e #\Space) (make-instance 'utf-8-decoder))
                ((char= e #\a) (make-instance 'utf-8-decoder))
                ;; this is a bit of a hack, don't know if utf-8-decoder is right
                (t (make-instance 'utf-8-decoder))))))

(defmethod get-language ((record marc21-record))
  (let ((control (get-field record :|008|)))
    (when control (subseq control 35 38))))

;;}}}

;;{{{ CMARC

(defmethod initialize-instance :after ((record cmarc-record) &key)
  ;; for now (until I get around to fully implementing MARC-8, which is
  ;; needed for CCCII support) I'll just use the identity decoder
  (setf (charset-decoder record)
        (make-instance 'charset-decoder)))

;;}}}

(defmethod dump-unicode-field ((record generic-record) field-id)
  (loop for field in (get-field record field-id) do
       (format t "|~A|~%" (first field))
       ;; there is probably a way to do the following with a single format
       ;; control string but I'm not going to figure that out right now
       (loop for data in (rest field) do
            (format t "  $~A ~A~%" (string (first data))
                    (utf8->string (string->byte-array (rest data)))))))

(defun control-field-p (field-id)
  "Return T if FIELD-ID is a control field, otherwise NIL."
  (< (parse-integer (string field-id)) 10))

(defmethod dump-record ((record generic-record))
  (format t "~a:~%" (get-leader record))
  (let ((seen '()))
    (dolist (entry (slot-value record 'directory))
      (unless (member (first entry) seen)
        (pushnew (first entry) seen)
        (if (< (parse-integer (string (first entry))) 10)
            ;; control field
            (format t "~A  ~A~%" (string (first entry)) (get-field record (first entry)))
            ;; data field
            (progn
              (format t "~A  " (string (first entry)))
              (dump-unicode-field record (first entry))))))))


(defun process-marc-file (filename func record-type)
  "Calls FUNC on each record in the specified FILENAME."
  (flet ((new-string ()
           (make-array 1024 :fill-pointer 0 :adjustable t :element-type 'character)))
    ;; the file is read as ISO-8859-1 to prevent any character corruption when the
    ;; file is transcoded to the implementation's (Unicode) internal string format.
    (with-open-file (stream filename :direction :input :external-format :ISO-8859-1)
      (let ((raw-record (new-string))
            (ignore-whitespace-p t)
            (record-count 0))
        (do ((c (read-char stream nil) (read-char stream nil)))
            ((null c)
             (when (> (length raw-record) 0)
               ;; process the last record we were building up when the file ended
               (funcall func (make-instance record-type :raw-record raw-record))
               (incf record-count)))
          (cond ((and ignore-whitespace-p (whitespace-p c)) t)
                ((char= c +record-terminator+)
                 (funcall func (make-instance record-type :raw-record raw-record))
                 (incf record-count)
                 (setq raw-record (new-string))
                 (setq ignore-whitespace-p t))
                (t 
                 (vector-push-extend c raw-record 256)
                 (setq ignore-whitespace-p nil))))
        record-count))))

(defun read-all-records-in-marc-file (filename record-type)
  "Reads the specified MARC file, returning a list of MARC records"
  (let ((records '()))
    (process-marc-file filename #'(lambda (x) (push x records)) record-type)
    records))

(defun pointed-hebrew-title (record)
  (let ((decoder (make-instance 'utf-8-decoder)))
    (when (string= (get-language record) "heb")
      (let ((transcoded (transcode-string decoder
                                          (string->byte-array (cdadar (get-field record :|245| "a"))))))
        (some (lambda (c) (string= (cl-unicode:general-category c) "Mn")) transcoded)))))

(defun find-pointed-hebrew (filename)
  (let ((records '()))
    (process-marc-file filename (lambda (x) (when (pointed-hebrew-title x) (push x records))) 'marc21-record)
    records))


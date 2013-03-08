;;;; -*- mode: Lisp; coding: utf-8; -*-

;;;; cl-marc --- support for reading Z39.2 records
;;;; Written by Tom Emerson, <temerson@ebscohost.com>
;;;;
;;;; Copyright (c) 2010-2013 EBSCO Publishing. All Rights Reserved.

;;;; NISO Z39.2 Specification: http://bit.ly/ePLWK6
;;;; LoC MARC: http://www.loc.gov/marc/marcdocz.html

(in-package #:cl-marc)

(defconstant +record-separator+ #\Gs)

(defclass marc-record ()
  ((raw-record
    :initarg :raw-record
    :initform (error "raw-record must be supplied"))
   base-address
   ;; a list: (tag len start)
   directory))

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

(defmethod character-coding-scheme ((record marc-record))
  (char (slot-value record 'raw-record) 9))

(defmethod initialize-instance :after ((record marc-record) &key)
  (let* ((base-address (parse-integer (slot-value record 'raw-record)
                                      :start 12 :end 17))
         (directory-length (/ (- base-address 25) 12)))
    (setf (slot-value record 'base-address) base-address)
    (setf (slot-value record 'directory)
          (extract-directory (subseq (slot-value record 'raw-record) 24) directory-length))))

(defmethod extract-raw-fields ((record marc-record) field-tag)
  "Extracts the raw contents of the field(s) specified by field-tag."
  (with-slots (raw-record base-address directory) record
    (flet ((get-field (length start)
             (let ((base (+ base-address start)))
               (subseq raw-record base (+ base length -1)))))
      (loop for (tag length start) in directory
           if (eq field-tag tag) collect (get-field length start)))))

(defmethod dump-directory ((record marc-record))
  (format t "~a:~%" (get-leader record))
  (dolist (entry (slot-value record 'directory))
    (format t "  ~S~%" (first entry))))

(defmethod get-leader ((record marc-record))
  (subseq (slot-value record 'raw-record) 0 24))

(defmethod get-field ((record marc-record) field-id &optional subfield-id)
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
                                       (subseq subfield 1)))
                        into subfields
                      ;; add the indicator to the start of the subfields
                      finally (return (cons (subseq field 0 2)
                                            (remove-if #'null subfields))))))
          (loop for raw-field in raw-fields
             collect (extract raw-field))))))

(defmethod dump-unicode-field ((record marc-record) field-id)
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

(defmethod dump-record ((record marc-record))
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


(defun process-marc-file (filename func)
  "Calls FUNC on each record in the specified FILENAME."
  (flet ((new-string ()
           (make-array 1024 :fill-pointer 0 :adjustable t :element-type 'character)))
    (with-open-file (stream filename)
      (let ((raw-record (new-string))
            (ignore-whitespace-p t)
            (record-count 0))
        (do ((c (read-char stream nil) (read-char stream nil)))
            ((null c)
             (when (> (length raw-record) 0)
               ;; process the last record we were building up when the file ended
               (funcall func (make-instance 'marc-record :raw-record raw-record))
               (incf record-count)))
          (cond ((and ignore-whitespace-p (whitespace-p c)) t)
                ((char= c +record-separator+)
                 (progn
                   (funcall func (make-instance 'marc-record :raw-record raw-record))
                   (incf record-count)
                   (setq raw-record (new-string))
                   (setq ignore-whitespace-p t)))
                (t (progn
                     (vector-push-extend c raw-record 256)
                     (setq ignore-whitespace-p nil)))))
        record-count))))

(defun read-all-records-in-marc-file (filename)
  "Reads the specified MARC file, returning a list of MARC records"
  (let ((records '()))
    (process-marc-file filename #'(lambda (x) (push x records)))
    records))

(defun get-language (record)
  (let ((control (get-field record :|008|)))
    (when control (subseq control 35 38))))

(defun get-unicode-field (marc-record field subfield)
  (let ((raw-value (cdadar (get-field marc-record field subfield))))
    (when raw-value
      (utf8->string (string->byte-array raw-value)))))

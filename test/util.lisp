(defpackage :http-parse-test
  (:use :cl :fiveam :http-parse)
  (:export #:run-tests))
(in-package :http-parse-test)

(defun to-bytes (string)
  (babel:string-to-octets string))

(defun to-string (bytes)
  (babel:octets-to-string bytes))

(defun file-contents (path)
  "Read an entire file in binary form."
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)))
        (buffer (make-array 8096 :element-type '(unsigned-byte 8))))
    (with-open-file (s path :element-type '(unsigned-byte 8))
      (loop for n = (read-sequence buffer s)
            while (< 0 n) do
        (setf bytes (http-parse::append-array bytes (subseq buffer 0 n)))))
    bytes))

(defun write-file (path contents)
  "Write a binary sequence to a file."
  (let ((s (open path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))))
    (write-sequence contents s)
    (close s)))

;; define the test suite
(def-suite http-parse-test :description "cl-async test suite")
(in-suite http-parse-test)

(test append-array
  "Test the append-array function"
  (let ((arr1 #(1 2 3))
        (arr2 #(4 5 6)))
    (is (equalp (http-parse::append-array arr1 arr2) #(1 2 3 4 5 6)))))

(test find-non-whitespace-pos
  "Test finding first non-whitespace character in a byte array"
  (let ((seq1 (babel:string-to-octets "hallo"))
        (seq2 (babel:string-to-octets "  omg lol"))
        (seq3 #(9 56 59)))
    (is (= (http-parse::find-non-whitespace-pos seq1) 0))
    (is (= (http-parse::find-non-whitespace-pos seq2) 2))
    (is (= (http-parse::find-non-whitespace-pos seq3) 1))))
        


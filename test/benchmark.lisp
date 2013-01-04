(in-package :http-parse-test)

;; ---- profile-list ----
;; cl-ppcre::split
;; cl-ppcre::scan
;; http-parse::get-header-block
;; http-parse::convert-headers-plist
;; http-parse::parse-headers
;; http-parse::get-complete-chunks
;; http-parse::make-parser
;; http-parse::append-array
;; http-parse::find-non-whitespace-pos
;; cl::subseq

(defclass benchmark ()
  ((start :accessor benchmark-start :initarg :start :initform (get-internal-real-time))
   (end :accessor :benchmark-end :initarg :end :initform nil)))

(defun finish-benchmark (name benchmark)
  (let* ((end (get-internal-real-time))
         (start (benchmark-start benchmark))
         (seconds (coerce (/ (- end start) internal-time-units-per-second) 'single-float)))
    (format t "BENCH: ~a: ~as~%" name seconds)))

(defun bench-parse-speed ()
  (let ((response (file-contents (asdf:system-relative-pathname :http-parse "test/data/test-response1.http")))
        (bench (make-instance 'benchmark)))
    (dotimes (i 100)
      (let* ((http (make-instance 'http-response))
             (parser (make-parser http :store-body t)))
        (funcall parser response)))
    (finish-benchmark "Parse speed" bench)))
      
(defun run-benchmarks ()
  (bench-parse-speed))

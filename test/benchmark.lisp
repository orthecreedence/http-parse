(in-package :http-parse-test)

(defclass benchmark ()
  ((start :accessor benchmark-start :initarg :start :initform (get-internal-real-time))
   (end :accessor :benchmark-end :initarg :end :initform nil)))

(defun finish-benchmark (name benchmark)
  (let* ((end (get-internal-real-time))
         (start (benchmark-start benchmark))
         (seconds (coerce (/ (- end start) internal-time-units-per-second) 'single-float)))
    (format t "BENCH: ~a: ~as~%" name seconds)))

(defun bench-parse-speed ()
  (let* ((response (file-contents (asdf:system-relative-pathname :http-parse "test/data/test-response1.http")))
         (http (make-instance 'http-response))
         (parser (make-parser http :store-body t))
         (bench (make-instance 'benchmark)))
    (dotimes (i 99)
      (funcall parser response))
    (finish-benchmark "Parse speed" bench)))
      
(defun run-benchmarks ()
  (bench-parse-speed))

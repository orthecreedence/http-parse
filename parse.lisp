(in-package :http-parse)

(defclass http ()
  ((version :accessor http-version :initarg :version :initform 1)
   (headers :accessor http-headers :initarg :headers :initform nil)
   (body :accessor http-body :initarg :body :initform (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t)))
  (:documentation "Base HTTP class, holds data common to both requests and responses."))
   
(defclass http-request (http)
  ((method :accessor http-method :initarg :method :initform nil)
   (resource :accessor http-resource :initarg :resource :initform "/"))
  (:documentation "HTTP request class, extends `http` and holds all request-specific data."))

(defclass http-response (http)
  ((status :accessor http-status :initarg :status :initform 0)
   (status-text :accessor http-status-text :initarg :status-text :initform ""))
  (:documentation "HTTP response class, extends `http` and holds all response-specific data."))

(defmethod print-object ((http http) s)
  (format s "#<HTTP (~a), ~s>"
          (http-version http)
          (http-headers http)))

(defmethod print-object ((http http-request) s)
  (format s "#<HTTP (~a), ~s (~a ~s)>"
          (http-version http)
          (http-headers http)
          (http-method http)
          (http-resource http)))

(defmethod print-object ((http http-response) s)
  (format s "#<HTTP (~a) ~s ~s>"
          (http-version http)
          (format nil "~a ~a" (http-status http) (http-status-text http))
          (http-headers http)))

(defparameter *scanner-header-parse-line*
  (cl-ppcre:create-scanner "\\r\\n" :multi-line-mode t)
  "Create a regex scanner for splitting header lines up.")
(defparameter *scanner-header-parse-kv*
  (cl-ppcre:create-scanner ":[ \s]+" :multi-line-mode t)
  "Create a regex scanner for splitting header kv pairs up.")
(defparameter *scanner-numeric*
  (cl-ppcre:create-scanner "^[0-9\.]+$")
  "Create a regex scanner that detects if a string can be converted to a numver.")
(defparameter *scanner-find-first-header*
  (cl-ppcre:create-scanner "^*[a-z-]+: .*" :case-insensitive-mode t :multi-line-mode t)
  "Create a scanner to find the first header in a string.")

(defun get-header-block (bytes &key get-previous-line)
  "Given the bytes of an HTTP request/response, pull out only the headers and
   optionally the line above the start of the headers. Returns the headers as a
   string and also the start position of the body of the HTTP request."
  (let* ((str (babel:octets-to-string bytes))
         (search-section-end (make-array 4 :element-type 'character :initial-contents #(#\return #\newline #\return #\newline)))
         (header-end (search search-section-end str)))
    (unless header-end
      (return-from get-header-block))
    (let ((header-start (cl-ppcre:scan *scanner-find-first-header* str)))
      (when header-start
        (if get-previous-line
            (let* ((previous-line-pos (or (search #(#\return #\newline) str :end2 (- header-start 2) :from-end t) 0)))
              (subseq str previous-line-pos header-end))
            (subseq str header-start header-end))))))

(defun convert-headers-plist (header-str)
  "Pull out headers in a plist from a string."
  (loop for line in (cl-ppcre:split *scanner-header-parse-line* header-str)
        append (let* ((kv (cl-ppcre:split *scanner-header-parse-kv* line))
                      (numberp (cl-ppcre:scan *scanner-numeric* (cadr kv)))
                      (val (if numberp
                               (read-from-string (cadr kv))
                               (string-downcase (cadr kv)))))
                 (list (intern (string-upcase (car kv)) :keyword)
                       val))))

(defgeneric parse-headers (http bytes)
  (:documentation
    "Given a slew of HTTP bytes, parse the headers out of them along with any
     other useful information lurking in there (status code, resource, etc).
     Returns the HTTP object passed in."))

(defmethod parse-headers ((http http-request) (bytes vector))
  (let ((header-str (get-header-block bytes :get-previous-line t)))
    (when header-str
      (let* ((top-line-end (search #(#\return #\newline) header-str))
             (top-line (subseq header-str 0 top-line-end))
             (header-str (subseq header-str (+ top-line-end 2)))
             (resource-start (search " " top-line))
             (version-start (search "HTTP/" top-line :start2 (1+ resource-start))))
        (setf (http-headers http) (convert-headers-plist header-str))
        (setf (http-method http) (subseq top-line 0 resource-start))
        (if version-start
            (let* ((version-str (subseq top-line version-start))
                   (version (read-from-string (subseq version-str (1+ (search "/" version-str))))))
              (setf (http-resource http) (subseq top-line (1+ resource-start) (1- version-start)))
              (setf (http-version http) version))
            (setf (http-resource http) (subseq top-line (1+ resource-start))))
        (values (http-headers http) http)))))


(defmethod parse-headers ((http http-response) (bytes vector))
  (let ((header-str (get-header-block bytes :get-previous-line t)))
    (when header-str
      (let* ((top-line-end (search #(#\return #\newline) header-str))
             (top-line (subseq header-str 0 top-line-end))
             (header-str (subseq header-str (+ top-line-end 2)))
             (status-start (search " " top-line))
             (status-text-start (search " " top-line :start2 (1+ status-start)))
             (version-str (subseq top-line 0 status-start)))
        (setf (http-headers http) (convert-headers-plist header-str))
        (setf (http-version http) (read-from-string (subseq version-str (1+ (search "/" version-str)))))
        (setf (http-status http) (read-from-string (subseq top-line (1+ status-start) status-text-start)))
        (setf (http-status-text http) (subseq top-line (1+ status-text-start)))
        (values (http-headers http) http)))))

(defun get-complete-chunks (data)
  "Given a chunk (octet vector) of HTTP data, return only the data from the
   *complete* chunks in the data and, return the position in the byte vector
   of the start of the next chunk, and lastly return whether the last chunk (the
   0-byte chunk) has been parsed (ie, HTTP body complete)."
  (let ((last-chunk-start -1)
        (completep nil)
        (chunk-start 0)
        (data-length (length data))
        (chunk-data (make-array 0 :element-type '(unsigned-byte 8)))
        (search-line-end  (make-array 2 :element-type '(unsigned-byte 8) :initial-contents #(13 10))))
    ;; loop over all available chunks until we get a partial
    (loop while (not (= last-chunk-start chunk-start)) do
      (setf last-chunk-start chunk-start)
      (let* ((chunk-blob (subseq data chunk-start data-length))
             (chunk-length-seq-start (or (find-non-whitespace-pos chunk-blob) 0))
             (chunk-length-seq-end (or (search search-line-end chunk-blob :start2 chunk-length-seq-start)
                                       chunk-length-seq-start))
             ;(lol (format t "~%length start/end: ~a/~a~%" chunk-length-seq-start chunk-length-seq-end))
             (chunk-length-seq (subseq chunk-blob chunk-length-seq-start chunk-length-seq-end))
             ;(lol (if (< 24 (length chunk-blob))
             ;         (subseq chunk-blob 0 24)
             ;         chunk-blob))
             ;(lol (format t "CHUNK BEG: ~a ~a (~s)~%" (find-non-whitespace-pos chunk-blob) lol (babel:octets-to-string lol)))
             ;(lol (format t "CHUNK BEG: ~s~%" (babel:octets-to-string chunk-length-seq)))
             (chunk-length (ignore-errors
                             (parse-integer
                               (babel:octets-to-string chunk-length-seq)
                               :radix 16)))
             (chunk-start-pos (+ chunk-length-seq-end 2))
             ;(lol (format t "calculating chunk: ~a + ~a~%" chunk-start-pos chunk-length))
             )
        (unless chunk-length (return))
        (let ((chunk (subseq chunk-blob chunk-start-pos (min (length chunk-blob) (+ chunk-start-pos (or chunk-length 0))))))
          ;(format t "chunk (~s): ~a~%" (length chunk-blob) (subseq (babel:octets-to-string chunk) 0 (min (or chunk-length 0) 60)))
          (cond
            ((eq chunk-length 0)
             ;(format t "zero chunk~%")
             (setf completep t)
             (return))
            ((numberp chunk-length)
             ;(format t "chunk length: ~a/~a~%" (length chunk) chunk-length)
             (when (<= chunk-length (length chunk))
               (setf chunk-data (append-array chunk-data chunk)
                     chunk-start (+ chunk-start chunk-length chunk-start-pos))))
            (t
             (return))))
        ;(format t "last-chunk/start: ~a/~a~%" last-chunk-start chunk-start)
        ))
    (values chunk-data chunk-start completep)))

(defgeneric make-parser (http header-cb data-cb &key store-body)
  (:documentation "Return a parser for an HTTP request/response."))

(defmethod make-parser ((http http) (header-cb function) (data-cb function) &key store-body)
  (let ((http-bytes (make-array 0 :element-type '(unsigned-byte 8)))
        (body-bytes (make-array 0 :element-type '(unsigned-byte 8)))
        (have-headers nil)
        (content-length nil)
        (chunked nil)
        (body-start nil)
        (search-body-start (make-array 4 :element-type '(unsigned-byte 8) :initial-contents #(13 10 13 10))))
    (lambda (data)
      (block parse-wrap
        ;; detect data EOF
        (when (eql data :eof)
          (when store-body
            (setf (http-body http) body-bytes))
          (return-from parse-wrap
                       (values http t t)))

        ;; store the new data
        (setf http-bytes (append-array http-bytes data))
        
        ;; grab/parse the headers. if headers aren't fully present, we return
        ;; without trying to parse the body
        (unless have-headers
          (let ((headers (parse-headers http http-bytes)))
            (if headers
                (let ((content-length-value (getf headers :content-length))
                      (transfer-encoding-value (getf headers :transfer-encoding)))
                  ;; get rid of the headers. we don't need them anymore
                  (setf http-bytes (subseq http-bytes (+ (search search-body-start http-bytes) 4)))
                  (setf body-start 0
                        have-headers t)

                  ;; let "interested parties" know that the headers are complete
                  (funcall header-cb headers)

                  (cond
                    ;; we have a content length. this makes things easy...
                    (content-length-value
                      (setf content-length content-length-value))
                    ;; we're chunking. great...
                    ((string= transfer-encoding-value "chunked")
                     (setf chunked t))
                    ;; no content-length or chunking? assume no body.
                    ;; TODO: is this correct for HTTP 1.0??
                    (t
                     (return-from parse-wrap (values http t t)))))
                (return-from parse-wrap))))

        ;; we have parsed headers. start work on the body
        (cond
          (chunked
            (multiple-value-bind (chunk-data next-chunk-start completep)
                (get-complete-chunks http-bytes)
              (when chunk-data
                (setf body-start 0
                      http-bytes (subseq http-bytes next-chunk-start))
                (funcall data-cb chunk-data)
                (when (and completep store-body)
                  (setf body-bytes (append-array body-bytes chunk-data))
                  (setf (http-body http) body-bytes))
                (return-from parse-wrap (values http t completep)))))
          (content-length
            (let* ((body (subseq http-bytes body-start (length http-bytes)))
                   (body-length (length body)))
              ;; don't "stream" data until we have the full body.
              (when (<= content-length body-length)
                (let ((body (if (= body-length content-length)
                                http-bytes
                                (subseq http-bytes 0 (+ body-start content-length)))))
                  (when store-body
                    (setf (http-body http) body))
                  (funcall data-cb body)
                  (return-from parse-wrap
                               (values http t t))))))
          (t
           (error "Got neither Content-Length nor chunked transfer.")))))))


(in-package :http-parse)

(defclass http ()
  ((version :accessor http-version :initarg :version :initform 1)
   (headers :accessor http-headers :initarg :headers :initform nil)
   (store-body :accessor http-store-body :initarg :store-body :initform nil)
   (force-stream :accessor http-force-stream :initarg :force-stream :initform nil)
   (body :accessor http-body :initarg :body :initform (make-array 0 :element-type '(unsigned-byte 8))))
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

(defun get-header-block (bytes &key get-previous-line)
  "Given the bytes of an HTTP request/response, pull out only the headers and
   optionally the line above the start of the headers. Returns the headers as a
   string."
  ;; search for the telltale \r\n\r\n that marks the end of the headers
  (let* ((header-break (search #(13 10 13 10) bytes))
         (header-block (subseq bytes 0 header-break)))
    (unless header-break
      (return-from get-header-block))
    (let* ((str (babel:octets-to-string header-block :encoding :iso-8859-1))
           (header-start (cl-irregsexp:if-match-bind
                             ((previous (* (progn (* (or #\Space (- #\A #\z) (- #\0 #\9) #\/ #\.)) #\Return #\Newline)))
                              (name (+ (or (- #\A #\z)
                                           (- #\0 #\9)
                                           (= #\-))))
                              #\: (space) val)
                             str
                             (length previous))))
      (when header-start
        (if get-previous-line
            (let ((previous-line-pos (or (search #(#\return #\newline) str :end2 (- header-start 2) :from-end t) 0)))
              (subseq str (find-non-whitespace-pos str :start previous-line-pos)))
            (subseq str header-start))))))

(defun convert-headers-plist (header-str)
  "Pull out headers in a plist from a string."
  (loop for line in (cl-irregsexp:match-split (progn #\Return #\Newline) header-str)
        append (cl-irregsexp:if-match-bind
                   (key ":" (* (space)) val)
                   line
                   (list (intern (string-upcase key) :keyword)
                         (cl-irregsexp:if-match-bind ((num (float)) (last)) val num val)))))

(defgeneric parse-headers (http bytes)
  (:documentation
    "Given a slew of HTTP bytes, parse the headers out of them along with any
     other useful information lurking in there (status code, resource, etc).
     Returns the HTTP object passed in."))

(defmethod parse-headers ((http http-request) (bytes vector))
  (let ((header-str (get-header-block bytes :get-previous-line t)))
    (when header-str
      (cl-irregsexp:match-bind
          (method (space) resource (or (progn (space) "HTTP/" version #\Return #\Newline)
                                       (progn #\Return #\Newline))
                  headers)
          header-str
        (setf (http-headers http) (convert-headers-plist headers))
        (setf (http-method http) (intern method :keyword))
        (setf (http-resource http) resource)
        (setf (http-version http) (read-from-string version))
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
   *complete* chunks in the data and return the position in the byte vector of
   the start of the next chunk, and lastly return whether the last chunk (the
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
             (chunk-length-seq (subseq chunk-blob chunk-length-seq-start chunk-length-seq-end))
             (chunk-length (ignore-errors
                             (parse-integer
                               (babel:octets-to-string chunk-length-seq)
                               :radix 16)))
             (chunk-start-pos (+ chunk-length-seq-end 2)))
        (unless chunk-length (return))
        (let ((chunk (subseq chunk-blob chunk-start-pos (min (length chunk-blob) (+ chunk-start-pos (or chunk-length 0))))))
          (cond
            ((eq chunk-length 0)
             (setf completep t)
             (return))
            ((numberp chunk-length)
             (when (<= chunk-length (length chunk))
               (setf chunk-data (append-array chunk-data chunk)
                     chunk-start (+ chunk-start chunk-length chunk-start-pos))))
            (t
             (return))))))
    (values chunk-data chunk-start completep)))

(defun make-parser (http &key header-callback body-callback multipart-callback finish-callback store-body)
  "Return a closure that parses an HTTP request/response by calling it with
   the bytes received as its only argument. The closure returns three values:
   the http object passed in, a boolean representing whether the headers have
   been fully parsed, and a boolean representing whether the request/response
   is finished (blank body, all body bytes accounted for, or 0-length chunk
   received).
   
   During the parsing, the closure will call (if specified) the `header-callback`
   with all the headers as a plist once they are fully parsed, and the
   `body-callback` with the body once either it finishes parsing (if we have
   Content-Length) or once for each set of completed chunks sent, which allows
   streaming the body as it comes in. If a multipart callback is given, it will
   be called at least once for each form field present in the multipart form
   data.

   The `:finish-callback` will be called when the HTTP payload is fully
   processed.
   
   The :store-body keyword indicates to the parser that we wish to keep the
   body (in its entirety) in the http object passed in (accessible via the
   http-body accessor). Otherwise, the body will be discarded as it's parsed
   (but remember, will still be sent to the body-callback as it comes in).
   
   Parsing can be forced to completion by passing :EOF into the data arg. It
   is recommended to do this if the client/server closes the connection before
   you do."
  (setf (http-store-body http) store-body)
  ;; create the main closure
  (let ((http-bytes (make-array 0 :element-type '(unsigned-byte 8)))
        (body-bytes (make-array 0 :element-type '(unsigned-byte 8)))
        (have-headers nil)
        (content-length nil)
        (chunked nil)
        (forced-chunk-bytes 0)
        (multipart-parser nil)  ; we'll init this after we get headers
        (search-body-start (make-array 4 :element-type '(unsigned-byte 8) :initial-contents #(13 10 13 10)))
        (100-continue-search (babel:string-to-octets "100 Continue")))
    (lambda (data)
      (block parse-wrap
        ;; detect data EOF
        (when (eql data :eof)
          (when (http-store-body http)
            (setf (http-body http) body-bytes))
          (when finish-callback
            (funcall finish-callback))
          (return-from parse-wrap
                       (values http t t)))

        ;; store the new data
        (setf http-bytes (append-array http-bytes data))

        ;; eliminate "100 Continue" blocks
        (let* ((continue-position (search 100-continue-search http-bytes :from-end t)))
          (when continue-position
            (setf http-bytes (subseq http-bytes
                                     (find-non-whitespace-pos http-bytes :start (+ continue-position
                                                                                   (length 100-continue-search)))))))

        ;; grab/parse the headers. if headers aren't fully present, we return
        ;; without trying to parse the body
        (unless have-headers
          (let ((headers (parse-headers http http-bytes)))
            (if headers
                (let ((content-length-value (getf headers :content-length))
                      (transfer-encoding-value (getf headers :transfer-encoding)))
                  ;; get rid of the headers. we don't need them anymore
                  (setf http-bytes (subseq http-bytes (+ (search search-body-start http-bytes) 4)))
                  (setf have-headers t)

                  ;; let "interested parties" know that the headers are complete
                  (when header-callback
                    (funcall header-callback headers))

                  ;; we now have headers, so if we want to parse multipart data,
                  ;; create the parser
                  (when multipart-callback
                    (setf multipart-parser (make-multipart-parser headers multipart-callback)))

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
                     (when finish-callback
                       (funcall finish-callback))
                     (return-from parse-wrap (values http t t)))))
                (return-from parse-wrap (values http nil nil)))))

        ;; we have parsed headers. start work on the body
        (cond
          (chunked
            (multiple-value-bind (chunk-data next-chunk-start completep)
                (get-complete-chunks http-bytes)
              (when (or completep (< 0 (length chunk-data)))
                (setf http-bytes (subseq http-bytes next-chunk-start))
                (when body-callback
                  (funcall body-callback chunk-data completep))
                (when multipart-parser
                  (funcall multipart-parser chunk-data))
                (when (http-store-body http)
                  (setf body-bytes (append-array body-bytes chunk-data)
                        (http-body http) body-bytes))
                (when (and completep finish-callback)
                  (funcall finish-callback)))
              (return-from parse-wrap (values http t completep))))
          (content-length
            (let* ((body (subseq http-bytes 0 (length http-bytes)))
                   (body-length (length body)))
              (if (http-force-stream http)
                  ;; we're forcing streaming into the callback even though we're
                  ;; not chunking. this can be useful for dealing with large
                  ;; amounts of data that a stupid client, which is stupid,
                  ;; sends as one big request instead of chunking it. this is
                  ;; specifically in regards to XHR in browsers, which doesn't
                  ;; support HTTP chunked uploads. brilliant.
                  (progn
                    (incf forced-chunk-bytes (length http-bytes))
                    (let ((finishedp (<= content-length forced-chunk-bytes)))
                      (when body-callback
                        (funcall body-callback http-bytes finishedp))
                      (when multipart-parser
                        (funcall multipart-parser http-bytes))
                      (setf http-bytes (make-array 0 :element-type '(unsigned-byte 8)))
                      (when finishedp
                        (funcall finish-callback))))
                  ;; don't "stream" data until we have the full body.
                  (when (<= content-length body-length)
                    (let ((body (if (= body-length content-length)
                                    http-bytes
                                    (subseq http-bytes 0 (+ 0 content-length)))))
                      (when (http-store-body http)
                        (setf (http-body http) body))
                      (when body-callback
                        (funcall body-callback body t))
                      (when multipart-parser
                        (funcall multipart-parser body))
                      (when finish-callback
                        (funcall finish-callback))
                      (return-from parse-wrap (values http t t)))))))
          (t
            ;; no content length, no chunking, I smell a request with no body
            (when finish-callback
              (funcall finish-callback))
            (return-from parse-wrap (values http t t))))))))


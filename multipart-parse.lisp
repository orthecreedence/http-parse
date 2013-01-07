(in-package :http-parse)

(defparameter *scanner-content-disposition-kv-pairs*
  (cl-ppcre:create-scanner "([a-z-]+)=\"([^\"]+)\"" :case-insensitive-mode t)
  "Grabs the key/value pairs from a Content-Disposition header.")

(defun get-header-kv-pairs (content-disposition-header-str)
  "Given a content-disposition header value, pull out the key/value pairs in it."
  (let ((pairs nil))
    (cl-ppcre:do-matches-as-strings (match *scanner-content-disposition-kv-pairs*
                                      content-disposition-header-str)
      (let* ((key (subseq match 0 (position #\= match)))
             (first-quote (1+ (position #\" match)))
             (value (subseq match first-quote (position #\" match :start first-quote))))
        (push value pairs)
        (push (intern (string-upcase key) :keyword) pairs)))
    pairs))

(defun make-multipart-parser (headers callback)
  "Make a multipart parser. Returns a closure that accepts a byte array of data
   from an HTTP body. Can be sent in in chunks. Callback will be called whenever
   a complete data chunk is sent in (called for each part in the data chunk) or
   as a continuation of a chunk that complete headers were sent in for but
   didn't finish sending in its body."
  (let* ((content-type-header (getf headers :content-type))
         (boundary-str (subseq content-type-header
                               (+ (search "boundary=" content-type-header) 9)))
         (boundary-str (concatenate 'string "--" boundary-str))
         (boundary (babel:string-to-octets boundary-str))
         (boundary-length (length boundary))
         (data (make-array 0 :element-type '(unsigned-byte 8)))
         (just-finished-chunk nil)
         (current-field-headers nil)
         (current-field-kv nil)
         (current-field-name nil))
    (lambda (chunk-data)
      (setf data (append-array data chunk-data))
      (block leave-parser
        (flet ((send-data-to-callback (body-start body-end body-complete-p)
                 (let ((send-data (subseq data body-start (if body-complete-p body-end nil))))
                   (funcall callback
                            current-field-name current-field-headers current-field-kv
                            send-data
                            body-complete-p))
                 (if body-complete-p
                     (progn
                       (setf current-field-headers nil
                             current-field-name nil
                             current-field-kv nil
                             just-finished-chunk t)
                       (setf data (subseq data (+ 2 body-end boundary-length))))
                     (setf data (make-array 0 :element-type '(unsigned-byte 8))))))
          ;; loop until no more boundaries found
          (loop while (or (search boundary data)
                          just-finished-chunk)
                for data-length = (length data) do
            ;; if this is the very first block (boundary is at the beginning of the
            ;; data), remove the boundary. makes parsing a bit easier.
            (when (and (not current-field-name)
                       (not just-finished-chunk)
                       (zerop (search boundary data)))
              (setf data (subseq data (+ 2 boundary-length))))

            (setf just-finished-chunk nil)

            ;; act differently depending on whether we're parsing a new part or an
            ;; existing
            (if current-field-headers
                ;; we are inside of an existing chunk.
                (let* ((body-end (- (or (search boundary data) 2) 2))
                       (body-complete-p (not (zerop body-end))))
                  (send-data-to-callback 0 body-end body-complete-p))

                ;; new chunk, set everything up (assuming we have the correct info)
                (let ((header-block (get-header-block data)))
                  (when header-block
                    ;; we have a header block, we can probably continue
                    (let* ((headers (convert-headers-plist header-block))
                           (kv (get-header-kv-pairs (getf headers :content-disposition)))
                           (field-name (getf kv :name))
                           (body-start (+ 4 (or (search #(13 10 13 10) data) 0)))
                           (body-end (- (or (search boundary data) 2) 2))
                           (body-complete-p (not (zerop body-end))))
                      (setf current-field-headers headers
                            current-field-kv kv
                            current-field-name field-name)
                      (send-data-to-callback body-start body-end body-complete-p)))))
            (when (= (length data) data-length)
              (return-from leave-parser))))))))


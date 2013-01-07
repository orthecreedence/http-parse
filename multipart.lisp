(in-package :http-parse)

(defun split-parts (boundary body-bytes)
  "Return a list of the different multipart form pieces (in order)."
  (let ((pieces nil)
        (boundary-length (length boundary))
        (last-boundary-2 0))
    (loop for boundary1 = (search boundary body-bytes :start2 last-boundary-2)
          for boundary2 = (search boundary body-bytes :start2 (1+ (or boundary1 0)))
          while (and boundary1 boundary2) do
      (let ((content (subseq body-bytes
                             (+ boundary1 boundary-length 2)
                             (- boundary2 2))))
        (push content pieces))
      (setf last-boundary-2 (identity boundary2)))
    (reverse pieces)))

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

(defun parse-part (part-bytes)
  "Given the inner content between two boundaries, parse out the headers, the
   key/value pairs from the Content-Disposition header, and the body content
   within the part."
  (let* ((headers (convert-headers-plist (get-header-block part-bytes)))
         (kv-pairs (get-header-kv-pairs (getf headers :content-disposition)))
         (content (subseq part-bytes (+ 4 (search #(13 10 13 10) part-bytes)))))
    (values kv-pairs headers content)))

(defun decode-multipart-body (content-type-header-value body-bytes)
  "Given a Content-Type header value and a body in multipart/form-data format,
   decode the form data into a hash table of field-name => byte array (value)."
  (let ((form-data (make-hash-table :test #'equal))
        (file-data (make-hash-table :test #'equal)))
    ;; first off, make sure we have a multipart header
    (when (search "multipart/form-data" content-type-header-value)
      (let* ((boundary-str (subseq content-type-header-value
                                   (+ (search "boundary=" content-type-header-value) 9)))
             (boundary-str (concatenate 'string "--" boundary-str))
             (boundary (babel:string-to-octets boundary-str)))
        (dolist (piece (split-parts boundary body-bytes))
          (multiple-value-bind (kv-pairs headers content)
              (parse-part piece)
            (let ((name (getf kv-pairs :name "unnamed-form-var")))
              (if (getf kv-pairs :filename)
                  (setf (gethash name file-data) (list :mime-type (getf headers :content-type)
                                                       :filename (getf kv-pairs :filename)
                                                       :data content))
                  (setf (gethash name form-data) (babel:octets-to-string content))))))))
    (values form-data file-data)))


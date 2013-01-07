(in-package :http-parse-test)

(test multipart-form-parsing
  "Test parsing of multipart form data"
  (let* ((content (file-contents (asdf:system-relative-pathname :http-parse "test/data/multipart.http")))
         (http (make-instance 'http-request))
         (parser (make-parser http :store-body t)))
    (funcall parser content)
    (multiple-value-bind (form-data file-data)
        (decode-multipart-body (getf (http-headers http) :content-type)
                               (http-body http))
      (is (string= (gethash "name" form-data) "wookie"))
      (is (string= (gethash "power" form-data) "growl"))
      (is (equalp (gethash "uploadz" file-data)
                  '(:mime-type "application/octet-stream"
                    :filename "test.lisp"
                    :data #(40 102 111 114 109 97 116
                            32 116 32 34 111 109 103
                            108 111 108 119 116 102 126
                            37 34 41 10)))))))

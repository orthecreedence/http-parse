(in-package :http-parse)

#|

POST / HTTP/1.1
User-Agent: curl/7.24.0
Host: 127.0.0.1:8090
Accept: */*
Content-Length: 422
Content-Type: multipart/form-data; boundary=----------------------------ec528d704fcf

------------------------------ec528d704fcf
Content-Disposition: form-data; name="name"

wookie
------------------------------ec528d704fcf
Content-Disposition: form-data; name="power"

growl
------------------------------ec528d704fcf
Content-Disposition: form-data; name="cracker"; filename="hello.lisp"
Content-Type: application/octet-stream

(format t "HAI~%")

------------------------------ec528d704fcf--

|#

(defun decode-multipart (content-type-header-value body-bytes)
  "Given a Content-Type header value and a body in multipart/form-data format,
   decode the form data into a hash table of field-name => byte array (value)."
  ;; first off, make sure we have a multipart header
  (when (search "multipart/form-data" content-type-header-value)
    ))


(in-package :http-parse-test)
(in-suite http-parse-test)

(defparameter *http-response1*
  (format nil "~
HTTP/1.1 100 Continue~c~c~
HTTP/1.1 100 Continue~c~c~
HTTP/1.1 200 OK~c~c~
Content-Type: text/plain~c~c~
Content-Length: 13~c~c~
~c~c~
{name:andrew}"
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline))

(defparameter *http-response2*
  (format nil "~
HTTP/1.1 200 OK~c~c~
Content-Type: application/vnd.musio.users~c~c~
Transfer-Encoding: chunked~c~c~
Cache-Control: public; max-age=69~c~c~
~c~c~
B~c~c~
{name:lucy,~c~c~
6~c~c~
age:3,~c~c~
2f~c~c~
likes:[acting sheepish,running,peeing,barking]}~c~c~
0~c~c"
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline))

(defparameter *http-response3*
  (format nil "~
HTTP/1.1 200 OK~c~c~
x-amz-id-2: 0kZfRJJkWSF7zj/eB94TOInNFIByGmN2Y0SDA5iJvWkICqREJ+MnocUtzukeSKKy~c~c~
x-amz-request-id: 5065F842E041E3CB~c~c~
Date: Sat, 28 Dec 2013 03:48:33 GMT~c~c~
Transfer-Encoding: chunked~c~c~
Server: AmazonS3~c~c~
~c~c~
171~c~c~
<?xml version=\"1.0\" encoding=\"UTF-8\"?>~c~c~
<InitiateMultipartUploadResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\"><Bucket>dev.turtl.it</Bucket><Key>files/52ab8473735ca40d5c000004</Key><UploadId>R3CjDEtAcsd4Vo5J1FoIAtxDWM4A4brHdgO8HtefBt1VmC3UFxr.wHibQ2iFD_Jiinl0QWaij.ht1bC4PzHuH4csv3O9a2wC4Kzt6aPNdf55pwKFImGy10mM8FGIZ6zD</UploadId></InitiateMultipartUploadResult>"
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline))

(defparameter *http-request1*
  (format nil "~
POST /users/dead HTTP/1.1~c~c~
Host: betthefarm.facebook.ru~c~c~
Content-Type: application/vnd.musio.users~c~c~
Transfer-Encoding: chunked~c~c~
~c~c~
C~c~c~
{name:larry,~c~c~
8~c~c~
age:512,~c~c~
25~c~c~
likes:[cussin,bitin,stealin,fightin]}~c~c~
0~c~c"
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline
          #\return #\newline))

(test get-header-block
  "Test that getting the entire header block works"
  (let* ((header-str *http-response1*)
         (bytes (to-bytes header-str)))
    (is (string= (http-parse::get-header-block bytes :get-previous-line t)
                 (format nil "HTTP/1.1 200 OK~c~cContent-Type: text/plain~c~cContent-Length: 13"
                         #\return #\newline
                         #\return #\newline)))
    (is (string= (http-parse::get-header-block bytes)
                 (format nil "Content-Type: text/plain~c~cContent-Length: 13"
                         #\return #\newline)))))

(test parse-headers
  "Test some pesky Amazon responses."
  (let* ((http-bytes (to-bytes *http-response3*))
         (http (make-instance 'http-response)))
    (http-parse::parse-headers http http-bytes)))

(test convert-headers-plist
  "Test converting headers to a plist"
  (let ((header-str (format nil "~
Transfer-Encoding: chunked~c~c~
Content-Type: application/json~c~c~
Date: May 7, 2028"
                            #\return #\newline
                            #\return #\newline)))
    (is (equalp (http-parse::convert-headers-plist header-str)
                '(:transfer-encoding "chunked"
                  :content-type "application/json"
                  :date "May 7, 2028")))))

(test parse-http-request
  "Test parsing an HTTP request"
  (let ((parsed-headers nil)
        (body-chunks nil))
    (let* ((request-str *http-request1*)
           (request-bytes (to-bytes request-str))
           (chunk1-end (1+ (search #(10 67 13) request-bytes)))
           (chunk2-end (search (to-bytes "bitin") request-bytes))
           (chunk1 (subseq request-bytes 0 chunk1-end))
           (chunk2 (subseq request-bytes chunk1-end chunk2-end))
           (chunk3 (subseq request-bytes chunk2-end))
           (http (make-instance 'http-parse:http-request))
           (parser (make-parser http
                                :store-body t
                                :header-callback (lambda (h)
                                                   (setf parsed-headers h))
                                :body-callback (lambda (data finishedp)
                                                 (declare (ignore finishedp))
                                                 (push data body-chunks)))))
      (multiple-value-bind (http-ret headers-complete body-complete)
          (funcall parser chunk1)
        (is (eql http http-ret) "chunk1 http: ~a != ~a" http-ret http)
        (is (eq headers-complete t) "chunk1 headers complete: ~a != ~a" headers-complete t)
        (is (eq body-complete nil) "chunk1 body complete: ~a != ~a" body-complete t))

      (multiple-value-bind (http-ret headers-complete body-complete)
          (funcall parser chunk2)
        (is (eql http http-ret) "chunk2 http: ~a != ~a" http-ret http)
        (is (eq headers-complete t) "chunk2 headers complete: ~a != ~a" headers-complete t)
        (is (eq body-complete nil) "chunk2 body complete: ~a != ~a" body-complete t))

      (multiple-value-bind (http-ret headers-complete body-complete)
          (funcall parser chunk3)
        (is (eql http http-ret) "chunk3 http: ~a != ~a" http-ret http)
        (is (eq headers-complete t) "chunk3 headers complete: ~a != ~a" headers-complete t)
        (is (eq body-complete t) "chunk3 body complete: ~a != ~a" body-complete t))
      
      (is (= (http-version http) 1.1))
      (is (eq (http-method http) :post))
      (is (string= (http-resource http) "/users/dead"))
      (is (string= (to-string (http-body http)) "{name:larry,age:512,likes:[cussin,bitin,stealin,fightin]}")))
    (is (equalp parsed-headers
                '(:host "betthefarm.facebook.ru"
                  :content-type "application/vnd.musio.users"
                  :transfer-encoding "chunked")))
    (is (equalp body-chunks
                (list (to-bytes "likes:[cussin,bitin,stealin,fightin]}")
                      (to-bytes "{name:larry,age:512,"))))))

(test parse-http-response
  "Test parsing an HTTP response"
  (let ((parsed-headers nil)
        (body-chunks nil)
        (finished 0))
    (let* ((request-str *http-response2*)
           (request-bytes (to-bytes request-str))
           (chunk1-end (1+ (search #(10 66 13) request-bytes)))
           (chunk2-end (search (to-bytes "age:3") request-bytes))
           (chunk1 (subseq request-bytes 0 chunk1-end))
           (chunk2 (subseq request-bytes chunk1-end chunk2-end))
           (chunk3 (subseq request-bytes chunk2-end))
           (http (make-instance 'http-parse:http-response))
           (parser (make-parser http
                                :store-body t
                                :header-callback (lambda (h)
                                                   (setf parsed-headers h))
                                :body-callback (lambda (data finishedp)
                                                 (declare (ignore finishedp))
                                                 (push data body-chunks))
                                :finish-callback (lambda () (incf finished)))))
      (multiple-value-bind (http-ret headers-complete body-complete)
          (funcall parser chunk1)
        (is (eql http http-ret) "chunk1 http: ~a != ~a" http-ret http)
        (is (eq headers-complete t) "chunk1 headers complete: ~a != ~a" headers-complete t)
        (is (eq body-complete nil) "chunk1 body complete: ~a != ~a" body-complete t))

      (multiple-value-bind (http-ret headers-complete body-complete)
          (funcall parser chunk2)
        (is (eql http http-ret) "chunk2 http: ~a != ~a" http-ret http)
        (is (eq headers-complete t) "chunk2 headers complete: ~a != ~a" headers-complete t)
        (is (eq body-complete nil) "chunk2 body complete: ~a != ~a" body-complete t))

      (multiple-value-bind (http-ret headers-complete body-complete)
          (funcall parser chunk3)
        (is (eql http http-ret) "chunk3 http: ~a != ~a" http-ret http)
        (is (eq headers-complete t) "chunk3 headers complete: ~a != ~a" headers-complete t)
        (is (eq body-complete t) "chunk3 body complete: ~a != ~a" body-complete t))
      
      (is (= (http-version http) 1.1))
      (is (eq (http-status http) 200))
      (is (string= (http-status-text http) "OK"))
      (is (string= (to-string (http-body http)) "{name:lucy,age:3,likes:[acting sheepish,running,peeing,barking]}")))
    (is (equalp parsed-headers
                '(:content-type "application/vnd.musio.users"
                  :transfer-encoding "chunked"
                  :cache-control "public; max-age=69")))
    (is (equalp body-chunks
                (list (to-bytes "age:3,likes:[acting sheepish,running,peeing,barking]}")
                      (to-bytes "{name:lucy,"))))
    (is (= finished 1))))

(test real-response
  "Test parsing of an honest to god (not made up) HTTP response"
  (let* ((response (file-contents (asdf:system-relative-pathname :http-parse "test/data/test-response1.http")))
         ;; load body from curl
         (body (file-contents (asdf:system-relative-pathname :http-parse "test/data/test-response1.body.http")))
         (http (make-instance 'http-response))
         (parser (make-parser http :store-body t)))
    (funcall parser response)
    (is (equalp (http-headers http)
                '(:server "nginx"
                  :date "Wed, 02 Jan 2013 06:17:06 GMT"
                  :content-type "text/html; charset=utf-8"
                  :transfer-encoding "chunked"
                  :connection "close"
                  :vary "Accept-Encoding")))
    (is (= (http-version http) 1.1))
    (is (= (http-status http) 200))
    (is (string= (http-status-text http) "OK"))
    (is (equalp body (http-body http)))))


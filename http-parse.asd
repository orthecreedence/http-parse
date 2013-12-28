(asdf:defsystem http-parse
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1.8"
  :description "A library for parsing HTTP requests/responses (synchronous or asynchronous)."
  :depends-on (#:babel #:cl-ppcre)
  :components
  ((:file "package")
   (:file "util" :depends-on ("package"))
   (:file "parse" :depends-on ("util"))
   (:file "multipart-parse" :depends-on ("util"))))


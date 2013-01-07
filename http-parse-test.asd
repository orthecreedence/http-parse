(asdf:defsystem http-parse-test
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :version "0.1"
  :description "Tests for http-parse."
  :depends-on (#:http-parse #:babel #:eos)
  :components
  ((:module test
    :serial t
    :components ((:file "util")
                 (:file "parse")
				 (:file "run")))))


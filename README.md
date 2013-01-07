http-parse
==========
A pure-lisp library for parsing HTTP requests/responses. Right now the focus for
this library is making it useful and easy to use. With time, slower parts of
http-parse will be replaced to make it screaming fast.

The purpose of this library is to be able to easily parse incoming HTTP data
either synchronously *or* asynchronously (but it was mainly built for streaming
HTTP data asynchronously).

Documentation
-------------

### http (class)
This is a base class extended by [http-request](#http-request) and [http-response](#http-response).
It holds values that both requests/responses deal with (HTTP version, headers,
body).

This class, and those that extend it, are meant to be instiantiated with no
parameters and passed into the [make-parser](#make-parser) function, which fills
in all the details which can be read out later.

##### http-version
Accessor for the parsed HTTP version out of the http object.

##### http-headers
Accessor for the headers parsed from the HTTP request/response

##### http-body
Accessor for the full HTTP body from the request/response (although storing of the
body in the `http` object must be explicitely asked for by passing `:store-body t`
into [make-parser](#make-parser).

### http-request (class)
_extends [http](#http)_

Holds values specific to an HTTP request (method, resource)

##### http-method
Accessor for the parsed HTTP request method as a keyword: `:GET`, `:POST`, etc.

##### http-resource
Accessor for the resource in the request. Parse with [puri](http://www.cliki.net/puri).

### http-response (class)
_extends [http](#http)_

Holds values specific to an HTTP response (status, status text).

##### http-status
Accessor for the HTTP response status (integer).

##### http-status-text
Accessor for the HTTP response status string: `OK`, `Insufficient Privileges`,
etc.

### make-parser (function)
```common-lisp
(defun make-parser (http &key header-callback body-callback store-body)
  => lambda
```

This is what you've all been waiting for, folks. This function initializes an
HTTP parser (a closure) which can be fed binary data in sequence and will
parse an HTTP request/response as it comes in.

It accepts a class of [http-request](#http-request) or [http-response](#http-response)
as its only required argument. It returns a closure which only has one argument:
_a byte array that contains pieces of an HTTP request/response_. The pieces must
be in order, but other than that, there is no restriction on how many times the
parser can be called with new data until it is finished. In some cases (older
HTTP versions), the end of an HTTP payload is marked by an EOF on the socket. If
this occurs, you can pass `:eof` into the parser instead of a byte array to
signal that it should finish up.

The parser closure returns three values: the [http](#http) object passed in, a
boolean indicating if the headers are finished parsing, and a boolean indicating
if the HTTP body has been fully parsed.

`make-parser` accepts two callbacks, `:header-callback` and `:body-callback`. The
[header callback](#make-parser-header-callback) is fired when all the headers have
been parsed. It takes one argument, a plist of finished headers. The [body
callback](#make-parser-body-callback) is called either when the entire body has
been received (in the case of `:content-length` being present in the headers) or
piece by piece as it is sent in (when the body is chunked).

The `:store-body` keyword specifies that the parser should store the body (as a
byte array) into the given [http](#http) object as it is parsed. Otherwise, the
best way to get the body data is via the [body-callback](#make-parser-body-callback).

```common-lisp
;; example. anything under my-app is not included.
(let ((http (make-instance 'http-response))
      (parser (make-parser http
                           :header-callback (lambda (headers)
                                              (my-app:got-headers!!! headers))
                           :body-callback (lambda (bytes)
                                              (my-app:got-body-piece bytes)))))
  (loop for http-data = (my-app:get-http-data-from-request-i-sent-out-earlier) do
    (multiple-value-bind (http headers-finished-p body-finished-p)
        (funcall parser http-data)
      (when body-finished-p
        (my-app:close-http-stream))
      ...)))
```

##### Parser lambda definition
```common-lisp
(lambda (byte-array) ...)
  => http, headers-finished-p, body-finished-p
```

As noted, if an EOF happens on the socket the HTTP data is coming in on, you may
indicate this to the parser by sending in `:eof` instead of the byte array.

##### header-callback definition
```common-lisp
(lambda (header-plist) ...)
```

Headers are in the form `'(:host "musio.com" :content-type "text/html" ...)`.
Headers are __not__ reversed, they are passed in the order they occur in the
HTTP payload.

##### body-callback definition
```common-lisp
(lambda (byte-array) ...)
```

Byte-array is __not__ cumulative, it is just the *new* data that has been parsed
from the payload. If multiple chunks are parsed at once, their body data is sent
in as one call to the `body-callback`. Incomplete chunks are *not* sent in until
they are completed.

Tests
-----
Tests are under the `http-parse-test` package:

```common-lisp
(ql:quickload :http-parse-test)
(http-parse-test:run-tests)
```

Please report any bugs you find or failing tests to the [issues list](https://github.com/orthecreedence/http-parse/issues).

License
-------
MIT Licensed. Enjoy.

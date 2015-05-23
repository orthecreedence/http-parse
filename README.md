# This library has been superceded by [fast-http](https://github.com/fukamachi/fast-http)
http-parse had a good, long life and served many HTTP requests, but it's now
time for it to stand aside and let libraries better than itself take its place.
[fast-http](https://github.com/fukamachi/fast-http) is an incredible library
(and is now the core parser used in [Wookie](http://wookie.lyonbros.com)).

Use fast-http instead of http-parse. This library is retired.

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

##### http-store-body
`store-body` specifies whether the HTTP body should be stored (in its entirety)
in the [http-body](#http-body) accessor. If this is set after initializing a
parser, it should be done so no later than the [header-callback](#header-callback-definition)
being fired, or else pieces of the body may be missing.

##### http-force-stream
`force-stream` lets the parser know that you want every TCP packet that comes in
to be passed into a body callback *as if it was sent via an HTTP chunk*. This is
an advanced option, but can be very useful in some cases. For instance, if you
have a server that supports file uploads and a client doesn't know how to chunk
an upload (like every browser ever), your server is going to spin its CPU
and waste memory buffering the entire file and passing it around as a huge array
instead of dealing with it packet by packet. This is a great way to fake HTTP
chunking in your server/client.

##### http-body
Accessor for the full HTTP body from the request/response (although storing of the
body in the `http` object must be explicitely asked for by passing `:store-body t`
into [make-parser](#make-parser).

### http-request (class)
_extends [http](#http-class)_

Holds values specific to an HTTP request (method, resource)

##### http-method
Accessor for the parsed HTTP request method as a keyword: `:GET`, `:POST`, etc.

##### http-resource
Accessor for the resource in the request. Parse with [puri](http://www.cliki.net/puri).

### http-response (class)
_extends [http](#http-class)_

Holds values specific to an HTTP response (status, status text).

##### http-status
Accessor for the HTTP response status (integer).

##### http-status-text
Accessor for the HTTP response status string: `OK`, `Insufficient Privileges`,
etc.

### make-parser (function)
```common-lisp
(defun make-parser (http &key header-callback body-callback multipart-callback finish-callback store-body)
  => closure
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

`make-parser` accepts these callbacks: `:header-callback`, `:body-callback`, 
`:multipart-callback`, and `:finish-callback`.

- The [header callback](#header-callback-definition) is fired when all the
headers have been parsed. It takes one argument, a plist of finished headers.
- The [body callback](#body-callback-definition) is called either when the
entire body has been received (in the case of `:content-length` being present in
the headers) or piece by piece as it is sent in (when the body is chunked).
- The [multipart callback](#multipart-callback-definition), if specified, is
passed into a multipart parser, which is given chunks of the body as they come
in. It decodes multipart form data such that the given callback is fired for
each form field present in the data. If it encounters a field that is split
into multiple chunks, it will fire the callback for each of the chunks,
indicating in one of the arguments whether that is the final chunk or not. This
makes it possible to stream the multipart data as it comes in (for instance, to
a file).
- The [finish-callback](#finish-callback-definition) is a function with no args
called when the parser has detected that the HTTP payload is completely parsed
(headers, body, etc).

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
(lambda (byte-array last-chunk-p) ...)
```

Byte-array is __not__ cumulative, it is just the *new* data that has been parsed
from the payload. If multiple chunks are parsed at once, their body data is sent
in as one call to the `body-callback`. Incomplete chunks are *not* sent in until
they are completed.

`last-chunk-p` is true if the entire body has been processed (if a `Content-Length`
was specified and all bytes accounted for, or if the body is chunked and the 
0-byte chunk has been encountered).

##### multipart-callback definition
See [multipart parser callback definition](#multipart-parser-callback-definition).

##### finish-callback definition
```common-lisp
(lambda () ...)
```
This callback is fired when the HTTP parser is finished parsing the
request/response.

### make-multipart-parser (function)
```common-lisp
(defun make-multipart-parser (headers callback))
  => closure
```
Returns a parser closure to deal with multipart form data. Data is fed to the
parser in as many chunks as needed (or all at once) and the given `callback`
will be fired at least once for each form field present in the multipart form
data. If data for a field is spread over multiple chunks, the callback is fired
for each of the chunks, along with a second argument indicating whether the
current chunk is that last for that field.

`headers` are all the headers from a parsed HTTP payload, in plist form.

NOTE: If `make-multipart-parser` detects that the data being decoded is *not* in
multipart format (determined by reading the headers), it returns `nil` instead
of a closure.

##### multipart parser callback definition
```common-lisp
(lambda (field-name field-headers field-meta body-bytes body-complete-p) ...)
```

This callback is fired for each form field encountered in a multipart request.

The `field-name` arg is a string indicating the name of the form field. The
`field-headers` arg is a plist containing the headers for that field (generally
this is `Content-Disposition` and sometimes `Content-Type` for uploads). The
`field-meta` arg is a plist of key/value pairs found in the `Content-Disposition`
header for the field (this is where the `field-name` arg comes from, and is also
used to specify the filename of uploaded files). `body-bytes` is a byte array
containing all or a chunk of that field's data, and `body-complete-p` indicates
whether or not the `body-bytes` being sent into the callback is the last bit of
data for that field.

Generally, this callback will be a closure that is able to track the current
field it's operating on and be able to handle the case where `body-bytes` is
spread over multiple calls if `body-complete-p` is `nil`.

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

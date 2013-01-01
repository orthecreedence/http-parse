(in-package :http-parse)

(defun append-array (arr1 arr2)
  "Create an array, made up of arr1 followed by arr2."
  (let ((arr1-length (length arr1))
        (arr2-length (length arr2)))
    (let ((arr (make-array (+ arr1-length arr2-length)
                           :element-type (array-element-type arr1))))
      (replace arr arr1 :start1 0)
      (replace arr arr2 :start1 arr1-length)
      arr)))

(defun find-non-whitespace-pos (seq)
  "Find the position of the first non-whitespace character in a sequence."
  (loop for i from 0
        for byte across seq do
    (unless (or (= byte 9)
                (= byte 10)
                (= byte 13)
                (= byte 32))
      (return-from find-non-whitespace-pos i))))


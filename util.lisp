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

(defun find-non-whitespace-pos (seq &key (start 0))
  "Find the position of the first non-whitespace character in a sequence."
  (dotimes (i (- (length seq) start))
    (let* ((i (+ i start))
           (byte (aref seq i)))
      (unless (or (eq byte 9) (eq byte #\tab)
                  (eq byte 13) (eq byte #\return)
                  (eq byte 10) (eq byte #\newline)
                  (eq byte 32) (eq byte #\space))
        (return-from find-non-whitespace-pos i)))))


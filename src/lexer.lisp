(in-package :claculator)

;;;; TODO: Mind "3+-2"

(defun tokenize-expression (stream operators)
  "Takes in a stream and tokenizes it into a list of symbols and numbers which could be passed to a parser to parse.
For example: \"2+3*20% /5\" -> '(2 + 3 * 0.2 / 5)"
  (let ((tokens '()))
    (loop :for char := (peek-char t stream nil :eof) ;; >=
          :until (eql char :eof)
          :do (format t "EXPR: ~a, ~a~%" char tokens)
              (if (digit-char-p char)
                (push (tokenize-number stream) tokens)
                (push (tokenize-operator stream operators) tokens))
          :finally (return (reverse tokens)))))

(defun tokenize-operator (stream operators)
  (let ((operator-string (make-array 0 :element-type 'character
                                       :fill-pointer 0
                                       :adjustable t))
        (operators operators))
    (loop :for char := (peek-char t stream nil :eof)
          :until (eql char :eof)
          :do (read-char stream nil :eof)
              ;; If char is a digit, it means we have reached the end of the operator, return it
              (when (digit-char-p char)
                (return (second (find-operator operator-string operators))))
              ;; Narrow down search of operators with anything that starts with the characters
              (vector-push-extend char operator-string)
              (setf operators
                    (remove-if-not (lambda (arg) (str:starts-with? operator-string (car arg)))
                                   operators))
              (format t "OPER: ~a, ~a, ~0S~%" char operator-string operators) 
              ;; When there is only one operator starting with the first characters,
              ;; Loop through the rest of the characters and read-char to make stream up-to-date
              ;; Then return the operator
              (when (= 1 (length operators))
                (loop :for i :from 1 :to (- (length (first (car operators))) (length operator-string))
                  :do (read-char stream nil :eof)
                  :finally (return-from tokenize-operator (second (car operators)))))
              ;; Could not find any operator that matches (starting with the first characters)
              (when (= 0 (length operators))
                (error "Could not completely tokenize the stream ~S ; Unexpected char ~S" stream char)))))

;;;     ttttp      cc
;;; "455+487*2"   "2+3*4+5"
(defun tokenize-number (stream)
  "Tokenizes a number taken from a stream. and returns it"
  (let ((digits (make-array 0 :element-type 'character
                              :fill-pointer 0
                              :adjustable t)))
    (loop :for char := (peek-char t stream nil :eof)
          :until (or (eql char :eof) (not (digit-char-p char))) ; ORDER MATTERS!
          :do (read-char stream nil :eof)
              (vector-push-extend char digits))
    ;; If digits is not empty then parse and return
    (unless (= 0 (length digits)) 
      (parse-integer digits))))

(in-package :claculator)

;;;; TODO: Mind "3+-2"

(defun tokenize-expr (stream operators)
  "Takes in a stream and tokenizes it into a list of symbols and numbers which could be passed to a parser to parse.
For example: \"2+3*20% /5\" -> '(2 + 3 * 0.2 / 5)"
  (let ((tokens '()))
    (loop :for char := (peek-char t stream nil :eof)
          :until (eql char :eof)
          :do (format t "~a, ~a~%" char tokens)
              (if (digit-char-p char)
                (push (tokenize-number stream) tokens)
                ;; (when (null (tokenize-operator stream operators))
                ;;   (format t "[ERROR] Could not completely tokenize string. ")
                ;;   (format t "Unexpected char ~s" char)
                ;;   (return-from tokenize-expr tokens))

                ;; (let ((token (tokenize-operator stream operators)))
                ;;   (if token
                ;;     (progn
                ;;       (push token tokens)
                ;;       (read-char stream t :eof))
                ;;     ))
                )
          :finally (return (reverse tokens)))))

(defun tokenize-operator (stream operators)
  ;; (let ((operators-by-length (sort operators (lambda (a b)
  ;;                                              (> (length a) (length b)))))
  ;;       (operator-string (make-array 1 :element-type 'character
  ;;                                      :fill-pointer 1
  ;;                                      :adjustable t)))
  ;;   (loop :for char := (peek-char t stream nil :eof)
  ;;         :until (eql :eof char)
  ;;         :initially (vector-push-extend char operator-string)
  ;;         :do (when (find-operator operator-string operators-by-length)
  ;;               (return-from tokenize-operator operator-string))
  ;;             (when (find-operator operator-string operators-by-length :test #'str:starts-with?)
  ;;               (vector-push-extend char operator-string))))
  )

;;;     ttttp      cc
;;; "455+487*2"   "2+3*4+5"
(defun tokenize-number (stream)
  "Tokenizes a number taken from a stream. and returns it"
  (let ((digits (make-array 1 :element-type 'character
                              :fill-pointer 1
                              :adjustable t)))
    (loop :for char := (peek-char t stream nil :eof)
          :while (and (not (eql :eof char)) (digit-char-p char))
          :do (vector-push-extend char digits)
              (read-char stream nil :eof))
    (parse-integer (reverse digits))))

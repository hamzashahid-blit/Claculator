(in-package :claculator)

(defun token-operator-p (str operators)
  "Tries to find STR in OPERATORS"
  (gethash str operators))

(defun tokenize-expr (stream operators)
  "Takes in a stream and tokenizes it into a list of symbols and numbers which could be passed to a parser to parse.
For example: \"2+3*20% /5\" -> '(2 + 3 * 0.2 / 5)"
  (let ((tokens '()))
    (loop :for char := (peek-char t stream nil :eof)
          :until (eql char :eof)
          :do (format t "~a, ~a~%" char tokens)
              (if (digit-char-p char)
                (push (tokenize-number stream) tokens)
                (let ((token-op (token-operator-p (string char) operators)))
                  (if token-op
                    (progn
                      (push token-op tokens)
                      (read-char stream t :eof))
                    (progn
                      (format t "[ERROR] Could not completely tokenize string. ")
                      (format t "Unexpected char ~s" char)
                      (return tokens)))))
          :finally (return (reverse tokens)))))

;;;     ttttp      cc
;;; "455+487*2"   "2+3*4+5"
(defun tokenize-number (stream)
  "Tokenizes a number taken from a stream. and returns it"
  (let ((digits '()))
    (loop :for char := (peek-char t stream nil :eof)
          :while (and (not (eql :eof char)) (digit-char-p char))
          :do (push char digits)
              (read-char stream nil :eof)
          :finally (return (parse-integer (coerce (reverse digits) 'string))))))

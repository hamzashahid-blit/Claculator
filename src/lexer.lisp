(in-package :claculator)

;; TODO: parse 2+3*4+5 into (+ (+ 2 (* 3 4)) 5)
;; TODO: parse double digits
;; TODO: parse multi-character functions
;; TODO: parse 3x + 5/7^3 into (+ (* 3 x) (/ 5 (^ 7 3)))

(defun lexer (str-expr)
  "Takes in an algebraic string expression and parses it into a list of symbols and numbers which could be pased to a parser to parse.
For example: \"2+3*20% /5\" -> '(2 + 3 * 0.2 / 5)"
  (with-input-from-string (stream str-expr)
    (let ((expr nil))
      (loop :for c := (peek-char t stream nil :eof)
            :while (not (eql c :eof))
        :do (format t "~a, ~a~%" c expr)
            (if (digit-char-p c)
              (push (tokenize-number stream) expr)
              (let ((c (string-upcase (string c))))
                (cond
                  ((string= c "+") (push '+ expr) (read-char stream t :eof))
                  ((string= c "*") (push '* expr) (read-char stream t :eof))
                  ;; ((string= c "+") (push '+ expr) (read-char stream t :eof))
                  ;; ((string= c "*") (push '* expr) (read-char stream t :eof))
                  (t (format t (str:concat
                                 "[ERROR] Could not completely tokenize string. "
                                 "Unexpected char ~s") c)
                     (return-from lexer expr)))))
        :finally (return-from lexer (reverse expr))))))

;;     ttttp      cc
;; "455+487*2"   "2+3*4+5"
(defun tokenize-number (stream)
  "Tokenizes a number taken from a stream. and returns it"
  (let ((num nil))
    (loop :for c := (peek-char t stream nil :eof)
      :while (and (not (eql :eof c)) (str:digit? (string c)))
      :do (push c num)
          (read-char stream nil :eof) 
      :finally (return-from tokenize-number 
                            (parse-integer (coerce (reverse num) 'string))))))

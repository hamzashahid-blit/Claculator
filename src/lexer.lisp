(in-package :claculator)

;; (cond
;;   ((string= c "+") (push (gethash "+" *clac-ops*) expr))
;;   ((string= c "-") (push (gethash "-" *clac-ops*) expr)))

;; (cond
;;   ((string= c "+") (push '+ expr) (read-char stream t :eof))
;;   ((string= c "*") (push '* expr) (read-char stream t :eof))
;;   ;; ((string= c "+") (push '+ expr) (read-char stream t :eof))
;;   ;; ((string= c "*") (push '* expr) (read-char stream t :eof))
;;   (t (format t (str:concat
;;                  "[ERROR] Could not completely tokenize string. "
;;                  "Unexpected char ~s") c)
;;     (return-from lexer expr)))

;; ;; make a list of key value pairs from a hash-table so that
;; ;; you could loop with LOOP :for (a b) :in kv-list
;; (let ((kv-list '()))
;;            (maphash (lambda (k v)
;;                       (push k kv-list)
;;                       (push v kv-list))
;;              *clac-ops*)
;;            (reverse kv-list))


;; (defun gen-lexer-ops-cond (hash-map stream str-to-compare eof-value error-expr-list)
;;   (let ((expr (gensym)))
;;     `(let ((,expr '()))
;;        (cond ,(maphash
;;                 (lambda (k v)
;;                   (print `((string= ,str-to-compare ,k)
;;                            (push ,v ,expr)
;;                            (read-char ,stream t ,eof-value))))
;;                hash-map)
;;             (t ,error-expr-list))
;;        ,expr)))

(defun gen-lexer-ops-cond (hash-map stream str-to-compare eof-value error-expr-list)
  (let ((tokens (gensym)))
    `(let ((,tokens '()))
       (cond ,(let ((kv-list (gensym)))
                `(let ((kv-list '()))
                   ,(maphash (lambda (k v)
                              (push k kv-list)
                              (push v kv-list))
                      hash-map)
                   ,@(loop :for (k v) :in kv-list
                       :collect `((string= ,str-to-compare ,k)
                                  (push ,v ,tokens)
                                  (read-char ,stream t ,eof-value)))))
             (t ,error-expr-list))
       ,tokens)))


(defmacro lexer-ops-cond (hash-map stream str-to-compare eof-value &body error-expr)
  ;; (format t "~a, ~a, ~a, ~a; ~a~%" hash-map stream str-to-compare eof-value error-expr)
  (gen-lexer-ops-cond hash-map stream str-to-compare eof-value error-expr))

(defun lexer (str-expr ops-hash-map)
  "Takes in an algebraic string expression and tokenizes it into a list of symbols and numbers which could be pased to a parser to parse.
For example: \"2+3*20% /5\" -> '(2 + 3 * 0.2 / 5)"
  (with-input-from-string (stream str-expr)
    (let ((tokens '()))
      (loop :for c := (peek-char t stream nil :eof)
            :until (eql c :eof)
            :do (format t "~a, ~a~%" c tokens)
                (if (digit-char-p c)
                  ;; Finds the entire number and returns it as an integer
                  (push (tokenize-number stream) tokens)
                  (let ((c (string-upcase (string c))))
                    (cond
                      ((string= c "+") ;(lexer-ops-cond ops-hash-map stream c :eof (format t "NOOOOO"))
                        (push '+ tokens) (read-char stream t :eof))
                      ((string= c "*") (push '* tokens) (read-char stream t :eof))
                      ((string= c "+") (push '+ tokens) (read-char stream t :eof))
                      ((string= c "*") (push '* tokens) (read-char stream t :eof))
                      (t (format t (str:concat
                                     "[ERROR] Could not completely tokenize string. "
                                     "Unexpected char ~s") c)
                        (return-from lexer tokens)))))
            :finally (return-from lexer (reverse tokens))))))

;;;     ttttp      cc
;;; "455+487*2"   "2+3*4+5"
(defun tokenize-number (stream)
  "Tokenizes a number taken from a stream. and returns it"
  (let ((digits '()))
    (loop :for c := (peek-char t stream nil :eof)
      :while (and (not (eql :eof c)) (str:digit? (string c)))
      :do (push c digits)
          (read-char stream nil :eof)
      :finally (return-from tokenize-number 
                            (parse-integer (coerce (reverse digits) 'string))))))

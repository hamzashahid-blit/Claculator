(in-package :claculator)

;;; Commented code is useless and just for reference.

;; (cond
;;   ((string= c "+") (push (gethash "+" *clac-ops*) expr))
;;   ((string= c "-") (push (gethash "-" *clac-ops*) expr)))

;; (cond
;;   ((string= c "+") (push '+ expr) (read-char stream t :eof))
;;   ((string= c "*") (push '* expr) (read-char stream t :eof))
;;   (t (format t "[ERROR] Could not completely tokenize string. ")
;;      (format t "Unexpected char ~s" c)
;;      (return-from lexer expr)))
;; 
;; ;; make a list of key value pairs from a hash-table so that
;; ;; you could loop with LOOP :for (a b) :in kv-list
;; (let ((kv-list '()))
;;   (maphash (lambda (k v)
;;              (push k kv-list)
;;              (push v kv-list))
;;     *clac-ops*)
;;   (reverse kv-list))
;; 
;; 
;; (defun gen-lexer-ops-cond (hash-map stream str-to-compare eof-value error-expr-list)
;;   (let ((expr (gensym)))
;;     `(let ((,expr '()))
;;        (cond ,(maphash
;;                 (lambda (k v)
;;                   (print `((string= ,str-to-compare ,k)
;;                            (push ,v ,expr)
;;                            (read-char ,stream t ,eof-value))))
;;                 hash-map)
;;             (t ,error-expr-list))
;;        ,expr)))

;; (let ((char-str (string-upcase (string char-str))))
;;   ;; TODO: FIX THIS:
;;   ;;       (tokenize-expr-ops-cond operator-hash-table stream char-str :eof (format t "NOOOOO"))
;;   ;;       to stop repitition as done below. This above macro should read  
;;   (cond
;;     ((string= char-str "+") (push '+ tokens) (read-char stream t :eof))
;;     ((string= char-str "*") (push '* tokens) (read-char stream t :eof))
;;     (t (format t "[ERROR] Could not completely tokenize string. ")
;;        (format t "Unexpected char ~s" char-str)
;;        (return tokens))))

(defun gen-tokenize-expr-ops-cond (hash-map stream str-to-compare eof-value error-expr-list)
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

(defmacro tokenize-expr-ops-cond (hash-map stream str-to-compare eof-value &body error-expr)
  ;; (format t "~a, ~a, ~a, ~a; ~a~%" hash-map stream str-to-compare eof-value error-expr)
  (gen-lexer-ops-cond hash-map stream str-to-compare eof-value error-expr))

(defun is-token-operator? (char operator-hash-table)
  "Takes in a character CHAR and a hash-table containing operators OPERATOR-HASH-TABLE.
It turns CHAR into a string and capitalizes it. Then tries to find it in OPERATOR-HASH-TABLE
Returns T or NIL if it was found or not as its first value and the
actual operator-function associated with it as the second value"
  (let ((char-str (string-upcase (string char))))
    (maphash (lambda (k v)
               (when (string= char-str k)
                 (return-from is-token-operator? (values t v))))
      operator-hash-table)

    (values nil nil)))

(defun tokenize-expr (str-expr operator-hash-table)
  "Takes in an algebraic string expression and tokenizes it into a list of symbols and numbers which could be pased to a parser to parse.
For example: \"2+3*20% /5\" -> '(2 + 3 * 0.2 / 5)"
  (with-input-from-string (stream str-expr)
    (let ((tokens '()))
      (loop :for char := (peek-char t stream nil :eof)
            :until (eql char :eof)
            :do (format t "~a, ~a~%" char tokens)
                (if (digit-char-p char)
                  (push (tokenize-number stream) tokens)
                  (multiple-value-bind (is-operator? token)
                    (is-token-operator? char operator-hash-table)
                    (if is-operator?
                      (progn
                        (push token tokens)
                        (read-char stream t :eof))
                      (progn
                        (format t "[ERROR] Could not completely tokenize string. ")
                        (format t "Unexpected char ~s" char)
                        (return tokens)))))
            :finally (return (reverse tokens))))))

;;;     ttttp      cc
;;; "455+487*2"   "2+3*4+5"
(defun tokenize-number (stream)
  "Tokenizes a number taken from a stream. and returns it"
  (let ((digits '()))
    (loop :for char := (peek-char t stream nil :eof)
          :while (and (not (eql :eof char)) (str:digit? (string char)))
          :do (push char digits)
              (read-char stream nil :eof)
          :finally (return (parse-integer (coerce (reverse digits) 'string))))))

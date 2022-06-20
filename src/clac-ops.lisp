(in-package :claculator)

;; TODO: Mind "3+-2"

(defmacro defhash-map (name (key value) &rest other-pairs)
  "Macro for GEN-EXPR-DEFHASH-MAP"
  (gen-expr-defhash-map name (cons (list key value) other-pairs)))

(defun gen-expr-defhash-map (name definitions)
  "Creates a global variable with DEFPARAMETER with name containing a new hash-map with all the definitions in (key value) pair lists."
  `(defparameter ,name
     ,(let ((table (gensym)))
        `(let ((,table (make-hash-table :test 'equalp)))
           (setf ,@(loop :for def :in definitions
                     :collect `(gethash ,(first def) ,table)
                     :collect `(quote ,(second def))))
           ,table))))

(defmethod percentage (arg)
  (/ arg 100))

(defhash-map *clac-ops*
  ("+" +)
  ("-" -)
  ("*" *)
  ("/" /)
  ("%" percentage)
  ("mod" mod)
  ("rem" rem))

(defmacro define-operators (hash-map &rest tokens)
  `(defhash-map ,hash-map ,@tokens))

(defun operators-count (operators)
  (hash-table-size operators))

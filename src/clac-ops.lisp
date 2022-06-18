(in-package :claculator)

(defun gen-expr-defhash-map (name definitions)
  "Creates a global variable with DEFPARAMETER with name containing a new hash-map with all the definitions in (key value) pair lists."
  `(defparameter ,name
     ,(let ((table (gensym)))
        `(let ((,table (make-hash-table :test 'equal)))
           (setf ,@(loop :for def :in definitions
                     :collect `(gethash ,(first def) ,table)
                     :collect `(quote ,(second def))))
           ,table))))

(defmacro defhash-map (name (key value) &rest other-pairs)
  "Macro for GEN-EXPR-DEFHASH-MAP"
  (gen-expr-defhash-map name (cons (list key value) other-pairs)))

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

(in-package :claculator)

(defparameter *text-style*
  (c:make-text-style :sans-serif :roman :large))


(defgeneric calculator-frame-top-level (frame &key command-parser
                                                   command-unparser
                                                   partial-command-parser
                                                   prompt))

(defmethod calculator-frame-top-level
    ((frame c:application-frame)
     &key (command-parser 'command-line-command-parser)
       (command-unparser 'command-line-command-unparser)
       (partial-command-parser
        'command-line-read-remaining-arguments-for-partial-command)
       (prompt "Command: "))
  (declare (ignore command-parser command-unparser partial-command-parser prompt))
  (clim-extensions:simple-event-loop))

(defun make-button (label operator &key width height
                                        (max-width c:+fill+) min-width
                                        (max-height c:+fill+) min-height)
  (c:make-pane 'c:push-button
    :label label
    :activate-callback operator
    :text-style *text-style*
    :width width :height height
    :max-width  max-width  :min-width  min-width
    :max-height max-height :min-height min-height))


(c:define-application-frame claculator ()
  ((state :initform (list 0)
          :accessor state)
   (screen-field :initform nil
                 :accessor screen-field))
  (:pointer-documentation nil)
  (:reinitialize-frames t)
  (:menu-bar nil)
  (:panes
    (screen :text-editor
            :value "0"
            :text-style *text-style*) 
    (one   (make-button "1" (queue-number 1)))
    (two   (make-button "2" (queue-number 2)))
    (three (make-button "3" (queue-number 3)))
    (four  (make-button "4" (queue-number 4)))
    (five  (make-button "5" (queue-number 5)))
    (six   (make-button "6" (queue-number 6)))
    (seven (make-button "7" (queue-number 7)))
    (eight (make-button "8" (queue-number 8)))
    (nine  (make-button "9" (queue-number 9)))
    (zero  (make-button "0" (queue-number 0)))
    (add   (make-button "+" (queue-operator '+)))
    (sub   (make-button "-" (queue-operator '-)))
    (mult  (make-button "*" (queue-operator '*)))
    (div   (make-button "/" (queue-operator '/)))
    (eval  (make-button "="  #'calculate))
    (ac    (make-button "AC" #'init-ac))
    (ce    (make-button "CE" #'init-ce))
    (int :interactor
         :width 300
         :height 100))
  (:layouts
    (default (with-slots (screen-field) c:*application-frame*
               (c:vertically ()         ;(:width 150 :max-width 500)
                 (setf screen-field screen)
                 ;; screen
                 (c:horizontally ()     ;(:height 50)
                   ac ce)
                 (c:tabling (:grid t)
                   (list one two three add)
                   (list four five six sub)
                   (list seven eight nine mult)
                   (list zero div eval int)
                   )))))
  ;; (:top-level (calculator-frame-top-level . nil))
  )

(defun show ()
  (setf (c:gadget-value (slot-value c:*application-frame* 'screen-field))
        (str:join #\Space (reverse (mapcar #'write-to-string (state c:*application-frame*))))))

(defun operator? (operator)
  (not (numberp operator)))

(defun queue-number (number)
  (lambda (gadget)
    (declare (ignore gadget))
    (with-slots (state) c:*application-frame*
      (let ((last-elem (first state)))
        ;; Check if the last number on stack is an operator
        ;; (format (c:find-pane-named c:*application-frame* 'int) "~a~%" last-elem)
        (if (operator? last-elem) ;(evenp (length state))
          (when (and (< (length state) 3))
            (push number state))
          (setf (first state) (+ (* 10 last-elem) number))))

      (show))))

(defun queue-operator (operator)
  (lambda (gadget)
    (declare (ignore gadget))
    (with-slots (state) c:*application-frame*
      (when (and (< (length state) 3)
                 (oddp (length state)))
        (push operator state)
        (show)))))

(defun calculate (gadget)
  (declare (ignore gadget))
  (with-slots (state) c:*application-frame*
    (when (= 3 (length state))
      (setf state (list (funcall (second state) (first state) (third state))))
      (show))))

(defun init-ac (gadget)
  (declare (ignore gadget))
  (setf (state c:*application-frame*) '(0))
  (show))

(defun init-ce (gadget)
  (declare (ignore gadget))
  (pop (state c:*application-frame*))
  (show))

(define-claculator-command (com-quit :name t) ()
  (init-ac t)
  (c:frame-exit c:*application-frame*))

(defun run ()
  ;; bt:make-thread
  ;; lambda ()
  (c:run-frame-top-level (c:make-application-frame 'claculator)))












;; (str:join #\Space (mapcar #'write-to-string '(0 + 1)))
;; (defun display ()
;;   (with-slots (state) c:*application-frame*
;;     (setf (screen-field c:*application-frame*)
;;       (str:join #\Space (mapcar #'write-to-string state)))
;;     (when (= 3 (length state))
;;       (apply (second state) (first state) (third state)))))

;; (define-claculator-command (queue-number :name t) ((operand 'integer))
;;   (push operand (state c:*application-frame*)))

;; (define-claculator-command (queue-number :name t) ((operator 'function))
;;   (push operator (state c:*application-frame*)))

;; ;; (define-claculator-command (calculate :name t) ()
;; ;;   (with-slots (state) c:*application-frame*
;; ;;     (when (= 3 (length state))
;; ;;       (apply (second state) (first state) (third state)))))

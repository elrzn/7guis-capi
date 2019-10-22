;;;; 4-timer.lisp

(in-package #:7guis-capi)

(capi:define-interface progress ()
  ((elapsed-seconds :initform 0 :type string :accessor progress-elapsed-seconds))
  (:panes
   (progress-bar-label
    capi:display-pane
    :background :transparent
    :text "Elapsed time")
   (progress-bar
    capi:progress-bar
    :start 0
    :end 30)
   (elapsed-seconds-display
    capi:display-pane
    :background :transparent
    :text "0s")
   (slider
    capi:slider
    :start 1
    :end 60)
   (reset
    capi:push-button
    :text "Reset"))
  (:layouts
   (main capi:column-layout '(progress-bar-layout elapsed-seconds-display slider reset))
   (progress-bar-layout capi:row-layout '(progress-bar-label progress-bar)))
  (:default-initargs
   :title "Timer"
   :best-width 480))

(defun tick (interface)
  (capi:execute-with-interface
   interface
   #'(lambda ()
       (with-slots (elapsed-seconds progress-bar elapsed-seconds-display slider) interface
         ;; Update elapsed seconds message.
         (setf (capi:display-pane-text elapsed-seconds-display)
               (format nil "~ds" (incf elapsed-seconds)))))))

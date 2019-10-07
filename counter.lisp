;;;; counter.lisp

(in-package #:7guis-capi)

(defun increase-counter (data interface)
  (declare (ignore data))
  (setf (capi:text-input-pane-text (counter-text interface))
        (write-to-string (incf (counter-value interface)))))

(capi:define-interface counter ()
  ((value
    :initarg :value
    :initform 0
    :type fixnum
    :accessor counter-value
    :documentation "The counter value that will be increased."))
  (:panes
   (counter-text capi:text-input-pane :text (write-to-string value) :accessor counter-text)
   (increase-button capi:push-button :text "Count" :callback #'increase-counter))
  (:layouts (main-layout capi:row-layout '(counter-text increase-button)))
  (:default-initargs :title "Counter")
  (:documentation
   "Frame containing a label or read-only textfield T and a button B.
Initially, the value in T is “0” and each click of B increases the
value in T by one."))

;;;; 7guis-capi.lisp

(in-package #:7guis-capi)

(capi:define-interface 7guis ()
  ()
  (:panes
   (counter capi:push-button
            :text "Counter"
            :callback (lambda (data interface)
                        (declare (ignore data interface))
                        (capi:display
                         (make-instance 'counter)))))
  (:layouts (main capi:column-layout '(counter)))
  (:default-initargs :title "7GUIs"))

(defun main ()
  (capi:display (make-instance '7guis)))

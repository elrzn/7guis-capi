;;;; temperature-converter.lisp

(in-package #:7guis-capi)

(defun celsius->fahrenheit (c)
  "Convert from Celsius to Fahrenheit.

F = C * (9/5) + 32"
  (+ (* c (/ 9 5)) 32))

(defun fahrenheit->celsius (f)
  "Convert from Fahrenheit to Celsius.

C = (F - 32) * (5/9)"
  (* (- f 32) (/ 5 9)))

(capi:define-interface temperature-converter ()
  ()
  (:panes
   (celsius capi:text-input-pane
            :title "Celsius"
            :change-callback #'(lambda (new-text pane interface text-length)
                                 (declare (ignore pane interface text-length))
                                 (when (> (length new-text) 0)
                                   (let ((c (nth-value 0 (parse-integer new-text))))
                                     (when c
                                       (setf (capi:text-input-pane-text fahrenheit)
                                             (write-to-string (celsius->fahrenheit c))))))))
   (fahrenheit capi:text-input-pane
               :title "Fahrenheit"
               :change-callback #'(lambda (new-text pane interface text-length)
                                    (declare (ignore pane interface text-length))
                                    (when (> (length new-text) 0)
                                      (let ((f (nth-value 0 (parse-integer new-text))))
                                        (when f
                                          (setf (capi:text-input-pane-text celsius)
                                                (write-to-string (fahrenheit->celsius f)))))))))
  (:layouts
   (main capi:row-layout '(celsius fahrenheit)))
  (:default-initargs :title "TempConv"))

(defun foo ()
  (capi:display (make-instance 'temperature-converter)))
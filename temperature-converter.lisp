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

(defclass temperature-pane (capi:text-input-pane) ())

(defclass temperature-pane-celsius (temperature-pane) ())

(defclass temperature-pane-fahrenheit (temperature-pane) ())

(defun temperature-pane-update (new-text pane interface text-length)
  (when (> text-length 0)
    (let ((temperature (nth-value 0 (parse-integer new-text))))
      (when temperature
        (temperature-pane-update-other pane interface temperature)))))

(defgeneric temperature-pane-update-other (temperature pane interface))

(defmethod temperature-pane-update-other ((pane temperature-pane-celsius) interface temperature)
  (let ((other-pane (temperature-converter-pane-fahrenheit interface)))
    (setf (capi:text-input-pane-text other-pane)
          (write-to-string (celsius->fahrenheit temperature)))))

(defmethod temperature-pane-update-other ((pane temperature-pane-fahrenheit) interface temperature)
  (let ((other-pane (temperature-converter-pane-celsius interface)))
    (setf (capi:text-input-pane-text other-pane)
          (write-to-string (fahrenheit->celsius temperature)))))

(capi:define-interface temperature-converter ()
  ()
  (:panes
   (celsius temperature-pane-celsius
            :title "Celsius"
            :accessor temperature-converter-pane-celsius
            :change-callback #'temperature-pane-update)
   (fahrenheit temperature-pane-fahrenheit
               :title "Fahrenheit"
               :accessor temperature-converter-pane-fahrenheit
               :change-callback #'temperature-pane-update))
  (:layouts
   (main capi:row-layout '(celsius fahrenheit)))
  (:default-initargs :title "TempConv"))

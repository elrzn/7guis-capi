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

(defmethod temperature-pane-temperature ((pane temperature-pane))
  (nth-value 0 (parse-integer (capi:text-input-pane-text pane) :junk-allowed t)))

(defmethod temperature-pane-temperature-update ((pane temperature-pane) new-temperature)
  (let ((old-temperature (temperature-pane-temperature pane)))
    (when (or (not old-temperature)
              (not (= old-temperature new-temperature)))
      (setf (capi:text-input-pane-text pane) (write-to-string new-temperature)))))

(defun temperature-pane-callback (new-text pane interface text-length)
  (declare (ignore text-length))
  (let ((temperature (nth-value 0 (parse-integer new-text :junk-allowed t))))
    (when temperature
      (temperature-pane-update-other pane interface temperature))))

(defgeneric temperature-pane-update-other (pane interface temperature))

(defmethod temperature-pane-update-other ((pane temperature-pane-celsius) interface temperature)
  (let ((other-pane (temperature-converter-pane-fahrenheit interface))
        (new-temperature (celsius->fahrenheit temperature)))
    (temperature-pane-temperature-update other-pane new-temperature)))

(defmethod temperature-pane-update-other ((pane temperature-pane-fahrenheit) interface temperature)
  (let ((other-pane (temperature-converter-pane-celsius interface))
        (new-temperature (fahrenheit->celsius temperature)))
    (temperature-pane-temperature-update other-pane new-temperature)))

(capi:define-interface temperature-converter ()
  ()
  (:panes
   (celsius temperature-pane-celsius
            :title "Celsius"
            :accessor temperature-converter-pane-celsius
            :change-callback #'temperature-pane-callback)
   (fahrenheit temperature-pane-fahrenheit
               :title "Fahrenheit"
               :accessor temperature-converter-pane-fahrenheit
               :change-callback #'temperature-pane-callback))
  (:layouts
   (main capi:row-layout '(celsius fahrenheit)))
  (:default-initargs :title "TempConv"))

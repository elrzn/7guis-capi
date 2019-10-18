;;;; 3-flight-booker.lisp

(in-package #:7guis-capi)

(declaim (ftype (function (string) fixnum) string->date))

(defun string->date (given-string)
  (declare (string given-string))
  (when (= 8 (length given-string))
    (let ((tokens (uiop/utility:split-string given-string :separator ".")))
      (when (= 3 (length tokens))
        (destructuring-bind (day month year)
            (mapcar #'parse-integer tokens)
          (encode-universal-time 0 0 0 day month (+ 2000 year)))))))

(declaim (ftype (function (fixnum) string) date->string))

(defun date->string (given-date)
  (declare (fixnum given-date))
  (multiple-value-bind
      (second minute hour day month year day-of-week dst-p tz)
      (decode-universal-time given-date)
    (declare (ignore second minute hour day-of-week dst-p tz))
    (format nil "~d.~d.~d" day month (abs (- 2000 year)))))

(defun change-callback (new-text pane interface text-length)
  (declare (ignore interface text-length))
  (setf (capi:simple-pane-background pane) (if (string->date new-text) nil :red)))

(capi:define-interface flight-booker ()
  ()
  (:panes
   (ticket-type capi:option-pane :items '("one-way flight" "return flight"))
   (start-date capi:text-input-pane
               :text (date->string (get-universal-time))
               :change-callback #'change-callback)
   (arrival-date capi:text-input-pane
                 :text ""
                 :change-callback #'change-callback
                 :enabled nil)
   (book capi:push-button :text "Book"))
  (:layouts
   (main capi:column-layout '(ticket-type start-date arrival-date book)))
  (:default-initargs :title "Flight Booker"))

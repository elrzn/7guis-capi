;;;; 3-flight-booker.lisp

(in-package #:7guis-capi)

(declaim (ftype (function (string) fixnum) string->date))

(defparameter +flight-ticket-options+ '(one-way-flight return-flight))

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

(defclass flight-booker-date (capi:text-input-pane) ())

(defmethod get-date ((pane flight-booker-date))
  (string->date (capi:text-input-pane-text pane)))

(defun flight-booker-date-change-callback (new-text pane interface text-length)
  (declare (ignore new-text interface text-length))
  (setf (capi:simple-pane-background pane)
        (if (get-date pane) :white :red)))

(defun flight-booker-ticket-selection-callback (ticket-type interface)
  (let ((pane (flight-booker-arrival-date interface)))
    (setf (capi:simple-pane-enabled pane) (not (eq ticket-type 'one-way-flight)))))

(capi:define-interface flight-booker ()
  ()
  (:panes
   (ticket-type capi:option-pane
                :items +flight-ticket-options+
                :print-function #'string-downcase
                :selection-callback #'flight-booker-ticket-selection-callback)
   (start-date flight-booker-date
               :text (date->string (get-universal-time))
               :change-callback #'flight-booker-date-change-callback)
   (arrival-date flight-booker-date
                 :text ""
                 :change-callback #'flight-booker-date-change-callback
                 :accessor flight-booker-arrival-date
                 :enabled nil)
   (book capi:push-button :text "Book"))
  (:layouts
   (main capi:column-layout '(ticket-type start-date arrival-date book)))
  (:default-initargs :title "Flight Booker"))

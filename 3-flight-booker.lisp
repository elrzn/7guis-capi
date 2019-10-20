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

(defparameter +flight-ticket-options+
  '((:single . "one way flight")
    (:return . "return flight")))

(defclass flight-booker-date (capi:text-input-pane) ())

(defmethod initialize-instance :after ((self flight-booker-date) &key)
  (setf (capi:text-input-pane-text self) (date->string (get-universal-time))))

(defmethod get-date ((pane flight-booker-date))
  (string->date (capi:text-input-pane-text pane)))

(defun flight-booker-date-change-callback (new-text pane interface text-length)
  (declare (ignore new-text text-length))
  ;; Set a red background for the flight booker date pane if its
  ;; content evaluates to an invalid date.
  (setf (capi:simple-pane-background pane)
        (if (get-date pane) :white :red))
  ;; Check if all dates are okay, and if that's the case, enable the
  ;; booking button.
  (setf (capi:simple-pane-enabled (flight-booker-book interface))
        (dates-okay-p interface)))

(defun flight-booker-ticket-selection-callback (ticket-type interface)
  (let ((pane (flight-booker-arrival-date interface)))
    ;; Disable arrival date input field if the user selected a single
    ;; ticket.
    (setf (capi:simple-pane-enabled pane)
          (not (eq ticket-type (cdr (assoc :single +flight-ticket-options+)))))))

(capi:define-interface flight-booker ()
  ()
  (:panes
   (ticket-type capi:option-pane
                :items (mapcar #'cdr +flight-ticket-options+)
                :selection-callback #'flight-booker-ticket-selection-callback)
   (start-date flight-booker-date
               :change-callback #'flight-booker-date-change-callback
               :reader flight-booker-start-date)
   (arrival-date flight-booker-date
                 :change-callback #'flight-booker-date-change-callback
                 :accessor flight-booker-arrival-date
                 :enabled nil)
   (book capi:push-button
         :text "Book"
         :accessor flight-booker-book))
  (:layouts
   (main capi:column-layout '(ticket-type start-date arrival-date book)))
  (:default-initargs :title "Flight Booker"))

(defmethod dates-okay-p ((interface flight-booker))
  (let ((start-date (flight-booker-start-date interface))
        (arrival-date (flight-booker-arrival-date interface)))
    (when-let ((start-date-date (get-date start-date))
               (arrival-date-date (get-date arrival-date)))
      (>= arrival-date-date start-date-date))))


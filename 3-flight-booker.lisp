;;;; 3-flight-booker.lisp

(in-package #:7guis-capi)

(declaim (ftype (function (string) fixnum) string->date))

(defun string->date (given-string)
  "From a dd.mm.yy string, return a valid date."
  (declare (string given-string))
  (when (= 8 (length given-string))
    (let ((tokens (uiop/utility:split-string given-string :separator ".")))
      (when (= 3 (length tokens))
        (destructuring-bind (day month year)
            (mapcar #'parse-integer tokens)
          (encode-universal-time 0 0 0 day month (+ 2000 year)))))))

(declaim (ftype (function (fixnum) string) date->string))

(defun date->string (given-date)
  "From a date, return a string representation with format dd.mm.yy."
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
  (check-booking interface))

(defun flight-booker-ticket-selection-callback (ticket-type interface)
  (declare (ignore ticket-type)) ;; We have a method for this.
  (let ((pane (flight-booker-arrival-date interface)))
    ;; Disable arrival date input field if the user selected a single
    ;; ticket.
    (setf (capi:simple-pane-enabled pane)
          (return-ticket-p interface)))
  (check-booking interface))

(defun flight-booker-book-callback (data interface)
  (declare (ignore data))
  (capi:display-message "Reserved a ~a on ~a."
                        (capi:choice-selected-item (flight-booker-ticket-type interface))
                        (if (single-ticket-p interface)
                            (date->string (get-date (flight-booker-start-date interface)))
                          (format nil "~a and ~a"
                                  (date->string (get-date (flight-booker-start-date interface)))
                                  (date->string (get-date (flight-booker-arrival-date interface)))))))

(capi:define-interface flight-booker ()
  ()
  (:panes
   (ticket-type capi:option-pane
                :items (mapcar #'cdr +flight-ticket-options+)
                :selection-callback #'flight-booker-ticket-selection-callback
                :reader flight-booker-ticket-type)
   (start-date flight-booker-date
               :change-callback #'flight-booker-date-change-callback
               :reader flight-booker-start-date)
   (arrival-date flight-booker-date
                 :change-callback #'flight-booker-date-change-callback
                 :accessor flight-booker-arrival-date
                 :enabled nil)
   (book capi:push-button
         :text "Book"
         :callback #'flight-booker-book-callback
         :accessor flight-booker-book))
  (:layouts
   (main capi:column-layout '(ticket-type start-date arrival-date book)))
  (:default-initargs :title "Flight Booker"))

(defmethod check-booking ((interface flight-booker))
  "Check if all dates are okay, and if that's the case, enable the
booking button."
  (setf (capi:simple-pane-enabled (flight-booker-book interface))
        (dates-okay-p interface)))

(defmethod single-ticket-p ((interface flight-booker))
  "Return true if the user selected a single flight ticket."
  (let ((ticket-type (flight-booker-ticket-type interface)))
    (eq (capi:choice-selected-item ticket-type)
        (cdr (assoc :single +flight-ticket-options+)))))

(defmethod return-ticket-p ((interface flight-booker))
  "Return true if the user selected a return flight ticket."
  (not (single-ticket-p interface)))

(defmethod dates-okay-p ((interface flight-booker))
  "Check whether dates are correct and in the case of a return flight
ticket, if the arrival is set in the future."
  (when-let ((start-date (get-date (flight-booker-start-date interface))))
    (if (single-ticket-p interface)
        t
      (when-let ((arrival-date (get-date (flight-booker-arrival-date interface))))
        (>= arrival-date start-date)))))

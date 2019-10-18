;;;; 3-flight-booker.lisp

(in-package #:7guis-capi)

(capi:define-interface flight-booker ()
  ()
  (:panes
   (ticket-type capi:option-pane :items '("one-way flight" "return flight"))
   (start-date capi:text-input-pane :text "")
   (arrival-date capi:text-input-pane :text "" :enabled nil)
   (book capi:push-button :text "Book"))
  (:layouts
   (main capi:column-layout '(ticket-type start-date arrival-date book)))
  (:default-initargs :title "Flight Booker"))

;;;; 4-timer.lisp

(in-package #:7guis-capi)

(capi:define-interface progress ()
  ()
  (:panes
   (progress-bar-label
    capi:display-pane
    :background :transparent
    :text "Elapsed time")
   (progress-bar
    capi:progress-bar
    :start 0
    :end 30)
   (elapsed-seconds
    capi:display-pane
    :background :transparent
    :text "0s")
   (slider
    capi:slider
    :start 1
    :end 60)
   (reset
    capi:push-button
    :text "Reset"))
  (:layouts
   (main capi:column-layout '(progress-bar-layout elapsed-seconds slider reset))
   (progress-bar-layout capi:row-layout '(progress-bar-label progress-bar)))
  (:default-initargs
   :title "Timer"
   :best-width 480))

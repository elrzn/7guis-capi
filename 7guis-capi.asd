;;;; 7guis-capi.asd

(asdf:defsystem #:7guis-capi
  :description "7GUIs challenge implemented in LispWorks CAPI"
  :author "Eric Lorenzana"
  :license "The Unlicense"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "counter")
               (:file "7guis-capi")))

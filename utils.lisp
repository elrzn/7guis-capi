;;;; utils.lisp

(in-package #:7guis-capi)

(defmacro if-let (bindings then else)
  (let ((symbols (mapcar #'first bindings)))
    `(let ,bindings
       (if (and ,@symbols)
           ,then
         ,else))))

(defmacro when-let (bindings then)
  `(if-let ,bindings ,then nil))

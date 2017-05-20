(defpackage #:constraint-solver-system
  (:use #:cl #:asdf))

(in-package :asdf)

(defsystem constraint-solver
  :components ((:file "package")
               (:file "constraint-solver"
                      :depends-on ("data-types" "package" "csp-reader"))
               (:file "csp-reader"
                      :depends-on ("data-types" "package"))
               (:file "helper-macros"
                      :depends-on ("package"))
               (:file "run-tests"
                      :depends-on ("constraint-solver" "package"))
               (:file "data-types"
                      :depends-on ("helper-macros" "package")))
  :depends-on (:cl-ppcre))

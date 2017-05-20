(in-package #:constraint-solver)
(defclass csp ()
  ((vars :initarg :vars :accessor vars)
   (past-vars :initform () :accessor past-vars)
   (constraints :initarg :constraints :accessor constraints)))

(defclass-accessor-init csp-constraint ()
  (vars allowed-tuples))

(defclass csp-variable ()
  ((domain :initarg :domain :accessor domain)
   (min-value :reader min-value)
   (value-index :reader value-index)
   (prunes :initform () :accessor prunes)
   (index :initarg :index :reader index)
   (degree :initform 0 :accessor degree)
   (value :initform nil :reader value)))

(defmethod initialize-instance :after ((var csp-variable) &key)
  (setf (slot-value var 'min-value) (first (domain var))))

(defmethod (setf value) (val (var csp-variable))
  (setf (slot-value var 'value) val)
  (when val
    (setf (slot-value var 'value-index) (- val (min-value var)))))

(defclass statistics ()
  ((n-solutions :initform 0 :accessor n-solutions)
   (n-nodes :initform 0 :accessor n-nodes)
   (time-taken :initform 0 :accessor time-taken)))

(defmethod print-statistics ((statistics statistics))
  (format t "~a solutions, ~a nodes visited, ~a seconds taken~%"
          (n-solutions statistics) (n-nodes statistics)
          (float (/ (time-taken statistics) 1000))))

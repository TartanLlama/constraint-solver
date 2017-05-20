(in-package #:constraint-solver)
(defun symbol->keyword (sym)
  (intern (symbol-name sym) :keyword))

(defmacro defclass-accessor-init (name superclass slots)
  `(defclass ,name ,superclass
     ,(loop for slot in slots collecting
           `(,slot :initarg ,(symbol->keyword slot)
                   :accessor ,slot))))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

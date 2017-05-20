(in-package #:constraint-solver)
(defmacro shift-forward (list)
  `(setq ,list (rest ,list)))

(defun collect-constraints (constraint-data possible-vars)
  "Collects constraints from the input data and indexes all needed structures"
  (let ((constraints
         (make-array (list (length possible-vars) (length possible-vars)))))

    (loop while constraint-data do
         (let* ((vars (cdar constraint-data))
                (concrete-vars
                 (mapcar (lambda (var) (nth var possible-vars)) vars))
                (domain-sizes
                 (mapcar (lambda (var) (length (domain var)))
                         concrete-vars)))

           (shift-forward constraint-data)

           (let ((allowed-tuples (make-array domain-sizes :initial-element nil)))
             ;; Loop until this constraint is finished
             (loop while (not (or (eq (caar constraint-data) 'c)
                                  (eq constraint-data nil)))
                do
                  ;; Increase the degree of each variable
                  (mapcar (lambda (var) (incf (degree var))) concrete-vars)

                  ;; Set the element in the allowed-tuples array
                  ;; corresponding to these variables
                  (setf
                    (aref allowed-tuples
                          (- (caar constraint-data)
                             (min-value (first concrete-vars)))
                          (- (cadar constraint-data)
                             (min-value (second concrete-vars))))
                    t)
                  (shift-forward constraint-data))

             ;; Index this constraint in the constraints table
             (setf (aref constraints (second vars) (first vars))
                   (setf (aref constraints (first vars) (second vars))
                         (make-instance 'csp-constraint :vars vars
                                        :allowed-tuples
                                        allowed-tuples))))))
    constraints))

(defun range (start end)
  (loop for i from start to end collect i))

(defun parse-csp (csp)
  "Parses a CLOS csp object from an input description"
  (let* ((n-vars (caar csp))
         (min-value most-positive-fixnum)
         (max-value 0)
         (vars
          (loop for i from 1 to n-vars collecting
               (make-instance 'csp-variable
                              :domain (apply #'range (nth i csp))
                              :index (1- i))
             do (setf max-value (max max-value (second (nth i csp))))
               (setf min-value (min min-value (first (nth i csp)))))))
    (make-instance 'csp
                   :vars
                   vars
                   :constraints
                   (collect-constraints (nthcdr (1+ n-vars) csp) vars))))

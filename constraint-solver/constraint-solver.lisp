(in-package #:constraint-solver)
(defparameter *print-solutions-p* t)

;;; applicable-constraint and constraint-holds are implemented as macros
;;; because after profiling the code, I noticed that a huge amount of time
;;; was spent in function call overhead to these two functions.
;;; Changing them to macros reduced execution time of MAC3 to 1/8 of its original
(defmacro applicable-constraint (csp var1 var2)
  "Returns the constraint which involves both given variables"
  `(aref (constraints ,csp) (index ,var1) (index ,var2)))

(defmacro constraint-holds (constraint val1 val2)
  "Check if the given constraint holds for the given values

Returns t if it holds, nil otherwise"
  `(aref (allowed-tuples ,constraint) ,val1 ,val2))



(defmethod test-constraint ((csp csp) var1 var2)
  "Tests the constraint involving two given variables

Returns t if it holds, nil otherwise"
  (constraint-holds
   (applicable-constraint csp var1 var2)
   (value-index var1) (value-index var2)))

(defmethod print-assignments ((csp csp))
  "Prints out the assignments to variables in a csp"
  (let ((ordered-vars (sort (concatenate 'list (past-vars csp) (vars csp))
                            (lambda (a b) (< (index a) (index b))))))
    (loop for i from 0 below (length ordered-vars) do
         (format t "Var ~a: ~a~%" i (value (nth i ordered-vars))))))

(defmethod found-solution ((csp csp))
  "A solution was found, so print some useful statistics"
  (when *print-solutions-p*
    (format t "Found solution~%")
    (print-assignments csp)
    (format t "~%")))

(defmethod revise ((csp csp) var1 var2)
  "Revises the arc from the first var to the second

Returns t if var1 has a non-empty domain after the revision, nil otherwise"
  (let ((constraint (applicable-constraint csp var1 var2)))
    (loop for d1 in (domain var1) do

         (let ((has-support nil))
           (loop for d2 in (if (value var2) (list (value var2)) (domain var2))
              until has-support do
                (setf has-support (constraint-holds constraint
                                                    (- d1 (min-value var1))
                                                    (- d2 (min-value var2)))))

           (when (not has-support)
             (push d1 (first (prunes var1))) ;so the prune can be undone
             (setf (domain var1) (remove d1 (domain var1))))))) ;do the prune
  (> (length (domain var1)) 0))

(defmethod ac3-revise ((csp csp) var1 var2)
  "Revises the arc from the first var to the second

Returns t if the domain was changed, nil otherwise"
  (let ((old-domain-size (length (domain var1))))
    (revise csp var1 var2)
    (/= old-domain-size (length (domain var1)))))

(defmethod undo-pruning ((var csp-variable))
  "Undoes one level of pruning on a variable"
  (setf (domain var)
        (sort ;so that assignments are done in order
         (nconc (domain var) (pop (prunes var)))
         #'<)))

(defmethod revise-future-arcs ((csp csp) vars)
  "Revises all arcs from future variables to the one at the given index

Returns whether arc consistency was achieved"
  (dolist (var (rest vars)) (push nil (prunes var)))
  (loop with consistent = t
     for future-var in (rest vars)
     while consistent do
       (setf consistent
             (revise csp future-var (first vars)))
     finally (return consistent)))

(defun print-vars (vars)
  "Dumps information about variables

Used for debugging"
  (format t "~{~a~%~}~%"
          (loop for v in vars collecting
               (list (value v) (domain v) (prunes v)))))

(defmethod recurse ((csp csp) vars solver-function
                    statistics dynamic-ordering call-type consistent)
  "Generic function to recurse to the next level, handling solutions being found"
  (with-accessors ((n-solutions n-solutions) (n-nodes n-nodes)) statistics
    (when consistent
      (if (= (length vars) 1)
          (progn
            (found-solution csp)
            (incf n-solutions))

          (case call-type
            (var
             (let ((old-var (pop (vars csp))))
               (push old-var (past-vars csp))
               (funcall solver-function csp
                        statistics dynamic-ordering)
               (setf (past-vars csp) (delete old-var (past-vars csp)))
               (push old-var (vars csp))))

            (domain
             (funcall solver-function csp statistics dynamic-ordering))

            (otherwise (error "Invalid call-type")))))))

(defun all-pairs (xs)
  "Makes a list of all pairs in xs without the same element"
  (let ((pairs ()))
    (loop for x1 in xs do
         (loop for x2 in xs
            when (not (eq x1 x2)) do
              (push (list x1 x2) pairs)))
    (nreverse pairs)))

(defmethod ac3 ((csp csp))
  "Carries out the ac3 arc consistency algorithm on the given csp"
  (loop for var in (append (past-vars csp) (vars csp)) do
       (push nil (prunes var)))
  (let ((queue (all-pairs (vars csp))))
    (loop while (> (length queue) 0) do
         (let ((pair (pop queue)))
           (when (apply #'ac3-revise csp pair)
             (let ((arcs-with-posssible-changes
                    (loop for var in (vars csp)
                       when (not (member var pair)) collect
                         (list var (first pair)))))
               (setf queue (nconc queue arcs-with-posssible-changes)))))))

  (loop for var in (vars csp)
     when (= (length (domain var)) 0) do
       (return-from ac3 nil))
  t)

(defmacro def-constraint-solver (name left-branch
                                 &optional (right-branch left-branch))
  "Defines a constraint solver function of the given name which carries out the specified instructions on the left and right branches of recursion

left and right branch should take three arguments, the csp, the call-type of the branch and the kind of dynamic-ordering in use, the latter two of which can be passed unadultered to the recurse function."
  `(defmethod ,name ((csp csp) statistics dynamic-ordering)
     (flet ((left-branch (csp call-type statistics)
              ,left-branch)
            (right-branch (csp call-type statistics)
              ,right-branch))
       (solve csp statistics #'left-branch
              #'right-branch dynamic-ordering))))

(defmethod solve ((csp csp) statistics left-branch
                  right-branch dynamic-ordering)
  "Generic function to solve a constraint program using the given solver specification and CSP"
  (with-accessors ((vars vars)) csp
    (when dynamic-ordering
      (setf vars (funcall dynamic-ordering vars)))

    (with-accessors ((n-solutions n-solutions) (n-nodes n-nodes)) statistics
      (incf n-nodes)
      (let ((var (first vars)))
        (with-accessors ((domain domain)) var
          ;; Left branch
          (setf (value var) (first domain))
          (funcall left-branch csp 'var statistics)
          (setf (value var) nil)

          ;; Right branch
          (let ((to-restore (pop domain)))
            (when (> (length domain) 0)
              (funcall right-branch csp 'domain statistics))
            (push to-restore domain)))
        statistics))))

(def-constraint-solver backtracking
    ;;Left branch
    (let ((consistent t))
      (loop for past-var in (past-vars csp)
         while consistent
         do (setf consistent
                  (test-constraint csp past-var (first (vars csp)))))

      (recurse csp (vars csp) #'backtracking
               statistics dynamic-ordering call-type consistent))

  ;;Right branch
  (backtracking csp statistics dynamic-ordering))

(def-constraint-solver forward-checking
    (let* ((consistent
            (revise-future-arcs csp (vars csp)))
           ;; Keep track of what to undo, as the list may get reordered
           (to-undo (copy-list (rest (vars csp)))))
      (recurse csp (vars csp) #'forward-checking
               statistics dynamic-ordering call-type consistent)
      (mapcar #'undo-pruning to-undo)))

(def-constraint-solver mac3
    (let ((consistent (ac3 csp)))
      (recurse csp (vars csp) #'mac3
               statistics dynamic-ordering call-type consistent)
      (mapcar #'undo-pruning (append (past-vars csp) (vars csp)))))

(defun run-solver-on-file (solver filename ordering
                           &optional (print-solutions-p t))
  "Runs the given solver on the csp description in the given file"
  (defparameter *print-solutions-p* print-solutions-p)
  (flet ((lowest-deg-dom (vars)
           (sort vars #'< :key (lambda (x) (/ (length (domain x)) (degree x)))))

         (greatest-deg-dom (vars)
           (sort vars #'> :key (lambda (x) (/ (length (domain x)) (degree x)))))

         (smallest-dom (vars)
           (sort vars #'< :key (lambda (x) (length (domain x)))))

         (largest-dom (vars)
           (sort vars #'> :key (lambda (x) (length (domain x)))))

         (lowest-deg (vars) (sort vars #'> :key #'degree))

         (greatest-deg (vars) (sort vars #'> :key #'degree)))

  (with-open-file (file filename)
    (let ((statistics (make-instance 'statistics))
          (start-time (get-internal-run-time)))
      (let ((csp (parse-csp (read file)))
            (dynamic-ordering))

        (ccase ordering
          (greatest-deg
           (setf (vars csp) (greatest-deg (vars csp))))
          (lowest-deg
           (setf (vars csp) (lowest-deg (vars csp))))
          (default nil)
          (lowest-deg-dom (setf dynamic-ordering #'lowest-deg-dom))
          (greatest-deg-dom (setf dynamic-ordering #'greatest-deg-dom))
          (smallest-dom (setf dynamic-ordering #'smallest-dom))
          (largest-dom (setf dynamic-ordering #'largest-dom)))
;;          (otherwise (error "Ordering should be one of 'greatest-deg, 'lowest-deg, 'default, 'lowest-deg-dom, 'greatest-deg-dom, 'smallest-dom or 'largest-dom")))

        (funcall solver csp statistics dynamic-ordering))

      (setf (time-taken statistics)
            (- (get-internal-run-time) start-time))
      (print-statistics statistics)
      statistics))))

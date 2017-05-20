(in-package #:constraint-solver)
(require 'cl-ppcre)

;; A list of the folder name for the CSP class, the lowest value at which execution takes too long and the biggest instance size for the class
(defparameter *test-data* '(("nqueens" 12 25)
                            ("langfordPairings" 8 12)
                            ("crystalMaze" 6 10)))

(defparameter *test-functions* (list #'backtracking #'forward-checking #'mac3))
(defparameter *ordering-types* '(default greatest-deg
                                 lowest-deg-dom greatest-deg-dom lowest-deg
                                 smallest-dom largest-dom))

(defparameter *times-to-run-test* 3)
(defparameter *function-names* (list "backtracking" "forwardChecking" "MAC3"))

(defun print-results (results)
  "Pretty prints the results created from running the tests"
  (loop for test in results do
       (format t "~a:~%" (first test))
       (loop for run in (rest test) do
            (print-statistics run))
       (format t "~%")))

(defun generate-test-runs (test-data)
  "Generates file names and other information needed for the test function"
  ;; Map over the different class definitions
  (mapcar
   (lambda (datum)
     (list (first datum)
           (remove-if
            ;; Works out if the instance should be excluded
            (lambda (path)
              (let ((exclude nil))
                (loop for long in
                     (loop for i from (second datum)
                        to (third datum)
                        collect (format nil "~a" i)) do
                     (when (search long (namestring path))
                       (setf exclude t)))
                exclude))

            ;; All instance files in the given directory
            (directory (format nil "../~a/*.csp" (first datum))))))
   test-data))

(defun product (list1 list2)
  "Cartesian product of two lists"
  (let (templist)
    (dolist (item1 list1)
      (dolist (item2 list2)
        (push (list item1 item2) templist)))
    (nreverse templist)))


(defun average-statistics (statistics)
  "Returns a new statistic object with the average of the given statistics"
  (let ((avg-n-solutions 0)
        (avg-n-nodes 0)
        (avg-time-taken 0))
    (dolist (statistic statistics)
      (incf avg-n-solutions (n-solutions statistic))
      (incf avg-n-nodes (n-nodes statistic))
      (incf avg-time-taken (time-taken statistic)))
    (setf avg-n-solutions (/ avg-n-solutions (length statistics)))
    (setf avg-n-nodes (/ avg-n-nodes (length statistics)))
    (setf avg-time-taken (/ avg-time-taken (length statistics)))
    (let ((avg-statistic (make-instance 'statistics)))
      (setf (n-solutions avg-statistic) avg-n-solutions)
      (setf (n-nodes avg-statistic) avg-n-nodes)
      (setf (time-taken avg-statistic) avg-time-taken)
      avg-statistic)))

(defun run-test (function file ordering)
  "Runs a test function over a file with the given ordering a number of times given by *times-to-run-test* and averages the results."
  (average-statistics
   (loop for i below *times-to-run-test* collecting
        (run-solver-on-file function file ordering nil))))

(defun generate-input-for-gnuplot (results)
  "Puts the results into a format which gnuplot can consume and outputs them to files"
  (loop for element in (list
                        (list #'time-taken "Time taken in ms")
                        (list #'n-nodes "Number of nodes visited")) do
       (with-open-file (stream
                        (pathname (format nil "testResults/~a for ~a"
                                          (second element) (first results)))
                        :direction :output
                        :if-exists :supersede :if-does-not-exist :create)
         (format stream "#~a~%" (second element))
         (format stream "~{~a ~}~%"
                 (cons "inputSize"
                       (mapcar
                        (lambda (x) (concatenate 'string (first x) "_" (second x)))
                        (product *function-names*
                                 (mapcar
                                  (lambda (x) (nstring-downcase (symbol-name x)))
                                  *ordering-types*)))))
         (loop for test in (second results) do
              (format stream "~a"
                      (cl-ppcre:scan-to-strings "[0-9]+"
                                                (pathname-name (first test))))
              (loop for run in (rest test) do
                   (format stream " ~a " (funcall (first element) run)))
              (format stream "~%")))))


(defun run-all-tests ()
  (let* ((test-sets (generate-test-runs *test-data*)))
    (loop for test-files in test-sets do
         (generate-input-for-gnuplot
          (list (first test-files)
                (loop for csp in (second test-files) collecting
                     (cons csp
                           (loop for function-order-pair in
                                (product *test-functions* *ordering-types*)
                              collecting
                                (run-test
                                 (first function-order-pair)
                                 csp (second function-order-pair))))))))))

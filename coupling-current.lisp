
(in-package #:huber-braun)

(defun coupling-current (V linked-neurons)
  "The sum of the currents due to gap junction coupling."
 (reduce #'+ (mapcar (lambda (n)
		       (let ((g-c (first n))
			     (V-c (slot-value (second n) 'V)))
			 (* g-c (- V V-c))))
		     linked-neurons)))

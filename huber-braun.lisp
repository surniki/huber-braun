;;;; huber-braun.lisp

(in-package #:huber-braun)

(defmodel huber-braun (V a-K a-sd a-sr)
  (dV-wrt-dt      (/ (- (- I-leak) I-Na I-K I-sd I-sr I-inj I-coupling) C))         ;; checked
  (d-a-K-wrt-dt   (* (/ phi tau-K) (- a-K-inf a-K)))                                ;; checked
  (d-a-sd-wrt-dt  (* (/ phi tau-sd) (- a-sd-inf a-sd)))                             ;; checked
  (d-a-sr-wrt-dt  (* (- (/ phi tau-sr)) (+ (* v-acc I-sd) (* v-dep a-sr))))         ;; checked
  (I-leak         (* g-leak (- V V-leak)))                                          ;; checked
  (a-Na           (/ 1.0 (+ 1.0 (exp (* (- s-Na) (- V V-0Na))))))                   ;; checked
  (I-Na           (* rho g-Na a-Na (- V V-Na)))                                     ;; checked
  (I-K            (* rho g-K a-K (- V V-K)))                                        ;; checked
  (I-sd           (* rho g-sd a-sd (- V V-sd)))                                     ;; checked
  (I-sr           (* rho g-sr a-sr (- V V-sr)))                                     ;; checked
  (a-K-inf        (/ 1.0 (+ 1.0 (exp (* -1.0 s-K (- V V-0K))))))                    ;; checked
  (a-sd-inf       (/ 1.0 (+ 1.0 (exp (* -1.0 s-sd (- V V-0sd))))))                  ;; checked
  (I-coupling     (coupling-current V linked-model-objects)))

(defprofile tonic huber-braun
  (I-inj   1.0)     ;; checked
  (C       1.0)     ;; checked
  (g-leak  0.1)     ;; checked
  (V-leak  -60.0)   ;; checked
  (rho     0.607)   ;; checked
  (g-Na    1.5)     ;; checked
  (V-Na    50.0)    ;; checked
  (g-K     2.0)     ;; checked
  (V-K     -90.0)   ;; checked
  (g-sd    0.25)    ;; checked
  (V-sd    50.0)    ;; checked
  (g-sr    0.25)    ;; checked
  (V-sr    -90.0)   ;; checked
  (s-Na    0.25)    ;; checked
  (V-0Na   -25.0)   ;; checked
  (phi     0.124)   ;; checked
  (tau-K   2.0)     ;; checked
  (tau-sd  10.0)    ;; checked
  (tau-sr  20.0)    ;; checked
  (v-acc   0.17)    ;; checked
  (v-dep   0.012)   ;; checked
  (s-K     0.25)    ;; checked
  (V-0K    -25.0)   ;; checked
  (s-sd    0.09)    ;; checked
  (V-0sd   -40.0))  ;; checked

(defprofile bursting huber-braun
  (I-inj   1.0)     ;; checked
  (C       1.0)     ;; checked
  (g-leak  0.1)     ;; checked
  (V-leak  -60.0)   ;; checked
  (rho     0.607)   ;; checked
  (g-Na    1.5)     ;; checked
  (V-Na    50.0)    ;; checked
  (g-K     2.0)     ;; checked
  (V-K     -90.0)   ;; checked
  (g-sd    0.25)    ;; checked
  (V-sd    50.0)    ;; checked
  (g-sr    0.35)    ;; checked
  (V-sr    -90.0)   ;; checked
  (s-Na    0.25)    ;; checked
  (V-0Na   -25.0)   ;; checked
  (phi     0.124)   ;; checked
  (tau-K   2.0)     ;; checked
  (tau-sd  10.0)    ;; checked
  (tau-sr  20.0)    ;; checked
  (v-acc   0.17)    ;; checked
  (v-dep   0.012)   ;; checked
  (s-K     0.25)    ;; checked
  (V-0K    -25.0)   ;; checked
  (s-sd    0.09)    ;; checked
  (V-0sd   -40.0))  ;; checked 

(defmacro with-open-model-digraph ((digraph) &body body)
  `(progn
     (open-model-digraph ,digraph)
     ,@body
     (close-model-digraph ,digraph)))

(defun simulate ()
  (let* ((g-c 0.0)
	 (n1 (make-instance 'huber-braun-tonic :V 0.0 :a-K 0.0 :a-sd 0.0 :a-sr 0.0 :id 'n1))
	 (n2 (make-instance 'huber-braun-bursting :V 0.0 :a-K 0.0 :a-sd 0.0 :a-sr 0.0 :id 'n2))
	 (network (digraph :nodes (list (node :name 'n1 :value n1)
					(node :name 'n2 :value n2))
			   :edges (list (edge :tail 'n1 :head 'n2 :value g-c)
					(edge :tail 'n2 :head 'n1 :value g-c)))))
    (with-open-model-digraph (network)
      (loop :repeat (/ 5000 0.01)
	    :do
	       (output-model-digraph-state network)
	       (update-model-digraph network)
	    :finally
	       (output-model-digraph-state network)))))

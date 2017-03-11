(in-package :universe-2d)

(defun square (x)
  (* x x))

(defun iround (x)
  "Round X to an integer value."
  (multiple-value-bind (rounded rem) (round x)
    (declare (ignore rem))
    rounded))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vector2->list (vec)
  (list (x vec) (y vec)))

(defun vector2-map (f v)
  (v2:make (funcall f (x v))
           (funcall f (y v))))

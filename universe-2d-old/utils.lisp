(in-package :universe-2d)

(defun take (n list)
  "Take N elements from the beginning of the LIST."
  (labels ((rec (n acc list)
             (if (> n 0)
                 (rec (- n 1) (cons (first list) acc) (rest list))
                 acc)))
    (rec n '() list)))

(defun partition (n list)
  (when (not (eq list nil))
    (let ((part (take n list)))
      (when (= n (length part))
        (cons part (partition n (nthcdr n list)))))))

(defun times (n fn)
  "Call FN an N number of times."
  ;; FIXME: Probably bad practice
  (let ((acc '()))
    (dotimes (i n)
      (push (funcall fn i) acc))
    acc))

(defun insert-first (list elem)
  (list* (car list) elem (cdr list)))

(defun insert-last (list elem)
  (append list (list elem)))

(defmacro mac (expr)
  "Pretty print the macro-expansion for EXPR."
  (pprint (macroexpand-1 expr)))

(defmacro fundoc (f)
  "Print documentation for function F."
  (documentation f 'function))

(defmacro vardoc (f)
  "Print documentation for variable F."
  (documentation f 'variable))

(defmacro swap (expr f &rest args)
  "Set the value of EXPR to the result of applying F
to the to the value of EXPR and any other optional arguments.
Similar to clojure swap!."
  `(setf ,expr (,f ,expr ,@args)))

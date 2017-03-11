;;;; universe-2d.lisp

(in-package #:universe-2d)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game engine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clear-screen (renderer &optional (color '(0 0 0 0)))
  "Clears the entire screen. Can provide an optional
COLOR to clear to."
  (apply #'sdl2:set-render-draw-color renderer color)
  (sdl2:render-clear renderer))

(defun draw-rect (renderer rect color)
  (apply #'sdl2:set-render-draw-color renderer color)
  (sdl2:render-fill-rect renderer (apply #'sdl2:make-rect rect)))

(defun draw-line (renderer v1 v2 color)
  ;; TODO: Write macro to remove duplication
  (apply #'sdl2:set-render-draw-color renderer color)
  (sdl2:render-draw-line renderer
                         ;; FIXME: Remove this duplication, we should
                         ;; FIXME: implement a solid API for interfacing
                         ;; FIXME: with SDL rects
                         (iround (x v1))   ;; x1
                         (iround (y v1))   ;; y1
                         (iround (x v2))   ;; x2
                         (iround (y v2)))) ;; y2

(defun main (main-loop title width height)
  (sdl2:with-init (:everything)
    (format t "Using SDL library version: ~D.~D.~D~%"
               sdl2-ffi:+sdl-major-version+
               sdl2-ffi:+sdl-minor-version+
               sdl2-ffi:+sdl-patchlevel+)

    (sdl2:with-window (win :title title
                           :w width
                           :h height
                           :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
        (funcall main-loop renderer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Force-directed layout algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vertex-offset (v u f)
  (let ((delta (v2:- (pos v) (pos u))))
    (if (v2:0p delta)
        (v2:make 0f0 0f0)
        (v2:*s (v2:/s delta (v2:length delta))
               (funcall f (v2:length delta))))))

(defun attractive-displacement (v u k)
  "Returns the attractive displacements for vector V and U respectively."
  (let ((offset (vertex-offset v u #'(lambda (d)
                                       (/ (square d) k)))))
    (cons (v2:- (disp v) offset)
          (v2:+ (disp u) offset))))

(defun repulsive-displacement (node nodes k)
  (reduce #'(lambda (disp v)
              (if (eq v node)
                  disp
                  (v2:+ disp (vertex-offset
                              node v #'(lambda (d)
                                         (/ (square k) d))))))
          nodes
          :initial-value (v2:make 0f0 0f0)))

(defun calculate-attractive-forces (edges k)
  (dolist (e edges)
    (let ((u (element (vertex-1 e)))
          (v (element (vertex-2 e))))
      (let ((disp (attractive-displacement u v k)))
        (format t "Attractive displacement: ~A -> ~A, ~A -> ~A~%"
                (id u) (car disp) (id v) (cdr disp))
        (setf (pos v) (car disp)
              (pos u) (cdr disp))))))

(defun calculate-repulsive-forces (nodes k)
  (dolist (n nodes)
    (let ((disp (repulsive-displacement n nodes k)))
      (format t "Repulsive displacement ~A -> ~A~%" (id n) disp)
      (setf (disp n) disp))))

(defun limit-displacement (nodes temp W L)
  "Limit displacement to temperature TEMP and prevent from
displacement outside frame."
  (dolist (v nodes)
    ;; TODO: magnitude may not be correct
    ;; v.pos += (v.disp / |v.disp|) * min(v.disp, t)
    (swap (pos v) v2:+ (v2:*s (v2:/s (disp v)
                                     (v2:length (disp v)))
                              (float (min (v2:length (disp v)) temp))))
    ;; v.pos.x = min(W/2, max(-W/2, v.pos.x))
    (swap (aref (pos v) 0) clamp 0.0 (float W))
    ;; v.pos.y = min(L/2, max(-L/2, v.pos.y))
    (swap (aref (pos v) 1) clamp 0.0 (float L))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter +screen-width+ 2048)
(defparameter +screen-height+ 1280)

;; We represent the universe as a directed cycle graph
;; (Note: It depends on whether we will have a boundary
;;        if it will be acyclical)
(defparameter *universe* (cl-graph:make-graph
                          'cl-graph:graph-container
                          :default-edge-type :undirected))

(defclass star-system ()
  ((id :initarg :id :reader id)
   (position
    :initarg :pos
    :initform (v2:make 0.0 0.0)
    :accessor pos)
   (displacement
    :initarg :disp
    :initform (v2:make 0.0 0.0)
    :accessor disp)))

(defun universe-systems (universe)
  (mapcar #'element (cl-graph:vertexes universe)))

(defun universe-edges (universe)
  (mapcar #'(lambda (e)
              (cons (element (vertex-1 e))
                    (element (vertex-2 e))))
          (cl-graph:edges universe)))

(defparameter area 10000)
(defparameter gravity 10)
(defparameter velocity (v2:make (float 1/800)
                                (float 1/800)))

(defun simulate-universe (universe elapsed)
  (let* ((nodes (universe-systems universe))
         (edges (universe-edges universe))
         (k (/ (sqrt area) (+ 1 (length nodes))))
         (max-displace (/ (sqrt (square area)) 10.0)))
    ;; Reset displacement
    (dolist (n nodes)
      (setf (disp n) (v2:make 0.0 0.0)))
    ;; Repulsive forces
    (dolist (n1 nodes)
      (dolist (n2 nodes)
        (when (not (eql n1 n2))
          (let* ((xdist (- (x (pos n1)) (x (pos n2))))
                 (ydist (- (y (pos n1)) (y (pos n2))))
                 (dist (sqrt (+ (square xdist) (square ydist)))))
            (when (> dist 0)
              (let ((repulsivef (/ (square k) dist)))
                (swap (x (disp n1)) + (* (/ xdist dist) repulsivef))
                (swap (y (disp n1)) + (* (/ ydist dist) repulsivef))))))))
    ;; Attractive forces
    (dolist (e edges)
      (let ((nf (car e))
            (nt (cdr e)))
        (let* ((xdist (- (x (pos nf)) (x (pos nt))))
               (ydist (- (y (pos nf)) (y (pos nt))))
               (dist (sqrt (+ (square xdist) (square ydist)))))
          (when (> dist 0)
            (let ((attractivef (/ (square dist) k)))
              (swap (x (disp nf)) - (* (/ xdist dist) attractivef))
              (swap (y (disp nf)) - (* (/ ydist dist) attractivef))
              (swap (x (disp nt)) + (* (/ xdist dist) attractivef))
              (swap (y (disp nt)) + (* (/ ydist dist) attractivef)))))))
    ;; Apply gravity, TODO: whatever that means?
    ;; (dolist (n nodes)
    ;;   (let* ((d (sqrt (* (x (pos n)) (y (pos n)))))
    ;;          (gf (* 0.01 k (float gravity) d)))
    ;;     (swap (x (pos n)) - (* gf (/ (x (pos n)) d)))
    ;;     (swap (y (pos n)) - (* gf (/ (y (pos n)) d)))))
    ;; Apply velocity
    (dolist (n nodes)
      (swap (x (disp n)) + (* (x velocity) elapsed))
      (swap (y (disp n)) + (* (y velocity) elapsed)))
    ;; Apply dist and keep within bounds
    (dolist (n nodes)
      (let* ((xdist (x (disp n)))
             (ydist (y (disp n)))
             (dist (sqrt (+ (square xdist) (square ydist)))))
        (when (> dist 0)
          (let ((limited-dist (min (* max-displace (/ 1.0 800.0)) dist)))
            (swap (x (pos n)) + (* (/ xdist dist) limited-dist))
            (swap (y (pos n)) + (* (/ ydist dist) limited-dist))))))))

(defun render (renderer elapsed)
  ;; Update simulations
  (simulate-universe *universe* elapsed)
  ;; Clear to black
  (clear-screen renderer)
  ;; Render universe graph
  (dolist (e (cl-graph:edges *universe*))
    (let ((u (element (vertex-1 e)))
          (v (element (vertex-2 e))))
      (draw-line renderer (pos u) (pos v) '(0 255 0 0))))
  (dolist (v (universe-systems *universe*))
    (let ((rect (mapcar #'iround (append
                                  (vector2->list (pos v)) '(25 25)))))
      (draw-rect renderer rect '(255 0 0 0)))))

(defun main-loop (renderer)
  (sdl2:with-event-loop (:method :poll)
    (:idle ()
           (sdl2:delay 16)
           ;; TODO: Implement timing
           (render renderer 0.16)
           (sdl2:render-present renderer))
    (:quit () t)))

(defun repl-start ()
  (sdl2:make-this-thread-main
   (lambda ()
     (main #'main-loop
           "Universe 2D"
           +screen-width+
           +screen-height+))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Seed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set-random-positions (nodes &optional (max 800))
  "Set a random position X and Y position for NODES. The MAX value
is set to 800 by default."
  ;; For now our data representation will be '((X Y) VERT)
  (dolist (n nodes)
    (let ((random-pos (v2:make (float (random max))
                               (float (random max)))))
      (setf (pos n) random-pos))))

(defun populate-graph (graph)
  (let ((systems (times 100 (lambda (i)
                              (make-instance 'star-system :id i)))))
    ;; (loop for v in systems do
    ;;   (cl-graph:add-vertex graph v))
    (loop for (v1 . v2) in (partition 2 (shuffle systems)) do
      (cl-graph:add-edge-between-vertexes graph v1 (car v2))))
  graph)

(defun run ()
  (setf *universe* (cl-graph:make-graph
                    'cl-graph:graph-container
                    :default-edge-type :undirected))
  (populate-graph *universe*)
  (set-random-positions (universe-systems *universe*))
  (repl-start))

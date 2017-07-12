
(in-package :sandpaint)


(defun make-rgba-array (size)
  (make-array
    (list size size 4)
    :adjustable nil
    :initial-element 0.0d0
    :element-type 'double-float))


(defun -scale-convert (v &key (s 1.0d0) (gamma 1.0d0))
  (declare (double-float v s gamma))
  (setf v (expt (/ v s) gamma)))


(defun -unsigned-256 (v)
  (declare (double-float v))
  (cond
    ((> v 1.0d0) 255)
    ((< v 0.0d0) 0)
    (t (round (* 255 v)))))


(defun -setf-operator-over (vals x y i a color)
  (declare (integer x y i))
  (declare (double-float a color))
  (setf (aref vals x y i)
        (+ (* (aref vals x y i) a) color)))

(defun -operator-over (vals x y r g b a)
  (declare (integer x y))
  (declare (double-float r g b a))
  (let ((ia (- 1.0 a)))
    (declare (double-float ia))
    (-setf-operator-over vals x y 0 ia r)
    (-setf-operator-over vals x y 1 ia g)
    (-setf-operator-over vals x y 2 ia b)
    (-setf-operator-over vals x y 3 ia a)))


(defun -draw-stroke (vals size grains v1 v2 r g b a)
  (declare (list v1 v2))
  (declare (double-float r g b a))
  (loop for i integer from 0 below grains do
    (inside* (size (rnd:on-line v1 v2) x y)
      (-operator-over vals x y r g b a))))


(defun copy-rgba-array-to-from (target source size)
  (square-loop (x y size)
    (setf (aref target x y 0) (aref source x y 0)
          (aref target x y 1) (aref source x y 1)
          (aref target x y 2) (aref source x y 2)
          (aref target x y 3) (aref source x y 3))))


(defstruct sandpaint
  (vals nil :read-only nil)
  (size nil :type integer :read-only nil)
  (r 0.0d0 :type double-float :read-only nil)
  (g 0.0d0 :type double-float :read-only nil)
  (b 0.0d0 :type double-float :read-only nil)
  (a 1.0d0 :type double-float :read-only nil))


(defun -draw-circ (vals size xy rad grains r g b a)
  (declare (double-float r g b a))
  (loop for i integer below grains do
    (inside* (size (math:add xy (rnd:in-circ rad)) x y)
      (-operator-over vals x y r g b a))))


(defun -offset-rgba (new-vals old-vals size x y nxy i)
  (destructuring-bind (nx ny)
    (mapcar #'round nxy)
    (if (and (>= nx 0) (< nx size) (>= ny 0) (< ny size))
      (setf (aref new-vals nx ny i) (aref old-vals x y i)))))

(defun chromatic-aberration (sand center &key (s 1.0) (noise 1.0))
  (with-struct (sandpaint- size vals) sand
    (let ((new-vals (make-rgba-array size)))
      (copy-rgba-array-to-from new-vals vals size)

      (square-loop (x y size)
        (let* ((xy (list x y))
               (dx (math:iscale
                     (math:sub (math:add (rnd:in-circ noise) xy) center)
                     s)))
          (-offset-rgba new-vals vals size x y (math:add xy dx) 0)
          (-offset-rgba new-vals vals size x y (math:sub xy dx) 2)))

      (setf (sandpaint-vals sand) new-vals))))


(defun -png-tuple (vals x y gamma)
  (declare (integer x y))
  (declare (double-float gamma))
  (let ((a (aref vals x y 3)))
    (declare (double-float a))
    (if (> a 0.0d0)
      (list
        (-unsigned-256 (-scale-convert (aref vals x y 0) :s a :gamma gamma))
        (-unsigned-256 (-scale-convert (aref vals x y 1) :s a :gamma gamma))
        (-unsigned-256 (-scale-convert (aref vals x y 2) :s a :gamma gamma))
        (-unsigned-256 (-scale-convert a :gamma gamma)))
      (list 0 0 0 0))))


(defun clear (sand rgba)
  (destructuring-bind (r g b a)
    rgba
    (declare (double-float r g b a))
    (with-struct (sandpaint- size vals) sand
      (square-loop (x y size)
        (setf (aref vals x y 0) (* a r)
              (aref vals x y 1) (* a g)
              (aref vals x y 2) (* a b)
              (aref vals x y 3) a)))))


(defun make (size &key (active '(0.0d0 0.0d0 0.0d0 1.0d0))
                       (bg '(1.0d0 1.0d0 1.0d0 1.0d0)))
  (destructuring-bind (ar ag ab aa br bg bb ba)
    (mapcar (lambda (x) (math:dfloat x))
            (append active bg))

    (let ((vals (make-rgba-array size)))
      (square-loop (x y size)
        (setf (aref vals x y 0) (* ba br)
              (aref vals x y 1) (* ba bg)
              (aref vals x y 2) (* ba bb)
              (aref vals x y 3) ba))

      (make-sandpaint
        :size size
        :r (* ar aa)
        :g (* ag aa)
        :b (* ab aa)
        :a aa
        :vals vals))))


(defun set-rgba (sand rgba)
  (destructuring-bind (r g b a)
    (math:dfloat* rgba)
    (declare (double-float r g b a))
    (setf (sandpaint-r sand) (* r a)
          (sandpaint-g sand) (* g a)
          (sandpaint-b sand) (* b a)
          (sandpaint-a sand) a)))


(defun pixel-hack (sand &optional (sa 0.9d0))
  "
  scale opacity of pix (0 0) by sa.
  "
  (let ((vals (sandpaint-vals sand)))
    (destructuring-bind (r g b a)
      (mapcar (lambda (i) (aref vals 0 0 i)) (math:range 4))
      (declare (double-float r g b a))
      (if (>= a 1.0d0)
        (let ((na (* a (math:dfloat sa))))
          (setf (aref vals 0 0 0) (* (/ r a) na)
                (aref vals 0 0 1) (* (/ g a) na)
                (aref vals 0 0 2) (* (/ b a) na)
                (aref vals 0 0 3) na))))))


(defun pix (sand vv)
  (with-struct (sandpaint- size vals r g b a) sand
    (loop for v list in vv do
      (inside* (size v x y)
        (-operator-over vals x y r g b a)))))


(defun pix* (sand vv n)
  (with-struct (sandpaint- size vals r g b a) sand
    (loop for i integer from 0 below n do
      (inside* (size (get-atup vv i) x y)
        (-operator-over vals x y r g b a)))))


(defun circ (sand vv rad n)
  (with-struct (sandpaint- size vals r g b a) sand
    (loop for v list in vv do
      (-draw-circ vals size v rad n r g b a))))


; draw circ from array
(defun circ* (sand vv num rad grains)
  (with-struct (sandpaint- size vals r g b a) sand
    (loop for i integer from 0 below num do
      (-draw-circ vals size (get-atup vv i)
                  rad grains r g b a))))


(defun strokes (sand lines grains)
  (with-struct (sandpaint- size vals r g b a) sand
    (loop for (u v) in lines do
      (-draw-stroke vals size grains u v r g b a))))


(defun stroke (sand line grains)
  (with-struct (sandpaint- size vals r g b a) sand
    (destructuring-bind (u v)
      line
      (-draw-stroke vals size grains u v r g b a))))


(defun lin-path (sand path rad grains &key (dens 1))
  (with-struct (sandpaint- size vals r g b a) sand
    (loop for u list in path and w list in (cdr path) do
      (let ((stps (math:int (floor (+ 1 (* dens (math:dst u w)))))))
        (declare (integer stps))
        (math:rep (p (math:linspace 0 1 stps :end nil))
          (-draw-circ vals size (math:on-line p u w) rad grains r g b a))))))


; TODO: 16 bit?
(defun save (sand fn &key (gamma 1.0d0))
  (let ((fnimg (append-postfix
                 (aif fn fn
                      (progn
                        (warn "missing file name, using: tmp.png")
                        "tmp"))
                 ".png"))
        (gamma* (math:dfloat gamma)))
    (with-struct (sandpaint- size vals) sand
      (let ((png (make-instance
                   'zpng::pixel-streamed-png
                   :color-type :truecolor-alpha
                   :width size
                   :height size)))

        (with-open-file
          (stream fnimg
            :direction :output
            :if-exists :supersede
            :if-does-not-exist :create
            :element-type '(unsigned-byte 8))
          (zpng:start-png png stream)
          (square-loop (x y size)
            (zpng:write-pixel (-png-tuple vals y x gamma*) png))
          (zpng:finish-png png))))
    (format t "~%file: ~a~%~%" fnimg)))



(in-package :bzspl)

;M = 1/6
;((1 4 1 0)
;(-3 0 3 0)
;(3 -6 3 0)
;(-1 3 -3 1))


(defvar *m*)
;(setf *m* '((1.0d0 4.0d0 1.0d0 0.0d0)
;            (-3.0d0 0.0d0 3.0d0 0.0d0)
;            (3.0d0 -6.0d0 3.0d0 0.0d0)
;            (-1.0d0 3.0d0 -3.0d0 1.0d0)))

;(setf *m* '((1.0d0 0.0d0 0.0d0 0.0d0)
;            (-3.0d0 3.0d0 0.0d0 0.0d0)
;            (3.0d0 -6.0d0 3.0d0 0.0d0)
;            (-1.0d0 3.0d0 -3.0d0 1.0d0)))

(setf *m* '((1.0d0 0.0d0 0.0d0 )
            (-2.0d0 2.0d0 0.0d0)
            (1.0d0 -2.0d0 1.0d0)))

;(setf *m* '((1.0d0 1.0d0 0.0d0 )
;            (-2.0d0 2.0d0 0.0d0)
;            (1.0d0 -2.0d0 1.0d0)))


(defstruct bzspl
  (n nil :type integer :read-only t)
  (closed nil :type boolean)
  (select-pts nil :type function)
  (get-seg nil :type function)
  (pts nil))


(defun do-m (pts)
  (loop for mrow in *m* collect
    (let ((s (list 0.0d0 0.0d0)))
      (loop for p in pts and mr in mrow do
        (setf s (math:add s (math:scale p mr))))
      s)))


(defun do-t (x pk)
  (let ((s (list 0.0d0 0.0d0)))
    (loop for p in pk and xi in (list 1.0d0 x (* x x)) do
      (setf s (math:add s (math:scale p xi))))
    s))


(defun -get-seg-open (n x)
  (let ((s (/ 1.0d0 (math:dfloat (- n 2)))))
    (if (>= x 1.0d0)
      (list
        1.0d0
        (- (floor (/ x s)) 1))
      (list
        (/ (mod x s) s)
        (floor (/ x s))))))


(defun -get-seg-closed (n x)
  (let ((s (/ 1.0d0 (math:dfloat n))))
    (list
      (/ (mod x s) s)
      (floor (/ x s)))))


(defun -mean (pts a b)
  (math:scale
    (math:add (get-atup pts a)
         (get-atup pts b))
    0.5d0))


(defun -select-pts-open (n pts seg)
  (cond ((< seg 1)
          (list
            (get-atup pts 0)
            (get-atup pts 1)
            (-mean pts 1 2)))
        ((< seg (- n 3))
          (list
            (-mean pts seg (+ seg 1))
            (get-atup pts (+ seg 1))
            (-mean pts (+ seg 1) (+ seg 2))))
        (t
          (list
            (-mean pts (- n 3) (- n 2))
            (get-atup pts (- n 2))
            (get-atup pts (- n 1))))))


(defun -select-pts-closed (n pts seg)
  (list
    (-mean pts (mod seg n) (mod (+ seg 1) n))
    (get-atup pts (mod (+ seg 1) n))
    (-mean pts (mod (+ seg 1) n) (mod (+ seg 2) n))))


(defun pos (b x)
  (with-struct (bzspl- n pts get-seg select-pts) b
    (destructuring-bind (x-loc seg)
      (funcall get-seg n (math:dfloat x))
      (do-t x-loc (do-m (funcall select-pts n pts seg))))))


(defun pos* (b xx)
  (with-struct (bzspl- n pts get-seg select-pts) b
    (loop for x in (math:dfloat* xx) collect
      (destructuring-bind (x-loc seg)
        (funcall get-seg n x)
        (do-t x-loc (do-m (funcall select-pts n pts seg)))))))


(defmacro rndpos (b n)
  `(pos* ,b (rnd:rndspace 0.0d0 1.0d0 ,n)))


(defun make (pts &key closed &aux (n (length pts)))
  (assert (>= n 4) (n) "must have at least 4 pts. has ~a." n)
  (let ((apts (make-dfloat-array n)))
    (loop for (x y) in pts and i from 0 do
      (setf (aref apts i 0) (math:dfloat x)
            (aref apts i 1) (math:dfloat y)))
    (make-bzspl :n n
                :pts apts
                :select-pts (if closed
                                #'-select-pts-closed
                                #'-select-pts-open)
                :get-seg (if closed
                             #'-get-seg-closed
                             #'-get-seg-open)
                :closed closed)))


(defun -move-rel (pts i xy)
  (destructuring-bind (x y)
    xy
    (incf (aref pts i 0) x)
    (incf (aref pts i 1) y)))


(defun -move (pts i xy)
  (destructuring-bind (x y)
    xy
    (setf (aref pts i 0) x
          (aref pts i 1) y)))


(defun move (b pos &key rel)
  (let ((do-move (if rel #'-move-rel #'-move)))
    (with-struct (bzspl- pts) b
      (loop for xy in pos and i from 0 do
        (funcall do-move pts i xy)))))


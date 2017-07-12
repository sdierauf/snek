
(in-package :snek)

(defun -roll-once (aa)
  (butlast (append (last aa) aa) 1))


(defun edge-length (snk e)
  (with-struct (snek- verts) snk
    (apply #'math:dst (mapcar (lambda (v) (get-atup verts v)) e))))


(defun init-circ (snk num rad &key (xy (list 0.0d0 0.0d0)) g)
  (let ((vv (loop for p in (math:linspace 0.0d0 1.0d0 num)
                  collect (add-vert! snk (math:on-circ p rad :xy xy)))))
    (loop for a in vv and b in (-roll-once vv)
          collect (add-edge! snk (list a b) :g g))))


(defun init-polygon (snk rad n &key (xy (list 0.0d0 0.0d0)) (rot (* 0.25 PI)) g)
  (let ((vv (loop for v in (math:polygon n rad :xy xy :rot rot)
                  collect (add-vert! snk v))))
    (loop for a in vv and b in (-roll-once vv)
          collect (add-edge! snk (list a b) :g g))))


(defun init-path (snk points &key g closed)
  (let ((vv (add-verts! snk points)))
    (if closed
      (loop for a in vv and b in (-roll-once vv)
            collect (add-edge! snk (list a b) :g g))
      (loop for a in vv and b in (cdr vv)
            collect (add-edge! snk (list a b) :g g)))))


(defun draw-verts (snk sand)
  (with-struct (snek- verts num-verts) snk
    (sandpaint:pix* sand verts num-verts)))


(defun draw-circ (snk sand rad grains)
  (with-struct (snek- verts num-verts) snk
    (sandpaint:circ* sand verts num-verts rad grains)))


; SANDPAINT

(defun draw-edges (snk sand grains &key g)
  (with-struct (snek- verts) snk
    (sandpaint:strokes
      sand
      (map 'list (lambda (ab) (mapcar (lambda (i) (get-atup verts i)) ab))
           (get-edges snk :g g))
        grains)))


; EXPORT

(defun export-2obj (snk fn)
  (let ((verts (get-all-verts snk))
        (edges (get-edges snk))
        (fnobj (append-postfix fn ".2obj")))
    (with-open-file (stream
                      fnobj
                      :direction :output
                      :if-exists :supersede)
      (format stream "o mesh~%")
      (dolist (ll verts)
        (destructuring-bind (a b)
          ll
          (format stream "v ~f ~f~%" a b)))
      (dolist (ll (coerce edges 'list))
        (destructuring-bind (a b)
          (math:add ll '(1 1))
          (format stream "e ~d ~d~%" a b))))

    (format t "~%num verts: ~a ~%" (length verts))
    (format t "num edges: ~a ~%" (length edges))
    (format t "~%file: ~a" fnobj)))


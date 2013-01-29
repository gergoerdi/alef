(defvar *dot-mapping*)
(defvar *dot-stream*)

(defgeneric dot-from-gnode (gnode dot))

(defun dot-from-gref* (gref)
  (or (gethash gref *dot-mapping*)
      (let ((dot (gensym "GNODE")))
        (setf (gethash gref *dot-mapping*) dot)
        (dot-from-gnode (gderef gref) dot)
        dot)))

(defun format-symbol (x)
  (if (symbolp x) (symbol-name x)
      (format nil "~A" x)))

(defun dot-node (dot label &optional style)
  (format *dot-stream* "~&~A [label=\"~A\" ~A]" dot label (if style (format nil ", ~A" style) "")))

(defun dot-edge (dot/from dot/to label)
  (format *dot-stream* "~&~A -> ~A[label=\"~A\"]" dot/from dot/to label))

(defmethod dot-from-gnode ((gnode cons-gnode) dot)
  (dot-node dot (format-symbol (gnode-cons gnode)) "shape=box, fillcolor=chartreuse")
  (loop for arg-dot in (mapcar #'dot-from-gref* (gnode-args gnode))
        for index = 1 then (1+ index)
        do (dot-edge dot arg-dot (format nil "~D." index)))  )

(defmethod dot-from-gnode ((gnode var-gnode) dot)
  (dot-node dot (format-symbol (gnode-var gnode)) "shape=box, fillcolor=lightblue"))

(defmethod dot-from-gnode ((gnode apply-gnode) dot)
  (dot-node dot "" "shape=circle, ordering=out, fixedsize=true, width=.25")
  (dot-edge dot (dot-from-gref* (gnode-fun gnode)) "f")
  (dot-edge dot (dot-from-gref* (gnode-arg gnode)) "x"))

(defmethod dot-from-gnode ((gnode fun-gnode) dot)
  (dot-node dot (format-symbol (gnode-fun-name gnode)) "shape=box, fillcolor=yellow")
    (loop for arg-dot in (mapcar #'dot-from-gref* (gnode-args gnode))
        for index = 1 then (1+ index)
        do (dot-edge dot arg-dot (format nil "~D." index))))

(defun dot-from-gref (gref &optional (stream *standard-output*))
  (let ((*dot-mapping* (make-hash-table))
        (*dot-stream* stream))
    (format *dot-stream* "digraph G{~&")
    (format *dot-stream* "node[style=filled]")
    (dot-from-gref* gref)
    (format *dot-stream* "~&}")))

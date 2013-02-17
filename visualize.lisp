(defvar *dot-mapping*)
(defvar *dot-stream*)

(defparameter *visualize-grefs* nil)

(defgeneric to-dot (x rootp))

(defmethod to-dot ((gref gref) rootp)
  (or (gethash gref *dot-mapping*)
      (let ((dot (dot-name (if *visualize-grefs* gref (gderef gref)))))
        (setf (gethash gref *dot-mapping*) dot)
        (to-dot (gderef gref) rootp)
        (when *visualize-grefs*
          (dot-node (dot-name gref) "" "shape=point")
          (dot-edge (dot-name gref) (dot-name (gderef gref)) ""))
        dot)))

(defun format-symbol (x)
  (if (symbolp x) (symbol-name x)
      (format nil "~S" x)))

(defconstant +dot-escape-chars+ "\"")

(defun dot-escape (str)
  (labels ((escape-p (char)
             (find char +dot-escape-chars+))

           (escape-char (char)
             (format nil "\\~A" char)))
    (with-output-to-string (s)
      (loop for start = 0 then (1+ pos)
            for pos = (position-if #'escape-p str :start start)
            do (write-sequence str s :start start :end pos)
            when pos do (write-sequence (escape-char (char str pos)) s)
            while pos))))

(defun dot-node (dot label rootp &optional style)
  (format *dot-stream* "~&~A [label=\"~A\" ~A ~A]"
          dot
          (dot-escape label)
          (if rootp "penwidth=3" "")
          (if style (format nil ", ~A" style) "")))

(defun dot-edge (dot/from dot/to label)
  (format *dot-stream* "~&~A -> ~A[label=\"~A\"]" dot/from dot/to (dot-escape label)))

(defmethod to-dot ((gnode bottom-gnode) rootp)
  (dot-node (dot-name gnode) (format-symbol (gnode-var gnode)) rootp "shape=house, fillcolor=lightsalmon")
  ;; (dot-node dot "" "shape=house, fillcolor=lightsalmon")
  ;; (dot-node dot "" "shape=point")
  ;; (dot-edge dot dot "")
  )

(defmethod to-dot ((gnode cons-gnode) rootp)
  (let* ((prim (typecase (gnode-cons gnode)
                 (integer t)
                 (string t)))
         (style (if prim "\"filled, rounded\"" "filled"))
         (color "chartreuse")
         (style (format nil "shape=box, style=~A, fillcolor=~A" style color)))
    (dot-node (dot-name gnode) (format-symbol (gnode-cons gnode)) rootp style))
  (loop for arg-dot in (mapcar #'(lambda (x) (to-dot x nil)) (gnode-args gnode))
        for index = 1 then (1+ index)
        do (dot-edge (dot-name gnode) arg-dot (format nil "~D." index))))

(defmethod to-dot ((gnode var-gnode) rootp)
  (dot-node (dot-name gnode) (format-symbol (gnode-var gnode)) rootp "shape=box, fillcolor=lightblue"))

(defmethod to-dot ((gnode apply-gnode) rootp)
  (dot-node (dot-name gnode) "" rootp "shape=circle, ordering=out, fixedsize=true, width=.25")
  (dot-edge (dot-name gnode) (to-dot (gnode-fun gnode) nil) "f")
  (loop for arg-dot in (mapcar #'(lambda (x) (to-dot x nil)) (gnode-args gnode))
        for index = 1 then (1+ index)
        do (dot-edge (dot-name gnode) arg-dot (format nil "~D." index))))

(defmethod to-dot ((gnode fun-gnode) rootp)
  (dot-node (dot-name gnode) (format-symbol (gnode-fun-name gnode)) rootp "shape=box, fillcolor=yellow")
  (loop for arg-dot in (mapcar #'(lambda (x) (to-dot x nil)) (gnode-args gnode))
        for index = 1 then (1+ index)
        do (dot-edge (dot-name gnode) arg-dot (format nil "~D." index))))

(defun dot-from-graph (gref &optional (stream *standard-output*))
  (let ((*dot-mapping* (make-hash-table))
        (*dot-stream* stream))
    (format *dot-stream* "digraph G{")
    (format *dot-stream* "~&graph[size=\"3,3\", bgcolor=\"transparent\"]")
    (format *dot-stream* "~&node[style=filled]")
    (to-dot gref t)
    (format *dot-stream* "~&}")))

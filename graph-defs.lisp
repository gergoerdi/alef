(defclass dotable ()
  ((dot-name :initform (gensym "V") :reader dot-name))
  (:documentation "Something that can be turned into a GraphViz node"))

(defclass gnode (dotable)
  ()
  (:documentation "Node in the graph representation"))

(defclass gref (dotable)
  ((node :initarg :node :accessor gderef))
  (:documentation "Pointer in the graph representation"))

(defun make-gref (gnode)
  (make-instance 'gref :node gnode))

(defclass cons-gnode (gnode)
  ((cons :initarg :cons :reader gnode-cons)
   (args :initarg :args :initform nil :accessor gnode-args)))

(defclass param-gnode (gnode)
  ((var :initarg :var :reader gnode-var)))

(defclass apply-gnode (gnode)
  ((fun :initarg :fun :accessor gnode-fun)
   (args :initarg :args :accessor gnode-args)))

(defclass fun-gnode (gnode)
  ((fun-name :initarg :fun-name :reader gnode-fun-name)
   (arity :initarg :arity :reader gnode-fun-arity)
   (args :initarg :args :initform (list) :accessor gnode-args)))

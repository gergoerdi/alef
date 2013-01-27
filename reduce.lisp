(defvar *functions*)

(defgeneric reduce-graph-node (gnode))

(defun reduce-graph (gref)
  (let ((new-gnode (reduce-graph-node (gderef gref))))
    (when new-gnode
      (setf (gderef gref) new-gnode)
      t)))

(defun reduce-graph* (gref)
  (handler-case (reduce-graph gref)
    (need-reduce (req)
      (when (reduce-graph* (need-reduce-gref req))
        (reduce-graph* gref)))))

(defmethod reduce-graph-node ((gnode cons-gnode))
  nil)

(defmethod reduce-graph-node ((gnode var-gnode))
  ;; (error "Internal error: variable reference in head")
  nil)

(defmethod reduce-graph-node ((fun fun-gnode))
  (assert (= (length (gnode-args fun)) (gnode-fun-arity fun)))
  (error "fun-gnode"))

(defgeneric gnode-add-arg (gnode arg))

(defmethod gnode-add-arg ((gnode cons-gnode) arg)
  (make-instance 'cons-gnode
                 :cons (gnode-cons gnode)
                 :args (append (gnode-args gnode) (list arg))))

(defmethod gnode-add-arg ((gnode fun-gnode) arg)
  (make-instance 'fun-gnode
                 :fun-name (gnode-fun-name gnode)
                 :arity (gnode-fun-arity gnode)
                 :args (append (gnode-args gnode) (list arg))))

(defgeneric reduce-app (gnode arg))

(defmethod reduce-app ((gnode gnode) arg)
  nil)

(defmethod reduce-app ((fun fun-gnode) arg)
  (when (< (length (gnode-args fun)) (gnode-fun-arity fun))
    (gnode-add-arg fun arg)))

(defmethod reduce-app ((cons cons-gnode) arg)
  (gnode-add-arg cons arg))

(defmethod reduce-graph-node ((app apply-gnode))
  (let ((fun (gnode-fun app)))
    (or (reduce-app (gderef fun) (gnode-arg app))
        (error 'need-reduce :gref fun))))


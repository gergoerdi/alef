(defgeneric gnode-force-cons (gnode gref))

(defmethod gnode-force-cons ((gnode cons-gnode) gref)
  (gnode-cons gnode))

(defmethod gnode-force-cons ((gnode gnode) gref)
  (error 'need-reduce :gref gref))

(defgeneric reduce-function (fun-info args))

(defmethod reduce-function ((fun-info prim-function-info) arg-grefs)
  (let ((args (loop for arg-gref in arg-grefs
                    collect (gnode-force-cons (gderef arg-gref) arg-gref))))
    (make-instance 'cons-gnode
                   :cons (apply (prim-function fun-info) args))))

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

(defun reduce-to-whnf (gref)
  (when (reduce-graph* gref)
    (reduce-to-whnf gref)))

(defmethod reduce-graph-node ((gnode cons-gnode))
  nil)

(defmethod reduce-graph-node ((gnode var-gnode))
  ;; (error "Internal error: variable reference in head")
  nil)

(defmethod reduce-graph-node ((fun fun-gnode))
  (assert (= (length (gnode-args fun)) (gnode-fun-arity fun)))
  (let ((fun-info (lookup-function (gnode-fun-name fun))))
    (reduce-function fun-info (gnode-args fun))))

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


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

(defmethod reduce-graph-node ((app apply-gnode))
  (let ((fun (gderef (gnode-fun app))))
    (typecase fun
      (fun-gnode
       (if (< (length (gnode-args fun)) (gnode-fun-arity fun))
           (gnode-add-arg fun (gnode-arg app))
           (error 'need-reduce :gref (gnode-fun app))))
      (cons-gnode
       (gnode-add-arg fun (gnode-arg app)))
      (var-gnode
       (error "var-gnode"))
      (t
       (error 'need-reduce :gref (gnode-fun app))))))


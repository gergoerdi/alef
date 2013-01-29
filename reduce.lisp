(defvar *functions*)

(defclass function-info ()
  ())

(defclass prim-function-info (function-info)
  ((arity :initarg :arity :reader function-arity)
   (fun :initarg :fun :reader prim-function)))

(defgeneric reduce-function (fun-info args))

(defmethod reduce-function ((fun-info prim-function-info) args)
  (make-instance 'cons-gnode
                 :cons (apply (prim-function fun-info)
                              (mapcar #'gnode-cons (mapcar #'gderef args)))))

(defclass match-function-info (function-info)
  ((matches :initarg :matches :reader function-matches)))

(defmethod function-arity ((function-info match-function-info))
  (length (caar matches)))

(defmethod reduce-function ((fun-info match-function-info) args)
  (format nil "~&~A~&" args)
  (loop for (pats . skeleton) in (function-matches fun-info)
        do (handler-case
               (return (gderef (deepclone-gref skeleton (mapcan #'match-pattern pats args))))
             (no-match ()))
        finally (error 'no-match)))

(defun lookup-function (fun-name)
  (gethash fun-name *functions*))

(defun register-match-function (fun-name matches)
  (setf (gethash fun-name *functions*)
        (make-instance 'match-function-info :matches matches)))

(defun register-prim-function (fun-name arity fun)
  (setf (gethash fun-name *functions*)
        (make-instance 'prim-function-info :arity arity :fun fun)))

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


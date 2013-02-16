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

(defmethod reduce-function ((fun-info match-function-info) args)
  (loop for (pats . body) in (function-matches fun-info)
        do (handler-case (return (gderef (deepclone-gref body (mapcan #'match-pattern pats args))))
             (no-match ()))
        finally (error 'no-match)))

(defgeneric reduce-graph-node (gnode))

(defun reduce-graph (gref)
  (let ((new-gnode (reduce-graph-node (gderef gref))))
    (when new-gnode
      (setf (gderef gref) new-gnode)
      t)))

(defun reduce-graph* (gref)
  (prog1
      (handler-case (reduce-graph gref)
        (need-reduce (req)
          (when (reduce-graph* (need-reduce-gref req))
            (reduce-graph* gref))))
    (simplify-apps gref)))

(defun reduce-to-whnf (gref)
  (when (reduce-graph* gref)
    (reduce-to-whnf gref)))

(defmethod reduce-graph-node ((gnode cons-gnode))
  nil)

(defmethod reduce-graph-node ((gnode bottom-gnode))
  nil)

(defmethod reduce-graph-node ((gnode var-gnode))
  (error "Internal error: variable reference in head"))

(defun split-at (lst i)
  (labels ((aux (lst i before)
             (cond
               ((null lst) (values (reverse before) nil (= i 0)))
               ((= i 0)    (values (reverse before) lst t))
               (t          (aux (cdr lst) (1- i) (cons (car lst) before))))))
    (aux lst i nil)))

(defmethod reduce-graph-node ((fun fun-gnode))
  (multiple-value-bind (actuals remaining saturated) (split-at (gnode-args fun) (gnode-fun-arity fun))
    (when saturated
      (let* ((fun-info (lookup-function (gnode-fun-name fun)))
             (result-gnode (reduce-function fun-info actuals)))
        (if (null remaining) result-gnode
            (make-instance 'apply-gnode :fun (make-gref result-gnode) :args remaining))))))

(defgeneric gnode-add-args (gnode args))

(defmethod gnode-add-args ((gnode gnode) args)
  nil)

(defmethod gnode-add-args ((gnode cons-gnode) args)
  (make-instance 'cons-gnode
                 :cons (gnode-cons gnode)
                 :args (append (gnode-args gnode) args)))

(defmethod gnode-add-args ((gnode fun-gnode) args)
  (make-instance 'fun-gnode
                 :fun-name (gnode-fun-name gnode)
                 :arity (gnode-fun-arity gnode)
                 :args (append (gnode-args gnode) args)))

(defmethod gnode-add-args ((gnode apply-gnode) args)
  (make-instance 'apply-gnode
                 :fun (gnode-fun gnode)
                 :args (append (gnode-args gnode) args)))

(defmethod reduce-graph-node ((app apply-gnode))
  (let ((fun (gnode-fun app)))
    (or (gnode-add-args (gderef fun) (gnode-args app))
        (error 'need-reduce :gref fun))))

(defgeneric simplify-app (gnode))

(defmethod simplify-app ((gnode gnode))
  nil)

(defmethod simplify-app ((app apply-gnode))
  (mapcar #'simplify-app-gref (cons (gnode-fun app) (gnode-args app)))
  (gnode-add-args (gderef (gnode-fun app)) (gnode-args app)))

(defvar *grefs*)
  
(defun simplify-app-gref (gref)
  (unless (gethash gref *grefs*)
    (setf (gethash gref *grefs*) t)
    (let ((new-gnode (simplify-app (gderef gref))))
      (when new-gnode
        (setf (gderef gref) new-gnode)))))

(defun simplify-apps (gref)
  (let ((*grefs* (make-hash-table)))
    (simplify-app-gref gref)))

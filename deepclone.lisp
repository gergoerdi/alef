(defgeneric deepclone-gnode (gnode vars mapping))

(defmethod deepclone-gnode ((gnode cons-gnode) vars mapping)
  (make-instance 'cons-gnode
                 :cons (gnode-cons gnode)
                 :args (loop for arg in (gnode-args gnode)
                             collect (deepclone-gref arg vars mapping))))

(defmethod deepclone-gnode ((gnode param-gnode) vars mapping)
  gnode)

(defmethod deepclone-gnode ((gnode apply-gnode) vars mapping)
  (make-instance 'apply-gnode
                 :fun (deepclone-gref (gnode-fun gnode) vars mapping)
                 :args (loop for arg in (gnode-args gnode)
                             collect (deepclone-gref arg vars mapping))))

(defmethod deepclone-gnode ((gnode fun-gnode) vars mapping)
  (make-instance 'fun-gnode
                 :fun-name (gnode-fun-name gnode)
                 :arity (gnode-fun-arity gnode)
                 :args (loop for arg in (gnode-args gnode)
                             collect (deepclone-gref arg vars mapping))))

(defun deepclone-gref (gref &optional vars (mapping (make-hash-table)))
  (or
   (and (typep (gderef gref) 'param-gnode)
        (cdr (assoc (gnode-var (gderef gref)) vars)))
   (gethash gref mapping)
   (let ((gref/copy (make-instance 'gref)))
     (setf (gethash gref mapping) gref/copy)
     (setf (gderef gref/copy) (deepclone-gnode (gderef gref) vars mapping))
     gref/copy)))

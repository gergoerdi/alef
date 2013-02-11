(defgeneric deepclone-gnode (gnode f))

(defmethod deepclone-gnode ((gnode cons-gnode) f)
  (make-instance 'cons-gnode
                 :cons (gnode-cons gnode)
                 :args (mapcar f (gnode-args gnode))))

(defmethod deepclone-gnode ((gnode var-gnode) f)
  gnode)

(defmethod deepclone-gnode ((gnode apply-gnode) f)
  (make-instance 'apply-gnode
                 :fun (funcall f (gnode-fun gnode))
                 :args (mapcar f (gnode-args gnode))))

(defmethod deepclone-gnode ((gnode fun-gnode) f)
  (make-instance 'fun-gnode
                 :fun-name (gnode-fun-name gnode)
                 :arity (gnode-fun-arity gnode)
                 :args (mapcar f (gnode-args gnode))))

(defun deepclone-gref (gref &optional vars mapping)
  (let ((dict (make-hash-table)))
    (when mapping
      (loop for (k . v) in mapping
            do (setf (gethash k dict) v)))
    (labels ((deepclone (gref)
               (or
                (and (typep (gderef gref) 'var-gnode)
                     (cdr (assoc (gnode-var (gderef gref)) vars)))
                (gethash gref dict)
                   (let ((gref/copy (make-instance 'gref)))
                     (setf (gethash gref dict) gref/copy)
                     (setf (gderef gref/copy) (deepclone-gnode (gderef gref) #'deepclone))
                     gref/copy))))
      (deepclone gref))))

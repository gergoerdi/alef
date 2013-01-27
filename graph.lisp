(defclass gnode ()
  ()
  (:documentation "Node in the graph representation"))

(defclass gref ()
  ((node :initarg :node :accessor gderef))
  (:documentation "Pointer in the graph representation"))

(defun make-gref (gnode)
  (make-instance 'gref :node gnode))

(defclass cons-gnode (gnode)
  ((cons :initarg :cons :reader gnode-cons)
   (args :initarg :args :initform nil :reader gnode-args)))

(defclass var-gnode (gnode)
  ((var :initarg :var :reader gnode-var)))

(defclass apply-gnode (gnode)
  ((fun :initarg :fun :reader gnode-fun)
   (arg :initarg :arg :reader gnode-arg)))

(defclass fun-gnode (gnode)
  ((fun-name :initarg :fun-name :reader gnode-fun-name)
   (arity :initarg :arity :reader gnode-fun-arity)
   (args :initarg :args :initform (list) :reader gnode-args)))

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
                 :arg (funcall f (gnode-arg gnode))))

(defmethod deepclone-gnode ((gnode fun-gnode) f)
  (make-instance 'fun-gnode
                 :fun-name (gnode-fun-name gnode)
                 :arity (gnode-fun-arity gnode)
                 :args (mapcar f (gnode-args gnode))))

(defun deepclone-gref (gref &optional mapping)
  (let ((dict (make-hash-table)))
    (when mapping
      (loop for (k . v) in mapping
            do (setf (gethash k dict) v)))
    (labels ((deepclone (gref)
               (or (gethash gref dict)
                   (let ((gref/copy (make-instance 'gref)))
                     (setf (gethash gref dict) gref/copy)
                     (setf (gderef gref/copy) (deepclone-gnode (gderef gref) #'deepclone))
                     gref/copy))))
      (deepclone gref))))

(defgeneric graph-from-expr (expr vars))

(defmethod graph-from-expr ((expr var-expr) vars)
  (let ((var (expr-symbol expr)))
    (or (cdr (assoc var vars))
        (error "Variable not in scope: ~s" var))))

(defmethod graph-from-expr ((expr cons-expr) vars)
  (make-gref (make-instance 'cons-gnode :cons (expr-symbol expr))))

(defmethod graph-from-expr ((expr apply-expr) vars)
  (let ((fun (graph-from-expr (expr-fun expr) vars))
        (arg (graph-from-expr (expr-arg expr) vars)))
    (make-gref (make-instance 'apply-gnode :fun fun :arg arg))))

(defmethod graph-from-expr ((expr let-expr) vars)
  (let* ((bindings (loop for (name . val) in (expr-bindings expr)
                         collect (list name val (make-instance 'gref))))
         (new-vars (append (loop for (name val ref) in bindings collect (cons name ref)) vars)))
    (loop for (name val ref) in bindings
          do (setf (gderef ref) (gderef (graph-from-expr val new-vars))))
    (graph-from-expr (expr-body expr) new-vars)))

(defmethod graph-from-expr ((expr lambda-expr) vars)
  (let* ((new-vars (loop for name in (mapcan #'pattern-vars (expr-formals expr))
                         for var = (make-gref (make-instance 'var-gnode :var name))
                         collect (cons name var))))
    (graph-from-expr (expr-body expr) (append new-vars vars))))

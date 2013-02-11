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
   (args :initarg :args :reader gnode-args)))

(defclass fun-gnode (gnode)
  ((fun-name :initarg :fun-name :reader gnode-fun-name)
   (arity :initarg :arity :reader gnode-fun-arity)
   (args :initarg :args :initform (list) :reader gnode-args)))
   (args :initarg :args :initform (list) :accessor gnode-args)))

(defgeneric graph-from-expr (expr))

(defvar *vars*)

(defclass var-hole ()
  ())

(defclass built-var (var-hole)
  ((gref :initarg :gref :reader var-gref)))

(defclass var-blueprint (var-hole)
  ((name :initarg :name :reader var-name)
   (expr :initarg :expr :reader var-expr)))

(defgeneric graph-from-var (var))

(defmethod graph-from-var ((var var-blueprint))
  (let ((gref (make-gref (make-instance 'var-gnode :var (var-name var)))))
    (setf (gethash (var-name var) *vars*) (make-instance 'built-var :gref gref))
    (let ((gref-body (graph-from-expr (var-expr var))))
      (setf (gderef gref) (gderef gref-body))
      gref-body)))

(defmethod graph-from-var ((var built-var))
  (var-gref var))

(defmethod graph-from-expr ((expr var-expr))
  (let* ((var-name (expr-symbol expr))
         (var (gethash var-name *vars*)))
    (if var (graph-from-var var)
        (make-gref
         (or (let ((fun (lookup-function var-name)))
               (and fun (make-instance 'fun-gnode :fun-name var-name :arity (function-arity fun))))
             (make-instance 'var-gnode :var var))))))

(defmethod graph-from-expr ((expr cons-expr))
  (make-gref (make-instance 'cons-gnode :cons (expr-symbol expr))))

(defmethod graph-from-expr ((expr apply-expr))
  (let ((fun (graph-from-expr (expr-fun expr)))
        (arg (graph-from-expr (expr-arg expr) )))
    (make-gref (make-instance 'apply-gnode :fun fun :args (list arg)))))

(defmethod graph-from-expr ((expr let-expr))
  (let ((vars-new (make-hash-table)))
    (loop for k being the hash-key using (hash-value v) of *vars*
          do (setf (gethash k vars-new) v))
    (let ((*vars* vars-new))
      (loop for (name . val) in (expr-bindings expr)
            do (setf (gethash name *vars*)
                     (make-instance 'var-blueprint :name name :expr val)))
      (graph-from-expr (expr-body expr)))))

(defmethod graph-from-expr ((expr lambda-expr))
  (error "Not implemented: lambdas")
  ;; (let* ((new-vars (loop for name in (mapcan #'pattern-vars (expr-formals expr))
  ;;                        for var = (make-gref (make-instance 'var-gnode :var name))
  ;;                        collect (cons name var))))
  ;;   (error "Not implemented: lambda lifting")
  ;;   (graph-from-expr (expr-body expr) (append new-vars vars)))
  )

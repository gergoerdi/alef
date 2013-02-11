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
   (args :initarg :args :initform nil :accessor gnode-args)))

(defclass var-gnode (gnode)
  ((var :initarg :var :reader gnode-var)))

(defclass apply-gnode (gnode)
  ((fun :initarg :fun :accessor gnode-fun)
   (args :initarg :args :accessor gnode-args)))

(defclass fun-gnode (gnode)
  ((fun-name :initarg :fun-name :reader gnode-fun-name)
   (arity :initarg :arity :reader gnode-fun-arity)
   (args :initarg :args :initform (list) :accessor gnode-args)))

(defgeneric graph-from-expr (expr))

(defmethod graph-from-expr ((expr var-expr))
  (let* ((var-name (expr-symbol expr))
         (var-expr (cdr (assoc var-name *vars*))))
    (if var-expr (graph-from-var var-name var-expr)   
        (make-gref
         (or (let ((fun (lookup-function var-name)))
               (and fun (make-instance 'fun-gnode :fun-name var-name :arity (function-arity fun))))
             (make-instance 'var-gnode :var var-name))))))

(defmethod graph-from-expr ((expr cons-expr))
  (make-gref (make-instance 'cons-gnode :cons (expr-symbol expr))))

(defmethod graph-from-expr ((expr apply-expr))
  (make-gref
   (let ((fun (graph-from-expr (expr-fun expr)))
         (arg (graph-from-expr (expr-arg expr) )))
     (make-instance 'apply-gnode :fun fun :args (list arg)))))

(defmethod graph-from-expr ((expr let-expr))
  (error "Not implemented: let")
  ;; (let ((vars-new (make-hash-table)))
  ;;   (loop for k being the hash-key using (hash-value v) of *vars*
  ;;         do (setf (gethash k vars-new) v))
  ;;   (let ((*vars* vars-new))
  ;;     (loop for (name . val) in (expr-bindings expr)
  ;;           do (setf (gethash name *vars*)
  ;;                    (make-instance 'var-blueprint :name name :expr val)))
  ;;     (graph-from-expr (expr-body expr))))
  )

(defmethod node-from-expr ((expr lambda-expr))
  (error "Not implemented: lambdas")
  ;; (let* ((new-vars (loop for name in (mapcan #'pattern-vars (expr-formals expr))
  ;;                        for var = (make-gref (make-instance 'var-gnode :var name))
  ;;                        collect (cons name var))))
  ;;   (error "Not implemented: lambda lifting")
  ;;   (graph-from-expr (expr-body expr) (append new-vars vars)))
  )

(defvar *vars*)
(defvar *vars-built*)

(defclass bottom-gnode (gnode)
  ((ptr :initarg :ptr :reader gnode-var-ptr)))

(defun graph-from-var (var-name var-expr)
  (or (cadr (assoc var-name *vars-built*))
      (let* ((gref (make-instance 'gref))
             (gref-ptr (list gref)))
        (setf (gderef gref) (make-instance 'bottom-gnode :ptr gref-ptr))
        (push (cons var-name gref-ptr) *vars-built*)
        (let ((gref* (graph-from-expr var-expr)))
          (rplaca gref-ptr gref*)
          gref*))))

(defvar *filled*)

(defun fill-var-gref (gref)
  (let ((*filled* (list)))
    (fill-var-gref* gref)))

(defgeneric fill-var-gref** (gref gnode))

(defmethod fill-var-gref** (gref (gnode bottom-gnode))
  (let ((gref* (var-gref (gnode-var-def gnode))))    
    (fill-var-gref* gref*)))

(defmethod fill-var-gref** (gref gnode)
  (fill-var-gnode (gderef gref))
  gref)

(defun fill-var-gref* (gref)
  (or (cdr (assoc gref *filled*))
      (progn
        (push (cons gref gref) *filled*)                  
        (let ((gref* (fill-var-gref** gref (gderef gref))))
          (push (cons gref gref*) *filled*)       
          gref*))))

(defgeneric fill-var-gnode (gnode))

(defmethod fill-var-gnode ((gnode gnode))
  (values))

(defmethod fill-var-gnode ((gnode cons-gnode))
  (setf (gnode-args gnode) (mapcar #'fill-var-gref* (gnode-args gnode)))
  (values))

(defmethod fill-var-gnode ((gnode apply-gnode))
  (setf (gnode-fun gnode) (fill-var-gref* (gnode-fun gnode)))
  (setf (gnode-args gnode) (mapcar #'fill-var-gref* (gnode-args gnode)))
  (values))


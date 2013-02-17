(defgeneric graph-from-expr (expr))

(defmethod graph-from-expr ((expr var-expr))
  (let* ((var-name (expr-symbol expr))
         (var-data (cdr (assoc var-name *vars*))))
    (if var-data (graph-from-var var-name var-data)
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
  (let ((*vars* *vars*))
    (add-vars (expr-bindings expr))
    ;; (fill-var-gref (graph-from-expr (expr-body expr)))
    (graph-from-expr (expr-body expr))))

(defmethod graph-from-expr ((expr lambda-expr))
  (error "Not implemented: lambdas"))

(defvar *vars-built*)

(defclass bottom-gnode (gnode)
  ((var :initarg :var :reader gnode-var)
   (ptr :initarg :ptr :reader gnode-var-ptr)))

(defun graph-from-var (var-name var-data)
  (or (car (second var-data))
      (let* ((gref (make-instance 'gref))
             (gref-ptr (list gref)))
        (setf (gderef gref) (make-instance 'bottom-gnode :var var-name :ptr gref-ptr))
        (setf (second var-data) gref-ptr)
        (let ((gref* (graph-from-expr (first var-data))))
          (rplaca gref-ptr gref*)
          gref*))))

(defvar *filled*)

(defun fill-var-gref (gref)
  (let ((*filled* (list)))
    (fill-var-gref* gref)))

(defgeneric fill-var-gref** (gref gnode))

(defmethod fill-var-gref** (gref (gnode bottom-gnode))
  (let ((gref* (car (gnode-var-ptr gnode))))
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

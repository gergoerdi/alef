(defclass expr ()
  ()
  (:documentation "Expression"))

(defclass symbol-expr (expr)
  ((symbol :initarg :symbol :reader expr-symbol)))

(defclass var-expr (symbol-expr)
  ()
  (:documentation "Variable occurance"))

(defclass cons-expr (symbol-expr)
  ()
  (:documentation "Constructor"))

(defclass apply-expr (expr)
  ((fun :initarg :fun :reader expr-fun)
   (arg :initarg :arg :reader expr-arg)))

(defclass let-expr (expr)
  ((bindings :initarg :bindings :reader expr-bindings)
   (body :initarg :body :reader expr-body)))

(defclass lambda-expr (expr)
  ((formals :initarg :formals :reader expr-formals)
   (body :initarg :body :reader expr-body)))

(defclass pattern ()
  ()
  (:documentation "Pattern"))

(defclass wildcard-pattern (pattern)
  ()
  (:documentation "Wildcard pattern"))

(defclass var-pattern (pattern)
  ((symbol :initarg :symbol :reader pattern-symbol))
  (:documentation "Variable binder"))

(defclass cons-pattern (pattern)
  ((cons :initarg :cons :reader pattern-cons)
   (args :initarg :args :initform nil :reader pattern-args))
  (:documentation "Constructor pattern"))

(defgeneric pattern-vars (pat))

(defmethod pattern-vars ((pat wildcard-pattern))
  (list))

(defmethod pattern-vars ((pat var-pattern))
  (list (pattern-symbol pat)))

(defmethod pattern-vars ((pat cons-pattern))
  (mapcar #'pattern-vars (pattern-args pat)))

;; TODO
(defun constructorp (x)
  (typecase x
    (integer t)
    (atom
     (or (eq x 'pair)
         (eq x 'cons)
         (eq x 'nil)))))

(defun normalize-pat (pat)
  (typecase pat
    (atom
     (cond ((eq pat :wildcard) (make-instance 'wildcard-pattern))
           ((constructorp pat) (make-instance 'cons-pattern :cons pat))
           (t                  (make-instance 'var-pattern :symbol pat))))
    (cons
     (make-instance 'cons-pattern
                    :cons (first pat)
                    :args (mapcar #'normalize-pat (rest pat))))))

(defun normalize-expr (expr)
  (typecase expr
    (atom
     (make-instance (if (constructorp expr) 'cons-expr 'var-expr) :symbol expr))
    (cons
     (case (first expr)
       ((let)
        (destructuring-bind ((&rest bindings) body) (rest expr)
          (make-instance 'let-expr
                         :bindings (loop for (name val) in bindings
                                         collect (cons name (normalize-expr val)))
                         :body (normalize-expr body))))
       ((lambda)
        (destructuring-bind ((&rest patterns) body) (rest expr)
          (make-instance 'lambda-expr
                         :formals (mapcar #'normalize-pat patterns)
                         :body (normalize-expr body))))
       (otherwise
        (reduce (lambda (f x) (make-instance 'apply-expr :fun f :arg x))
                (mapcar #'normalize-expr expr)))))))

(defgeneric denormalize (x))

(defmethod denormalize ((expr symbol-expr))
  (expr-symbol expr))

(defmethod denormalize ((expr let-expr))
  `(let ,(loop for (name . val) in (expr-bindings expr) collect (list name (denormalize val)))
     ,(denormalize (expr-body expr))))

(defmethod denormalize ((expr apply-expr))
  (list (denormalize (expr-fun expr)) (denormalize (expr-arg expr))))

(defmethod denormalize ((expr lambda-expr))
  `(lambda ,(mapcar #'denormalize (expr-formals expr))
     ,(denormalize (expr-body expr))))

(defmethod denormalize ((pat var-pattern))
  (pattern-symbol pat))

(defmethod denormalize ((pat cons-pattern))
  (cons (pattern-cons pat) (mapcar #'denormalize (pattern-args pat))))

(defmethod denormalize ((pat wildcard-pattern))
  :wildcard)

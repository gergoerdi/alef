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
   (arg :initarg :arg :reader expr-arg))
  (:documentation "Function application"))

(defclass let-expr (expr)
  ((bindings :initarg :bindings :reader expr-bindings)
   (body :initarg :body :reader expr-body))
  (:documentation "Let"))

(defclass lambda-expr (expr)
  ((formals :initarg :formals :reader expr-formals)
   (body :initarg :body :reader expr-body))
  (:documentation "Lambda abstraction"))

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

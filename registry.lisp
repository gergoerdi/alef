(defvar *constructors*)

(defvar *functions*)

(defmacro in-fresh-context (&body body)
  `(let ((*constructors* (make-hash-table))
         (*functions* (make-hash-table))
         (*vars* (list))
         (*vars-built* (list)))
     (register-builtin-types)
     (register-prim-functions)
     ,@body))

(defun add-constructors (constructors)
  (loop for (cons-name . cons-type) in constructors
        do (setf (gethash cons-name *constructors*) cons-type)))

(defun constructorp (x)
  (typecase x
    (integer t)
    (string t)
    (atom
     (nth-value 1 (gethash x *constructors*)))))

(defclass function-info ()
  ())

(defclass proto-function-info (function-info)
  ((arity :initarg :arity :reader function-arity)))

(defclass prim-function-info (proto-function-info)
  ((fun :initarg :fun :reader prim-function)))

(defclass match-function-info (function-info)
  ((matches :initarg :matches :reader function-matches)))

(defmethod function-arity ((function-info match-function-info))
  (length (caar (function-matches function-info))))

(defun register-proto-function (fun-name arity)
  (setf (gethash fun-name *functions*)
        (make-instance 'proto-function-info :arity arity)))

(defun register-match-function (fun-name matches)
  (setf (gethash fun-name *functions*)
        (make-instance 'match-function-info :matches matches)))

(defun register-prim-function (fun-name arity fun)
  (setf (gethash fun-name *functions*)
        (make-instance 'prim-function-info :arity arity :fun fun)))

(defun lookup-function (fun-name)
  (format nil "~A" *functions*)
  (gethash fun-name *functions*))

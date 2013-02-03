(defun partition-by (f xs)
  (let ((table (make-hash-table)))
    (loop for x in xs
          for key = (funcall f x)
          do (push x (gethash key table)))
    (maphash (lambda (key group) (setf (gethash key table) (reverse group))) table)
    table))

(defmacro with-vars ((mapping-name vars) &body body)
  (let ((forwards (gensym "FORWARDS")))
    `(multiple-value-bind (,mapping-name ,forwards)
         (loop for (var-name . var-expr) in ,vars
               for var-gref = (make-gref (make-instance 'var-gnode :var var-name))
               collect (cons var-name var-gref) into mapping
               collect (cons var-gref var-expr) into forwards
               finally (return (values mapping forwards)))
       (prog1
           (progn ,@body)
         (loop for (var-gref . var-expr) in ,forwards
                do (setf (gderef var-gref) (gderef (graph-from-expr var-expr ,mapping-name))))))))

(defun parse-program (program)
  (let ((program-parts (partition-by #'first program)))

    (add-constructors
     (loop for (defdata type-name type-args . constructors) in (gethash 'defdata program-parts)
           append (loop for (cons-name . cons-args) in constructors
                        collect (cons cons-name type-name))))

    (let ((vars
           (with-vars (var-mapping (loop for (defvar var-name var-body) in (gethash 'defvar program-parts)
                                         collect (cons var-name (normalize-expr var-body))))
                          (let ((fun-defs
                    (loop for (deffun name . matches) in (gethash 'deffun program-parts)
                          do (register-proto-function name (length (caar matches)))
                          collect (cons name (loop for (pats expr) in matches
                                                   collect (cons (mapcar #'normalize-pat pats) (normalize-expr expr)))))))

               ;; Build graphs for functions
               (loop for (fun-name . matches) in fun-defs
                     for match-nodes = (loop for (pats . expr) in matches
                                             collect (cons pats (graph-from-expr expr var-mapping)))
                     do (register-match-function fun-name match-nodes)))

             var-mapping)))
      (let ((g (cdr (assoc 'main vars))))
        ;; (simplify-apps g)
        g))))

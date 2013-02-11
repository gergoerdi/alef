(defun partition-by (f xs)
  (let ((table (make-hash-table)))
    (loop for x in xs
          for key = (funcall f x)
          do (push x (gethash key table)))
    (maphash (lambda (key group) (setf (gethash key table) (reverse group))) table)
    table))

(defun parse-program (program)
  (let ((program-parts (partition-by #'first program)))

    (add-constructors
     (loop for (defdata type-name type-args . constructors) in (gethash 'defdata program-parts)
           append (loop for (cons-name . cons-args) in constructors
                        collect (cons cons-name type-name))))

    (loop for (defvar var-name var-body) in (gethash 'defvar program-parts)
          do (push
              (cons var-name
                    (make-instance 'var-todo
                                   :var var-name
                                   :expr (normalize-expr var-body)))
              *vars*))
           
    (let ((fun-defs
           (loop for (deffun name . matches) in (gethash 'deffun program-parts)
                 do (register-proto-function name (length (caar matches)))
                 collect (cons name (loop for (pats expr) in matches
                                          collect (cons (mapcar #'normalize-pat pats) (normalize-expr expr)))))))

      ;; Build graphs for functions
      (loop for (fun-name . matches) in fun-defs
            for match-nodes = (loop for (pats . expr) in matches
                                    collect (cons pats (graph-from-expr expr)))
            do (register-match-function fun-name match-nodes)))

    (let ((g (graph-from-var (cdr (assoc 'main *vars*)))))
      ;; (fill-var-gref g)
      g)
    ))

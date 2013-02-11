(defun register-builtin-types ()
  (add-constructors '((true . bool)
                      (false . bool))))

(defun register-prim-functions ()
  (labels
      ((pred (p)
         (lambda (&rest args)
           (if (apply p args) 'true 'false))))
   (register-prim-function '>= 2 (pred #'>=))
   (register-prim-function '+ 2 #'+)
   (register-prim-function '- 2 #'-)
   (register-prim-function '* 2 #'*)
   (register-prim-function '/ 2 #'/)
   (register-prim-function 'show-int 1 (lambda (x) (format nil "~D" x)))
   (register-prim-function 'string-append 2 (lambda (x y) (format nil "~A~A" x y)))))

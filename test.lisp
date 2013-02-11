(defparameter *prog*
  '((deffun if
     ((true then else) then)
     ((false then else) else))
    
        (defdata list (a) (nil) (cons a (list a)))
    (defvar sep ", ")

    (deffun show-list*
     ((show-item (cons x xs)) (string-append (string-append sep (show-item x)) (show-list* show-item xs)))
     ((show-item nil) "]"))
    
    (deffun show-list
     ((show-item (cons x xs))
      (string-append (string-append "[" (show-item x)) (show-list* show-item xs)))
     ((show-item nil)
      "[]"))

    (deffun take
     ((0 xs) nil)
     ((n nil) nil)
     ((n (cons x xs)) (cons x (take (- n 1) xs))))

    (deffun map
     ((f nil) nil)
     ((f (cons x xs)) (cons (f x) (map f xs))))
    
    (deffun from-to
     ((from to) (if (>= from to)
                    nil
                    (cons from (from-to (+ 1 from) to)))))

    (defvar nats (cons 0 (map (+ 1) nats)))
    
    (defvar ones (cons 1 ones))
    
    
    ;; (defvar main (show-list show-int (take 5 nats)))
    ;; (defvar main (take 5 nats))
    ;; (defvar main (from-to 0 10))

    (defvar zig (cons 1 zag))
    (defvar zag (cons 2 zig))

    (defvar main nats)
    ))

(setf *prog*
      '(
        (defdata wrap (a) (wrap a))

        (deffun unwrap
         (((wrap x)) x))

        (deffun id
         ((x) x))

        (defvar main ((unwrap (wrap (id +))) 1 2))))

(setf *prog*
      '((defdata list (a) (nil) (cons a (list a)))
        (defvar ones (cons 1 ones))
        (defvar main (let ((ones* (cons 1 ones*))
                           (ones** ones*))
                       ones**))

        ))

(setf *prog*
      '((defdata pair-3 (x y z) (pair-3 x y z))
        (defvar x1 (+ 1 2))
        (defvar x2 x1)
        (defvar x3 x2)
        (defvar x4 x3)
        (defvar x5 x4)
        (defvar x6 x5)
        (defvar x7 x8)
        (defvar x8 x9)
        (defvar x9 x7)
        (defvar main (pair-3 x2 x1 x7))))

(setf *prog*
      '((defdata pair (a b) (pair a b))
        (defdata list (a) nil (cons a (list a)))
        
        (defvar x (cons 1 y))
        (defvar y (cons 2 x))
        (defvar main (pair x y))))

(setf *prog*
      '((defdata pair (a) (cons a a))
        (defvar main (cons main main))))

(defun test ()
  (with-open-file (s "bar-z.dot" :direction :output :if-exists :supersede)
    (in-fresh-context
      (let ((g (parse-program *prog*)))
        (reduce-to-whnf g)
        ;; (simplify-apps g)
        (loop for g* in (gnode-args (gderef g))
              do (reduce-to-whnf g*))
        (loop for g* in (loop for g* in (gnode-args (gderef g))
                              append (gnode-args (gderef g*)))
              do (reduce-to-whnf g*))
        (dot-from-graph g s)       
        g))))


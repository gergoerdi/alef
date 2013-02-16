(defconstant *lib-bool*
  '((deffun if
     ((true then else) then)
     ((false then else) else))))

(defconstant *lib-string*
  '((deffun string-concat
     ((nil) "")
     (((cons s ss)) (string-append s (string-concat ss))))))

(defconstant *lib-list*
  '((defdata list (a) nil (cons a (list a)))

    (deffun map
     ((f nil) nil)
     ((f (cons x xs)) (cons (f x) (map f xs))))

    (deffun show-list*
     ((show-item (cons x xs))
      (string-concat (cons ", " (cons (show-item x) (cons (show-list* show-item xs) nil)))))
     ((show-item nil) "]"))

    (deffun show-list
     ((show-item (cons x xs))
      (string-concat
       (cons "[" (cons (show-item x) (cons (show-list* show-item xs) nil)))))
     ((show-item nil)
      "[]"))

    (deffun take
     ((0 xs) nil)
     ((n nil) nil)
     ((n (cons x xs)) (cons x (take (- n 1) xs))))

    (deffun zip
     (((cons x xs) (cons y ys))
      (cons (pair x y) (zip xs ys)))
     ((:wildcard :wildcard) nil))

    (deffun from-to
     ((from to) (if (>= from to)
                    nil
                    (cons from (from-to (+ 1 from) to)))))))

(defconstant *lib-pair*
  '((defdata pair (a b) (pair a b))
    (deffun show-pair
     ((show/1 show/2 (pair x y))
      (string-concat (cons "(" (cons (show/1 x) (cons ", " (cons (show/2 y) (cons ")" nil))))))))))

(defparameter *prog*
  `(,@*lib-bool*
    ,@*lib-string*
    ,@*lib-list*
    ,@*lib-pair*

    (defvar nats (cons 0 (map (+ 1) nats)))

    (defvar ones (cons 1 ones))

    (deffun zipnats
     ((xs) (zip nats xs)))

    (defvar main (show-list (show-pair show-int show-int) (zipnats (from-to 10 13))))
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
      '((defdata pair (x y) (pair x y))
        (defvar x1 (+ 1 2))
        (defvar x2 x1)
        (defvar x3 x2)
        (defvar x4 x3)
        (defvar x5 x4)
        (defvar x6 x5)
        (defvar x7 x8)
        (defvar x8 x9)
        (defvar x9 x7)
        (defvar main (pair (pair x2 x7) (pair x1 x9)))))

(setf *prog*
      '((defdata pair (a b) (pair a b))
        (defdata list (a) nil (cons a (list a)))

        (defvar x (cons 1 y))
        (defvar y (cons 2 x))
        (defvar main (pair x y))))

(setf *prog*
      '((defdata pair (a) (cons a a))
        (defvar main (cons main main))))

(setf *prog*
      '((deffun f
         ((x) (f x)))

        (defvar main (f 1))))

(setf *prog*
      '((defdata list (a) nil (cons a (list a)))
        (defvar main
          (let ((zig (cons 1 zag))
                (zag (cons 2 zig)))
            zig))))

(setf *prog*
      '((defvar main
          (let ((x 1)
                (y x))
            (+ (let ((x 3)
                     (y (+ 9 x)))
                 y)
               y)))))

(setf *prog*
      '((defvar main
          (let ((x 1))
            (+ x
               x)))))

(defun test ()
  (in-fresh-context
    (let ((g (parse-program *prog*)))
      (simplify-apps g)
      ;; (reduce-graph* g)
      ;; (reduce-to-whnf g)
      (with-open-file (s "test-1.dot" :direction :output :if-exists :supersede)
        (dot-from-graph g s))
      (reduce-graph* (nth 0 (gnode-args (gderef g))))
      (with-open-file (s "test-2.dot" :direction :output :if-exists :supersede)
        (dot-from-graph g s))      
      g)))

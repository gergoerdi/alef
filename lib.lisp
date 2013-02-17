(defconstant +lib-bool+
  '((deffun if
     ((true then else) then)
     ((false then else) else))))

(defconstant +lib-string+
  '((deffun string-concat
     ((nil) "")
     (((cons s ss)) (string-append s (string-concat ss))))))

(defconstant +lib-list+
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

(defconstant +lib-pair+
  '((defdata pair (a b) (pair a b))
    (deffun show-pair
     ((show/1 show/2 (pair x y))
      (string-concat (cons "(" (cons (show/1 x) (cons ", " (cons (show/2 y) (cons ")" nil))))))))))


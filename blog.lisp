(defun blog-1 ()
  (let ((p `(,@+lib-list+
             (defvar main
               (let ((primes (cons 2 (cons 3 (cons 5 nil)))))
                 (map (+ (+ 1 1)) primes))))))
    (in-fresh-context
      (let ((g (parse-program p)))
        (with-open-file (s "graph-01-app.dot" :direction :output :if-exists :supersede)
          (dot-from-graph g s))
        (simplify-apps g)
        (with-open-file (s "graph-01.dot" :direction :output :if-exists :supersede)
          (dot-from-graph g s))
        (reduce-to-whnf g)
        (with-open-file (s "graph-01-map-reduced.dot" :direction :output :if-exists :supersede)
          (dot-from-graph g s))
        g))))

(defun blog-2 ()
  (let ((p `(,@+lib-list+
             (defvar nats (cons 0 (map + 1) nats))
             (defvar main nats))))
    (in-fresh-context
     (let ((g (parse-program *prog*)))
       (simplify-apps g)
       (with-open-file (s "graph-02.dot" :direction :output :if-exists :supersede)
         (dot-from-graph g s))
       (reduce-to-whnf (nth 1 (gnode-args (gderef g))))
       (with-open-file (s "graph-02-tail.dot" :direction :output :if-exists :supersede)
         (dot-from-graph g s))
       g))))

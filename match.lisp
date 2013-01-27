(define-condition no-match ()
  ())

(define-condition need-reduce ()
  ((gref :initarg :gref :reader need-reduce-gref))
  (:report (lambda (condition stream)
             (format stream "Need to reduce ~A" (need-reduce-gref condition)))))

(defgeneric match-pattern (pat gref))

(defmethod match-pattern ((pat wildcard-pattern) gref)
  (list))

(defmethod match-pattern ((pat var-pattern) gref)
  (list (cons (pattern-symbol pat) gref)))

(defmethod match-pattern ((pat cons-pattern) gref)
  (labels
      ((match-head (gref)
         (typecase (gderef gref)
           (cons-gnode
            (unless (eq (gnode-cons (gderef gref)) (pattern-cons pat))
              (error 'no-match)))
           (t
            (error 'need-reduce :gref gref)))))
    (typecase (gderef gref)
      ;; Because of well-typedness, there is no need to check arity here
      (cons-gnode
       (match-head gref))
      (apply-gnode
       (match-head (gnode-fun (gderef gref)))
       (mapcan #'match-pattern (pattern-args pat) (gnode-args (gderef gref)))))))

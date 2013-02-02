(define-condition no-match ()
  ())

(define-condition need-reduce ()
  ((gref :initarg :gref :reader need-reduce-gref))
  (:report (lambda (condition stream)
             (format stream "Need to reduce ~A" (need-reduce-gref condition)))))

(defgeneric match-pattern* (pat gnode gref))

(defun match-pattern (pat gref)
  (match-pattern* pat (gderef gref) gref))

(defmethod match-pattern* ((pat wildcard-pattern) gnode gref)
  (list))

(defmethod match-pattern* ((pat var-pattern) gnode gref)
  (list (cons (pattern-symbol pat) gref)))

(defmethod match-pattern* ((pat cons-pattern) (gnode cons-gnode) gref)
  (unless (eq (pattern-cons pat) (gnode-cons gnode))
    (error 'no-match))
  ;; Because of well-typedness, there is no need to check arity here
  (mapcan #'match-pattern (pattern-args pat) (gnode-args gnode)))

(defmethod match-pattern* ((pat cons-pattern) gnode gref)
  (error 'need-reduce :gref gref))

(defun insert-funcalls (expr)
  (if (atom expr)
      expr
      (cons 'funcall
            (mapcar #'insert-funcalls expr))))

(defmacro define-combinator (name (&rest args) result)
  (let ((rest (gensym))
        (result (insert-funcalls result)))
    `(progn
       (defvar ,name)
       (setf ,name
         (lambda (&optional ,@args &rest ,rest)
           (cond ((null ,(car args)) ,name)
                 ,@(let ((index 0))
                     (mapcar (lambda (v)
                               `((null ,v)
                                 (lambda (&rest ,rest)
                                   (apply ,name ,@(subseq args 0 (incf index)) ,rest))))
                             (cdr args)))
                 ((null ,rest) ,result)
                 (t (apply ,result ,rest)))))
       (defmacro ,name (&rest ,rest)
         `(funcall ,,name ,@,rest))
       ',name)))

(define-combinator I (x)
  x)

(define-combinator K (x y)
  x)

(define-combinator S (x y z)
  (x z (y z)))

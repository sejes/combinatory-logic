(defun insert-funcalls (expr)
  (if (atom expr)
      expr
      (cons 'funcall
            (mapcar #'insert-funcalls expr))))

(defmacro define-combinator (name (&rest args) result)
  (let ((argsg (gensym))
        (result (insert-funcalls result)))
    `(progn
       (defvar ,name)
       (setf ,name
         (lambda (&optional ,@args &rest ,argsg)
           (cond ((null ,(car args)) ,name)
                 ,@(let ((index 0))
                     (mapcar (lambda (v)
                               `((null ,v)
                                 (lambda (&rest ,argsg)
                                   (apply ,name ,@(subseq args 0 (incf index)) ,argsg))))
                             (cdr args)))
                 ((null ,argsg) ,result)
                 (t (apply ,result ,argsg)))))
       (defmacro ,name (&rest ,argsg)
         `(funcall ,,name ,@,argsg))
       ',name)))

(define-combinator I (x)
  x)

(define-combinator K (x y)
  x)

(define-combinator S (x y z)
  (x z (y z)))

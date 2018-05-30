(in-package #:lbol-macros)
(in-readtable hash-table-literal)

(defclass handle ()
  ((resource :initarg :resource
             :initform nil)))

(defmethod dispose (obj)
  nil)

(defmethod dispose ((obj handle))
  (format t "~%DISPOSING: ~a"
          (slot-value obj 'resource))
  nil)

(defmacro using (var form &body body)
  (let ((var-name (gensym)))
    `(let* ((,var-name ,form)
            (,var ,var-name))
       (unwind-protect (progn ,@body)
         (dispose ,var-name)))))

(defun test (foo)
  (using x foo
    (print "lots of work happening")
    (setf x "a new thing")))

;;---------------------------------------------

(defun baz (x &optional (accum 1))
  (if (<= x 0)
      accum
      (baz (- x 1) (* accum x))))

#+nil
(labels ((this (x &optional (accum 1))
           (if (<= x 0)
               accum
               (baz (- x 1) (* accum x)))))
  #'this)

(defmacro rlambda (arg-names &body body)
  "recursive lambda"
  `(labels ((this ,arg-names
              ,@body))
     #'this))

;;---------------------------------------------

(defun test3 (key ht)
  (symbol-macrolet ((x (gethash key ht)))
    (list x x x)))

#+nil
(let ((#:g704 obj))
  (symbol-macrolet
      ((resource (slot-value #:g704 'resource)))
    (print resource)
    (setf resource "yay!")
    (print resource)
    obj))

(defun test5 (obj)
  (with-slots (resource) obj
    (print resource)
    (setf resource "yay!")
    (print resource)
    obj))

(defun test4 (key ht)
  (let ((x (gethash key ht)))
    (list x x x)))

;;---------------------------------------------

;; ;; Simple function to make and populate a hash-table
;; (defun gen-hash-table (data)
;;   (let ((ht (make-hash-table)))
;;     (loop
;;        :for (k v) :on data :by #'cddr
;;        :do (setf (gethash k ht) v))
;;     ht))

;; ;; This is the function that reads the syntax
;; ;; and emits the AST
;; (defun ht-reader (stream subchar arg)
;;   (declare (ignore subchar arg))
;;   (let* ((elements (read stream t nil t)))
;;     `(gen-hash-table (list ,@elements))))

;; ;; make a readtable which is just like the default
;; ;; readtable but with our hash-table syntax as well
;; (defreadtable hash-table-literal
;;   (:merge :standard)
;;   (:dispatch-macro-char #\# #\h #'ht-reader))

;; (defmethod print-object ((object hash-table) stream)
;;   (format stream "#H~s" (alexandria:hash-table-plist object)))

;;---------------------------------------------

(defun my-add (x y)
  (+ x y))

(define-compiler-macro my-add (x y)
  (if (and (numberp x)
           (numberp y))
      (+ x y)
      `(+ ,x ,y)))

;;---------------------------------------------

#+nil
(progn
  (defun foo (&rest args)
    (ecase (length args)
      (2 (destructuring-bind (x y) args
           (* x y)))
      (3 (destructuring-bind (x y z) args
           (- x y z)))))
  
  (defun foo/2 (x y)
    (* x y))
  
  (defun foo/3 (x y z)
    (- x y z))

  (define-compiler-macro foo (&rest args)
    (ecase (length args)
      (2 (cons 'foo/2 args))
      (3 (cons 'foo/3 args)))))

(defun afun-name (name arity)
  (symbolicate
   name
   :/
   (format nil "~a" arity)))

(defmacro define-afun (name &body clauses)
  `(progn
     (defun ,name (&rest args)
       (ecase (length args)
         ,@(loop
              :for (cargs . cbody) :in clauses
              :for clen := (length cargs)
              :collect
              `(,clen (destructuring-bind ,cargs args
                        ,@cbody)))))

     ,@(loop
          :for (cargs . cbody) :in clauses
          :for clen := (length cargs)
          :for fname := (afun-name name clen)
          :collect
          `(defun ,fname ,cargs
             ,@cbody))
     
     (define-compiler-macro ,name (&rest args)
       (ecase (length args)
         ,@(loop
              :for (cargs . cbody) :in clauses
              :for clen := (length cargs)
              :collect
              `(,clen (cons ',(afun-name name clen)
                            args)))))))
#+nil
(define-afun blah
  ((x y) (* x y))
  ((x y z) (- x y z)))

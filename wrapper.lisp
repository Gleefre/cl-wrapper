(defpackage #:wrapper
  (:use #:cl)
  (:export #:wrap-if
           #:wrap-if*
           #:wrap-if-not
           #:naive-wrap-if
           #:naive-wrap-if*
           #:naive-wrap-if-not)
  (:import-from #:alexandria
                #:ensure-list
                #:with-gensyms
                #:make-gensym-list))

(in-package #:wrapper)

;;; naive wrap-if
;;; Stupid way of wrapping might be the best one to avoid complicating things.
;;; Also it is useful in writing a harder version of wrap-if

(defun wrap (wrap-form body)
  `(,@(ensure-list wrap-form)
    (progn ,@body)))

(defmacro naive-wrap-if (test wrap-form &body body)
  "If TEST is not NIL wraps body into WRAP-FORM. Have an implicit progn. Code is duplicated."
  `(if ,test
       ,(wrap wrap-form body)
       (progn ,@body)))

(defmacro naive-wrap-if-not (test wrap-form &body body)
  "If TEST is NIL wraps body into WRAP-FORM. Have an implicit progn. Code is duplicated."
  `(if ,test
       (progn ,@body)
       ,(wrap wrap-form body)))

;;; More general wrap-if
;;; Uses flet and @tun

(defstruct (place (:constructor %make-place (getter setter)))
  getter
  setter)

(defun place-value (place)
  (funcall (place-getter place)))

(defun (setf place-value) (place value)
  (funcall (place-setter place) value))

(defun make-place (x)
  `(%make-place
    (lambda () ,x)
    (lambda (value) (setf ,x value))))

(defun func-definition (name transfer-vars body)
  (let ((lambda-list (make-gensym-list (length transfer-vars) "TRANSFER-VAR")))
    `(,name
      ,lambda-list
      (symbol-macrolet ,(mapcar (lambda (transfer-var flet-parameter)
                                  `(,transfer-var (place-value ,flet-parameter)))
                         transfer-vars
                         lambda-list)
        ,@body))))

(defun func-call (name transfer-vars)
  `(,name ,@(mapcar #'make-place transfer-vars)))

(defmacro wrap-if (transfer-vars test wrap-form &body body)
  "Like naive-wrap-if but avoid duplication using flet and symbol-macrolet.
Takes an extra parameter, which indicates which variables should be transferred to body function."
  (with-gensyms (func)
    (let ((call (func-call func transfer-vars))
          (func-definition (func-definition func transfer-vars body)))
      `(flet (,func-definition)
         (naive-wrap-if ,test ,wrap-form ,call)))))

(defmacro wrap-if-not (transfer-vars test wrap-form &body body)
  "Like naive-wrap-if-not but avoid duplication using flet and symbol-macrolet.
Takes an extra parameter, which indicates which variables should be transferred to body function."
  (with-gensyms (func)
    (let ((call (func-call func transfer-vars))
          (func-definition (func-definition func transfer-vars body)))
      `(flet (,func-definition)
         (naive-wrap-if-not ,test ,wrap-form ,call)))))

;;; Multiple (test form) pairs in one wrap

(defmacro naive-wrap-if* ((&rest wraps) &body body)
  "Like NAIVE-WRAP-IF but allows to specify multiple TEST-FORM pairs."
  (if (null wraps)
      `(progn ,@body)
      (progn
        `(naive-wrap-if ,(first (car wraps)) ,(second (car wraps))
           (naive-wrap-if* (,@(cdr wraps)) ,@body)))))

(defmacro wrap-if* (transfer-vars (&rest wraps) &body body)
  "Like WRAP-IF but allows to specify multiple TEST-FORM pairs."
  (if (null wraps)
      `(progn ,@body)
      (progn
        `(wrap-if ,transfer-vars ,(first (car wraps)) ,(second (car wraps))
           (wrap-if* ,transfer-vars (,@(cdr wraps)) ,@body)))))

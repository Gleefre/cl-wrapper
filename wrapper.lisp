(defpackage #:wrapper
  (:use #:cl)
  (:export #:wrap-if
           #:wrap-if*
           #:naive-wrap-if
           #:naive-wrap-if*)
  (:import-from #:alexandria
                #:ensure-list
                #:with-gensyms
                #:make-gensym-list)
  (:import-from #:captures
                #:flet/capture))

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

;;; More general, extensible wrap-if
;;; Uses flet/captures and is inspired by @tun

(defmacro wrap-if (test wrap-form &body body)
  "Like naive-wrap-if but avoid duplication using flet and symbol-macrolet.
Use CAPTURE declarations to transfer variables."
  (with-gensyms (func)
    `(flet/capture ((,func () ,@body))
       (naive-wrap-if ,test ,wrap-form (,func)))))

;;; Multiple (test form) pairs in one wrap

(defmacro naive-wrap-if* ((&rest wraps) &body body)
  "Like NAIVE-WRAP-IF but allows to specify multiple TEST-FORM pairs."
  (if (null wraps)
      `(progn ,@body)
      `(naive-wrap-if ,(first (car wraps)) ,(second (car wraps))
         (naive-wrap-if* (,@(cdr wraps)) ,@body))))

(defmacro wrap-if* ((&rest wraps) &body body)
  "Like WRAP-IF but allows to specify multiple TEST-FORM pairs."
  (if (null wraps)
      `(progn ,@body)
      (with-gensyms (func)
        `(flet/capture ((,func () ,@body))
           (naive-wrap-if* ,wraps (,func))))))

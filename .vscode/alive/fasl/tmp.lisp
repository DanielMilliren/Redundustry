;;;; Domain-specific language

;;; Technical sources:
;;; https://letoverlambda.com/index.cl/guest/chap2.html
;;; https://letoverlambda.com/index.cl/guest/chap3.html
;;; https://metaspec.dev

;;;; Macros

;;; defclosure
;;; defines flattened syntax for a basic closure.
;;; Usage:
;;; (defclosure
;;;     ((arg1 arg2 ...))
;;;     ((let1 let2 ...))
;;;     ((larg1 larg2 ...))
;;;     ((inner function body here))
;;; )

(defmacro defclosure 
    ;; See https://letoverlambda.com/index.cl/guest/chap2.html
    ;; on how a closure is structured in Common Lisp
    ;; and https://metaspec.dev/s_let.html
    ;; on why before_l isn't returned
    (name ((&optional &rest arg)) ((&optional &rest letvar))
    ((&optional &rest largs)) ; arguments split over new line for readability
    ((&body l)))
    (declare (inline))
    "Defines a simple, named closure.
    Arguments:
    arg         - list of closure arguments
    letvar      - letvar form
    largs       - list of lambda arguments
    prepare     - code to run before l, returns nothing.
    l           - lambda function form
    
    In the prepare body:
    Use (return-from before_l) to return with lambda early."
    `(defun ,name ,arg                  ; name and outer function args here
        (let (,@letvar)                 ; letvar, this can be whatever
            (lambda (,@largs) (,@l)); return lambda
        )
    )
)

;;; Define and export

(defpackage "redund-dsl"
    (:export "defclosure")
)
;;;; SeqDLM Implementation

;;; Based on
;;; https://dl.acm.org/doi/10.5555/3571885.3571958
;;; Credit to Qi Chen et al. for SeqDLM algorithm

;;; Technical sources:
;;; https://letoverlambda.com/index.cl/guest/chap2.html
;;; https://letoverlambda.com/index.cl/guest/chap3.html
;;; https://metaspec.dev

;;;; seqdlm_func.lisp Documentation

;;; See the link to Qi Chen et al. above for the original paper,
;;; but lock categories in short:

;;; "pw"  = used when machid wants to read from AND write to a file
;;; "bw"  = used when machid just wants to write to multiple files
;;; "nbw" = used when machid just wants to write to one file
;;; "pr"  = used when machid just wants to read from a file

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
    (name ((&optional &rest arg)) ((&optional &rest letvar))
    ((&optional &rest largs)) ((&rest l)))  ; arguments split over new line for readability
    (declare (inline))
    "Defines a simple, named closure.
    Arguments:
    arg - list of closure arguments
    letvar - letvar form
    largs - list of lambda arguments
    l - lambda function form"
    `(defun ,name ,arg                  ; name and outer function args here
        (let (,@letvar)               ; letvar, this can be whatever
            (lambda (,@largs) (,@l))
        )
    )
)

;;; seqdlmlcm returns a closure that has:
;;; a file lock (lock)
;;; a file state (state)
;;; writer ID (machID)
;;; lock request functionality

;;; usage for seqdlmlcm instance:
;;; (funcall alpha nlock nmachID)
;;; -> returns a list of parameters
;;;     - lock
;;;     - state
;;;     - machine id
;;;     - success/fail boolean

;;; The reason why the lambda returns args for
;;; a new seqdlmlcm is because that makes it
;;; usable for generating new lcm lambdas.

;;; valid locks, from most to least severe:
;;; "pw"   = protective write
;;; "bw"   = blocking write
;;; "nbw"  = nonblocking write
;;; "pr"   = protective read
;;; "none" = no lock

;;; valid states:
;;; "none"    = no state
;;; "granted" = lock granted
;;; "cancel"  = canceling state, waiting for lock release
;;; if state is "none," default behavior is to treat as granted.

(defclosure seqdlmlcm
    ((lock state machid))       ; arguments for the seqdlmlcm func
    (())                        ; let variable list
    ((nlock nmachid))    ; vars for the returned lambda function
    ((cond
        ((equal lock "pw")  ; protective write
            (values lock state machid nil)  ; protective write
        )
        ((equal lock "bw")                  ; blocking write
            (values lock state machid nil)
        )
        ((equal lock "nbw")                 ; non-blocking write
            (cond
                ((equal state "canceling")
                    (if 
                        (or (equal nlock "nbw") (equal nlock "bw"))
                        (values nlock state nmachid t)
                        (values lock state machid nil)
                    )
                )
                (t (values lock state machid nil))
            )
        )
        ((equal lock "pr")                  ; protective read
            (if (equal nlock "pr")  ; allow only protective reads to be granted
                (values nlock state nmachid t)
                (values lock state machid nil)
            )
        )
        ((equal lock "none")
                (values nlock state nmachid t)
        )
    ))
)

;;; TO DO:
;;; Add one more
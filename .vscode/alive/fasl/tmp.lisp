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
    ;; See https://letoverlambda.com/index.cl/guest/chap2.html
    ;; on how a closure is structured in Common Lisp
    ;; and https://metaspec.dev/s_let.html
    ;; on why before_l won't be returned
    (name ((&optional &rest arg)) ((&optional &rest letvar))
    ((&optional &rest largs)) &optional ((before_l)) ((&rest l)))  ; arguments split over new line for readability
    (declare (inline))
    "Defines a simple, named closure.
    Arguments:
    arg         - list of closure arguments
    letvar      - letvar form
    largs       - list of lambda arguments
    before_l    - code to run before l. Not returned, as let is an implicit progn.
    l           - lambda function form"
    `(defun ,name ,arg                  ; name and outer function args here
        (let (,@letvar)                 ; letvar, this can be whatever
            (,@before_l)                ; code to run before returning lambda
            (lambda (,@largs) (,@l))    ; return lambda
        )
    )
)

;;;; Functions

;;; seqdlmlcm returns a function that has:
;;; str  - a file lock (lock)
;;; str - a file state (state)
;;; int  - writer ID (machID)
;;; lock request functionality

;;; An seqdlmlcm instance are one of the components of a file lock.

;;; usage for seqdlmlcm instance:
;;; (funcall alpha nlock nmachID)
;;; -> returns a list of parameters
;;;     - lock
;;;     - state 
;;;     - machine id
;;;     - success/fail boolean
;;; if nlock = "none" or nil, then no change is made.

;;; The reason why the lambda returns args for
;;; a new seqdlmlcm is because that makes it
;;; usable for generating new lcm lambdas,
;;; and it allows me to transfer everything needed
;;; to create a new lock more easily.

;;; valid locks, from most to least severe:
;;; "pw"   = protective write
;;; "bw"   = blocking write
;;; "nbw"  = nonblocking write
;;; "pr"   = protective read
;;; "none" = no lock, equivalent to nil

;;; valid states:
;;; "none"    = no state, nil is treated as "none".
;;; "granted" = lock granted
;;; "cancel"  = canceling state, waiting for lock release
;;; if state is "none," default behavior is to treat as granted.

(defclosure seqdlmlcm
    ((lock state machid))       ; arguments for the seqdlmlcm func
    (())                        ; let variable list
    ((nlock &optional nmachid)) ; vars for the returned lambda function
    ((cond
        ((or (equal nlock nil) (equal nlock "none"))
            (values lock state machid nil)  ; no lock proposed gets values
        )
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
                        (values nlock "granted" nmachid t)
                        (values lock state machid nil)
                    )
                )
                (t (values lock state machid nil))
            )
        )
        ((equal lock "pr")                  ; protective read
            (if (equal nlock "pr")          ; allow only protective reads to be granted
                (values nlock "granted" nmachid t)
                (values lock state machid nil)
            )
        )
        (t  ; fail safe
            (values lock state machid nil)
        )
    ))
)

;;; filelock (struct, constructor make-filelock)

;;; lcmfunc     = lock compatibility matrix, function.
;;; lockid      = lock id, integer.
;;; seqnum      = sequence num, integer.

(defstruct filelock 
    (lcmfunc (seqdlmlcm "none" "none" 0) :type function)
    (lockid 0 :type int)
    (seqnum 0 :type int)
    (:conc-name lockget-)
    "Defines a file lock.
    (lockget-lcmfunc n) returns the lock compatibility matrix.
    (lockget-lockid n) returns the lockid.
    (lockget-seqnum n) returns the sequence number."
)


;;; TO DO:
;;; Write rest of SeqDLM's Functions
;;; Closures:
;;; - SeqDLM I/O Permissions
;;; - etc.
;;; Functions:
;;; - Lock Granter
;;; - Lock Revoker
;;; - I/O Handling

;;; newfilelock
;;; creates a new file lock from the necessary components and returns the file lock.
;;; lockid  = lock id, must be an integer.
;;; seqnum  = sequence number for most recent change, must be an integer.
;;; lock    = lock, see seqdlmlcm, must be a string.
;;; state   = state of lock, see seqdlm, must be a string.
;;; machid  = machine id, must be an integer.

(defun newfilelock (
        (lockid :type int)
        (seqnum :type int)
        (lock :type string)
        (state :type string)
        (machid :type int)
    )
    "Usage:
    (newfilelock lockid seqnum lock state machid)
    lockid is the ID of the lock, used for logging purposes.
    seqnum is used to order updates in lock grants and revocations.
    lock is a string that identifies the lock type.
    state is a string that identifies the lock state.
    machid is an integer that identifies the owner of the lock."
    (make-filelock (seqdlmlcm lock state machid) lockid seqnum)
)

;;; check_io
;;; put under file logic
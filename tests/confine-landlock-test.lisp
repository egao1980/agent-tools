(in-package #:agent-tools/tests)

;;; R4d-4a on Linux: real Landlock / bubblewrap (compute-backend-landlock, the
;;; best mechanism available). :read :all with the default deny-read needs a
;;; mount namespace (bwrap or the trampoline's); where there is none, or no
;;; Landlock at all (e.g. the CI container), the tests skip with the reason.

(define-real-confinement-tests landlock
  (stack-compute-landlock:make-landlock-backend))

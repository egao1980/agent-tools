(in-package #:agent-tools/tests)

;;; R4d-4a on macOS: real Seatbelt (compute-backend-seatbelt, sandbox-exec).
;;; :read :all, network :none, writable = the test root. Skips with the
;;; backend's reason when sandbox-exec cannot run here.

(define-real-confinement-tests seatbelt
  (stack-compute-seatbelt:make-seatbelt-backend))

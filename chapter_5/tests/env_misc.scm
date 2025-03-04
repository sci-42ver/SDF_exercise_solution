(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
;; trivially load just create manager-env and expose only manage-software (but not others) to system-global-environment.
(eq? (the-environment) exposed_env) ; #f
(eq? (the-environment) user-initial-environment) ; #t
(eq? (the-environment) (interaction-environment)) ; #t

;; see SDF_exercises/software/sdf/manager/software-manager.scm new-environment
;; for what env is exposed here.
(manage 'new 'generic-interpreter 'combining-arithmetics)

(eq? (the-environment) system-global-environment)
;Value: #f
(eq? (the-environment) user-initial-environment)
;Value: #f
; (eq? (the-environment) exposed_env)
; (eq? (the-environment) exposed_env_by_new_environment)

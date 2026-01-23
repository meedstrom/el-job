;;; el-job.el --- Contrived way to call a function using all CPU cores -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Author:           Martin Edström <meedstrom@runbox.eu>
;; URL:              https://github.com/meedstrom/el-job
;; Created:          2024-10-30
;; Keywords:         processes
;; Package-Version:  2.6.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Imagine you have a function you'd like to run on a long list of inputs.
;; You could run (mapcar #'FN INPUTS), but that hangs Emacs until done.

;; This library lets you split up the inputs and run the function in many
;; subprocesses---one per CPU core---then merge their outputs and handle the
;; result as if it had been returned by that `mapcar'.  In the meantime,
;; current Emacs does not hang at all.

;; You do need to grok the concept of a callback.

;; Public API:
;; - Function `el-job-old-launch' (main entry point)
;; - Function `el-job-old-await'
;; - Function `el-job-old-is-busy'
;; - Variable `el-job-old-major-version'

;; Dev tools:
;; - Command `el-job-old-cycle-debug-level'
;; - Command `el-job-old-show-info'
;; - Command `el-job-old-kill-all'

;;; Code:

(require 'el-job-ng)

(define-obsolete-variable-alias 'el-job-major-version     'el-job-old-major-version    "2.7.0 (2026-01-21)")
(define-obsolete-variable-alias 'el-job-max-cores         'el-job-old-max-cores        "2.7.0 (2026-01-21)")
(define-obsolete-variable-alias 'el-job--debug-level      'el-job-old--debug-level     "2.7.0 (2026-01-21)")
(define-obsolete-variable-alias 'el-job--onetime-canary   'el-job-old--onetime-canary  "2.7.0 (2026-01-21)")
(define-obsolete-variable-alias 'el-job--all-jobs         'el-job-old--all-jobs        "2.7.0 (2026-01-21)")

(require 'el-job-old)

(define-obsolete-function-alias 'el-job-launch                      #'el-job-old-launch                       "2.7.0 (2026-01-21)")
(define-obsolete-function-alias 'el-job-kill-all                    #'el-job-old-kill-all                     "2.7.0 (2026-01-21)")
(define-obsolete-function-alias 'el-job-await                       #'el-job-old-await                        "2.7.0 (2026-01-21)")
(define-obsolete-function-alias 'el-job-is-busy                     #'el-job-old-is-busy                      "2.7.0 (2026-01-21)")
(define-obsolete-function-alias 'el-job-cycle-debug-level           #'el-job-old-cycle-debug-level            "2.7.0 (2026-01-21)")
(define-obsolete-function-alias 'el-job-show-info                   #'el-job-old-show-info                    "2.7.0 (2026-01-21)")
(define-obsolete-function-alias 'el-job--dbg                        #'el-job-old--dbg                         "2.7.0 (2026-01-21)")
(define-obsolete-function-alias 'el-job--locate-lib-in-load-history #'el-job-old--locate-lib-in-load-history  "2.7.0 (2026-01-21)")
(define-obsolete-function-alias 'el-job--ensure-compiled-lib        #'el-job-old--ensure-compiled-lib         "2.7.0 (2026-01-21)")
(define-obsolete-function-alias 'el-job--split-evenly               #'el-job-old--split-evenly                "2.7.0 (2026-01-21)")
(define-obsolete-function-alias 'el-job--split-optimally            #'el-job-old--split-optimally             "2.7.0 (2026-01-21)")
(define-obsolete-function-alias 'el-job--zip-all                    #'el-job-old--zip-all                     "2.7.0 (2026-01-21)")
(define-obsolete-function-alias 'el-job--windows-cores              #'el-job-old--windows-cores               "2.7.0 (2026-01-21)")
(define-obsolete-function-alias 'el-job--with                       #'el-job-old--with                        "2.7.0 (2026-01-21)")
(define-obsolete-function-alias 'el-job--spawn-processes            #'el-job-old--spawn-processes             "2.7.0 (2026-01-21)")
(define-obsolete-function-alias 'el-job--exec-workload              #'el-job-old--exec-workload               "2.7.0 (2026-01-21)")
(define-obsolete-function-alias 'el-job--poll                       #'el-job-old--poll                        "2.7.0 (2026-01-21)")
(define-obsolete-function-alias 'el-job--reap                       #'el-job-old--reap                        "2.7.0 (2026-01-21)")
(define-obsolete-function-alias 'el-job--handle-output              #'el-job-old--handle-output               "2.7.0 (2026-01-21)")
(define-obsolete-function-alias 'el-job--disable                    #'el-job-old--disable                     "2.7.0 (2026-01-21)")
(define-obsolete-function-alias 'el-job--sit-until-not              #'el-job-old--sit-until-not               "2.7.0 (2026-01-21)")

(defvar el-jobs :obsolete)


;;;; Mapcar-like entry-point

(defvar el-job--synchronous-result nil)
(defun el-job-parallel-mapcar (fn list &optional inject-vars)
  "Apply FN to LIST like `mapcar' in one or more parallel processes.

Function FN must be known in `load-history' to be defined in some file.
The parallel processes inherit `load-path' and then load that file.

Function FN must not depend on side effects from previous invocations of
itself, because each process gets a different subset of LIST.

Unlike the more general `el-job-ng-run', this is meant as a close
drop-in for `mapcar'.  It behaves like a synchronous function by
blocking execution until the processes are done, then returns the
result to the caller.

Quitting kills the processes, much like quitting would interrupt a
synchronous function.

INJECT-VARS as in `el-job-ng-run'.

For convenience, INJECT-VARS can contain bare symbols instead of cons
cells, because it is processed by `el-job-ng-vars'.

N/B: A crucial difference from `mapcar' is the temporary loss of scope,
since FN runs in external processes.
That means FN will not see let-bindings, runtime variables and the like,
that you might have meant to have in effect where
`el-job-parallel-mapcar' is invoked.
Nor can it mutate such variables for you -- the only way it can affect
the current Emacs session is if the caller of
`el-job-parallel-mapcar' does something with the return value."
  (let* ((vars (el-job-ng-vars (cons '(el-job-ng--child-unary . t) inject-vars)))
         (id (intern (format "parallel-mapcar.%S.%d" fn (sxhash vars)))))
    (el-job-ng-run
     :id id
     :require (unless (subr-primitive-p (symbol-function fn)) ;; Emacs 28
                (list (symbol-file fn 'defun t)))
     :inject-vars vars
     :funcall-per-input fn
     :inputs list
     :callback (lambda (outputs)
                 (setq el-job--synchronous-result outputs)))
    (unless (el-job-ng-await-or-die id 86400)
      (error "el-job-ng-parallel-mapcar: Timed out (hung for 24 hours): %S" fn))
    el-job--synchronous-result))

(provide 'el-job)

;;; el-job.el ends here

;; Local Variables:
;; checkdoc-spellcheck-documentation-flag: nil
;; checkdoc-verb-check-experimental-flag: nil
;; emacs-lisp-docstring-fill-column: 72
;; End:

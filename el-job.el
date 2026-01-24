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
;; Package-Version:  2.7.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Imagine you have a function you'd like to run on a long list of inputs.
;; You could run (mapcar #'FN INPUTS), but that hangs Emacs until done.

;; This library lets you split up the inputs and run the function in many
;; subprocesses---one per CPU core---then merge their outputs and handle the
;; result as if it had been returned by that `mapcar'.  In the meantime,
;; current Emacs does not hang at all.

;; A high-level wrapper is `el-job-parallel-mapcar', which intentionally hangs
;; Emacs so as to behave as a drop-in for `mapcar' that is merely faster.

;; The more general `el-job-ng-run' can be used asynchronously.

;;; Code:

(defconst el-job-internal-version 104)

(require 'el-job-ng)

(define-obsolete-variable-alias 'el-job-max-cores 'el-job-old-max-cores  "2.7.0 (2026-01-23)")
(require 'el-job-old)
(define-obsolete-function-alias 'el-job-launch  #'el-job-old-launch      "2.7.0 (2026-01-23)")
(define-obsolete-function-alias 'el-job-await   #'el-job-old-await       "2.7.0 (2026-01-23)")
(define-obsolete-function-alias 'el-job-is-busy #'el-job-old-is-busy     "2.7.0 (2026-01-23)")

;; FIXME: It seems to print the nil message during work
;;;###autoload
(defun el-job-parallel-mapcar (fn list &optional inject-vars)
  "Apply FN to LIST like `mapcar' in one or more parallel processes.

Function FN must be known in `load-history' to be defined in some file.
At spin-up, the parallel processes inherit `load-path', then load that
file \(even if it is not on `load-path'\), and then get to work.

Function FN should not depend on side effects from previous invocations
of itself, because each process gets a different subset of LIST.

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
That is why you may need INJECT-VARS.

N/B: The aforementioned loss of scope also means that FN cannot set or
mutate any variables for you -- the only way it can affect the current
Emacs session is if the caller of `el-job-parallel-mapcar' does
something with the return value."
  (let* (result
         (vars (el-job-ng-vars (cons '(el-job-ng--child-args . 1) inject-vars)))
         (id (intern (format "parallel-mapcar.%S.%d" fn (sxhash vars)))))
    (el-job-ng-run
     :id id
     :require (unless (subr-primitive-p (symbol-function fn)) ;; Emacs 28
                (list (symbol-file fn 'defun t)))
     :inject-vars vars
     :funcall-per-input fn
     :inputs list
     :callback (lambda (outputs)
                 (setq result outputs)))
    (unless (el-job-ng-await-or-die id 86400)
      (error "el-job-ng-parallel-mapcar: Timed out (hung for 24 hours): %S" fn))
    result))

(provide 'el-job)

;;; el-job.el ends here

;; Local Variables:
;; checkdoc-spellcheck-documentation-flag: nil
;; checkdoc-verb-check-experimental-flag: nil
;; emacs-lisp-docstring-fill-column: 72
;; End:

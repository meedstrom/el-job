;;; el-job.el --- Contrived way to call a function using all CPU cores -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Free Software Foundation, Inc.

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
;; Package-Version:  2.4.8-git
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

(defvaralias 'el-job-major-version     'el-job-old-major-version)
(defvaralias 'el-job-max-cores         'el-job-old-max-cores)
;; (defvaralias 'el-job--debug-level      'el-job-old--debug-level)
;; (defvaralias 'el-job--onetime-canary   'el-job-old--onetime-canary)
;; (defvaralias 'el-job--all-jobs         'el-job-old--all-jobs)

(require 'el-job-old)

(defalias 'el-job-launch                      #'el-job-old-launch)
(defalias 'el-job-kill-all                    #'el-job-old-kill-all)
(defalias 'el-job-await                       #'el-job-old-await)
(defalias 'el-job-is-busy                     #'el-job-old-is-busy)
;; (defalias 'el-job--dbg                        #'el-job-old--dbg)
;; (defalias 'el-job--locate-lib-in-load-history #'el-job-old--locate-lib-in-load-history)
;; (defalias 'el-job--ensure-compiled-lib        #'el-job-old--ensure-compiled-lib)
;; (defalias 'el-job--split-evenly               #'el-job-old--split-evenly)
;; (defalias 'el-job--split-optimally            #'el-job-old--split-optimally)
;; (defalias 'el-job--zip-all                    #'el-job-old--zip-all)
;; (defalias 'el-job--windows-cores              #'el-job-old--windows-cores)
;; (defalias 'el-job--with                       #'el-job-old--with)
;; (defalias 'el-job--spawn-processes            #'el-job-old--spawn-processes)
;; (defalias 'el-job--exec-workload              #'el-job-old--exec-workload)
;; (defalias 'el-job--poll                       #'el-job-old--poll)
;; (defalias 'el-job--reap                       #'el-job-old--reap)
;; (defalias 'el-job--handle-output              #'el-job-old--handle-output)
;; (defalias 'el-job--disable                    #'el-job-old--disable)
;; (defalias 'el-job-cycle-debug-level           #'el-job-old-cycle-debug-level)
;; (defalias 'el-job-show-info                   #'el-job-old-show-info)
;; (defalias 'el-job--sit-until-not              #'el-job-old--sit-until-not)

(defvar el-jobs :obsolete)
(let ((complainer
       (lambda (_) (error "Some renames in el-job 2.3.0, update your code"))))
  (fset 'el-job:id             complainer)
  (fset 'el-job:callback       complainer)
  (fset 'el-job:n-cores-to-use complainer)
  (fset 'el-job:ready          complainer)
  (fset 'el-job:busy           complainer)
  (fset 'el-job:stderr         complainer)
  (fset 'el-job:timestamps     complainer)
  (fset 'el-job:poll-timer     complainer)
  (fset 'el-job:finish-times   complainer)
  (fset 'el-job:spawn-args     complainer)
  (fset 'el-job:past-elapsed   complainer)
  (fset 'el-job:queued-inputs  complainer)
  (fset 'el-job:input-sets     complainer)
  (fset 'el-job:result-sets    complainer))

(provide 'el-job)

;;; el-job.el ends here

;; Local Variables:
;; checkdoc-spellcheck-documentation-flag: nil
;; checkdoc-verb-check-experimental-flag: nil
;; emacs-lisp-docstring-fill-column: 72
;; End:

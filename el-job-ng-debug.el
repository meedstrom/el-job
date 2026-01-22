;;; el-job-ng-debug.el --- Debug helpers for el-job-ng  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

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

;;; Commentary:

;; Debug tools.

;;; Code:

(require 'el-job-ng)

;;;###autoload
(defun el-job-ng-debug-kill-all ()
  "Kill all jobs and forget all metadata."
  (interactive)
  (cl-loop for id being each hash-key of el-job-ng--jobs
           do (el-job-ng-kill id))
  (clrhash el-job-ng--jobs))

;;;###autoload
(defun el-job-ng-debug-cycle ()
  "Cycle through values for `el-job-ng--debug-level'."
  (interactive)
  (message "Variable `el-job-ng--debug-level' set to %d"
           (setq el-job-ng--debug-level (% (1+ el-job-ng--debug-level) 3))))

(cl-defun el-job-ng-debug-run (&key id
                                    inject-vars
                                    require
                                    eval
                                    inputs
                                    funcall-per-input
                                    callback)
  "Like `el-job-ng-run' but run in main process.
This exists for comparison and debugging.

Arguments are the same as `el-job-ng-run' \(ID, INJECT-VARS, REQUIRE,
EVAL, INPUTS, FUNCALL-PER-INPUT, CALLBACK)."
  (setq id (or id (abs (random))))
  (let ((job (or (gethash id el-job-ng--jobs)
                 (puthash id (el-job-ng--make-job :id id) el-job-ng--jobs))))
    (el-job-ng--with job (.processes .outputs)
      (dolist (proc .processes)
        (delete-process proc))
      (dolist (lib require)
        (load (locate-library (symbol-name lib))))
      (setf .outputs nil)
      (let ((simple-child
             (lambda ()
               (dolist (form eval)
                 (eval form t))
               (let ((ctr 0))
                 (while-let ((input (pop inputs)))
                   (message "Running %s (%d)..." funcall-per-input (cl-incf ctr))
                   (push (funcall funcall-per-input input inputs)
                         .outputs))))))
        (eval `(let ,(cl-loop for (var . val) in inject-vars
                              if (listp val)
                              collect `(,var ',val)
                              else collect `(,var ,val))
                 (funcall ,simple-child))
              t)
        (when callback
          (funcall callback .outputs))))))

(provide 'el-job-ng-debug)

;;; el-job-ng-debug.el ends here

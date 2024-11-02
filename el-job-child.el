;;; el-job-child.el --- Worker code for children  -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Martin Edström

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

;; The part of the codebase that child processes will need, and no more.

;;; Code:

;; We use `time-convert' instead of `current-time' because
;; 1. (TICKS . HZ) implies a bit less GC churn than (HIGH LOW USEC PSEC)
;; 2. (TICKS . HZ) will be future default
;; 3. With `current-time', we would have to always inject `current-time-list'

(defun el-job-child--zip (alist1 alist2)
  "Zip two alists into one, destructively.
Like the Dash expression \(-zip-with #\\='nconc list1 list2).

ALIST1 and ALIST2 must have the same length,
and each element must be a proper list or nil."
  (let (merged)
    (while alist1
      (push (nconc (pop alist1) (pop alist2))
            merged))
    (when alist2 (error "Lists differed in length"))
    (nreverse merged)))

(defun el-job-child--receive-injection ()
  "Handle :inject-vars, :load and :eval-once."
  (let ((vars (read-minibuffer ""))
        (libs (read-minibuffer ""))
        (eval (read-minibuffer "")))
    (dolist (var vars)
      (set (car var) (cdr var)))
    (dolist (lib libs)
      (load lib))
    (if eval (eval eval))))

(defvar el-job-child--ready nil)
(defun el-job-child--work (func)
  "Run FUNC on one of ITEMS at a time.
FUNC comes from :funcall argument of `org-node-job-launch'.

Benchmark how long FUNC took to handle each item, and add that
information to the final return value."
  (unless el-job-child--ready
    (setq el-job-child--ready t)
    (el-job-child--receive-injection))
  (let ((items (read-minibuffer ""))
        item start output meta results)
    (if items
        (while items
          (setq item (pop items))
          (setq start (time-convert nil t))
          (setq output (funcall func item))
          (push (time-since start) meta)
          ;; May affect the durations erratically, so do this step after.
          (setq results (el-job-child--zip output results)))
      (funcall func))
    ;; Ensure durations are in same order that ITEMS came in, letting us
    ;; associate which with which just by index.
    (setq meta (nreverse meta))
    ;; Timestamp at finish.  Will be the very `car' of the metadata.
    (push (time-convert nil t) meta)
    (let (print-length
          print-level
          (print-circle t)
          (print-escape-newlines t)
          (print-symbols-bare t))
      (prin1 (cons meta results)))))

(provide 'el-job-child)

;;; el-job-child.el ends here

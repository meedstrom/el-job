;;; el-job-child.el --- Worker code for children  -*- lexical-binding: t; -*-

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

;;; Commentary:

;; The part of the codebase that child processes will need, and no more.

;;; Code:

(defun el-job-child--zip (list1 list2)
  "Destructively zip two lists into one.
Like the Dash expression \(-zip-with #\\='nconc list1 list2).

LIST1 and LIST2 must be lists of identical length,
and each element in them must be a list or nil."
  (let (merged)
    (while list1
      (push (nconc (pop list1) (pop list2)) merged))
    (when list2 (error "Lists differed in length"))
    (nreverse merged)))

(defun el-job-child--work (func &optional _)
  "Handle input from `el-job--exec-workload' and print a result.

Since `print' prints to standard output, the mother\\='s so-called
process filter function will see that and insert the result into
the corresponding process buffer.

Assume the input is a list of arguments to pass to FUNC one at a time.
FUNC comes from the :funcall-per-inputs argument of `el-job-launch'.

Benchmark how long FUNC takes to handle each item, and
add that information to the final return value."
  ;; Use `read-minibuffer' to receive what we got via `process-send-string'
  ;; from parent.  Could also use just `read', but that prints an unnecessary
  ;; "Lisp expression: " into parent's process buffer it'd have to clean up.
  (let ((vars (read-minibuffer ""))
        (libs (read-minibuffer "")))
    (dolist (var vars)
      (set (car var) (cdr var)))
    (dolist (lib libs)
      (load lib)))
  (catch 'die
    (let ((current-time-list nil) ;; Fewer cons cells
          input)
      ;; Begin infinite loop, treating each further input from parent as
      ;; a list of things to map to FUNC.
      (while (setq input (read-minibuffer ""))
        (let (item start output metadata results)
          (when (eq input 'die)
            (throw 'die nil))
          (unless input
            (error "Input must be non-nil"))
          (while input
            (setq item (pop input))
            (setq start (current-time))
            (setq output (funcall func item))
            (push (time-since start) metadata)
            (setq results (el-job-child--zip output results)))
          ;; Ensure the benchmarks are in same order that ITEMS came in,
          ;; letting us associate which with which just by index.
          (setq metadata (nreverse metadata))
          ;; Timestamp the finish-time.
          (push (current-time) metadata)
          (let ((print-length nil)
                (print-level nil)
                ;; Even though we had set :coding 'utf-8-emacs-unix in the
                ;; process buffer, this is still necessary.
                ;; https://github.com/meedstrom/org-node/issues/70
                (coding-system-for-write 'utf-8-emacs-unix)
                (print-circle t)
                (print-escape-newlines t)
                (print-symbols-bare t))
            (print (cons metadata results))))))))

(provide 'el-job-child)

;;; el-job-child.el ends here

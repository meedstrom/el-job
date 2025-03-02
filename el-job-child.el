;;; el-job-child.el --- Worker code for children  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Martin Edström

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

(defun el-job-child--zip (meta-list1 meta-list2)
  "Destructively zip two lists into one.
Like the Dash expression \(-zip-with #\\='nconc list1 list2).

META-LIST1 and META-LIST2 must be lists of identical length,
and each element in them must be a list or nil."
  (let (merged)
    (while meta-list1
      (push (nconc (pop meta-list1) (pop meta-list2))
            merged))
    (when meta-list2 (error "Lists differed in length"))
    (nreverse merged)))

(defun el-job-child--receive-injection ()
  "Handle :inject-vars and :load."
  (let ((vars (read-minibuffer ""))
        (libs (read-minibuffer "")))
    (dolist (var vars)
      (set (car var) (cdr var)))
    (dolist (lib libs)
      (load lib))))

(defvar el-job-child--ready nil)
(defun el-job-child--work (func &optional _)
  "Handle input from mother process `el-job--exec' and print a result.

Since `print' prints to standard output, it would be expected to
be passed to a function in the mother process, called the process
filter.

Assume the input is a list of arguments to pass to FUNC one at a time.
FUNC comes from the :funcall argument of `el-job-launch'.

Benchmark how long FUNC took to handle each item, and
add that information to the final return value."
  (unless el-job-child--ready
    (setq el-job-child--ready t)
    (el-job-child--receive-injection))
  (catch 'die
    (while-let ((input (read-minibuffer "")))
      (when (eq input 'die)
        (throw 'die nil))
      (let ((current-time-list nil) ;; Fewer cons cells
            item start output meta results)
        (if input
            (while input
              (setq item (pop input))
              (setq start (current-time))
              (setq output (funcall func item))
              (push (time-since start) meta)
              ;; May affect the durations erratically, so do this step now after benchmarks done.
              (setq results (el-job-child--zip output results)))
          (funcall func))
        ;; Ensure durations are in same order that ITEMS came in, letting us
        ;; associate which with which just by index.
        (setq meta (nreverse meta))
        ;; Timestamp the finish-time.  Will be the very `car' of the metadata.
        (push (current-time) meta)
        (let ((print-length nil)
              (print-level nil)
              ;; Even though we had set :coding 'utf-8-emacs-unix in the
              ;; process buffer, this is still necessary.
              ;; https://github.com/meedstrom/org-node/issues/70
              (coding-system-for-write 'utf-8-emacs-unix)
              (print-circle t)
              (print-escape-newlines t)
              (print-symbols-bare t))
          (print (cons meta results)))))))

(provide 'el-job-child)

;;; el-job-child.el ends here

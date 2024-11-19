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
;; 1. (TICKS . HZ) is fewer cons cells than (HIGH LOW USEC PSEC)
;; 2. (TICKS . HZ) will be future default
;; 3. If we used `current-time', we may have to inject `current-time-list'
;;    to be sure about behavior

(defun el-job-child--zip (metalist1 metalist2)
  "Destructively zip two alists into one.
Like the Dash expression \(-zip-with #\\='nconc list1 list2).

METALIST1 and METALIST2 must be proper lists of the same length,
and each element in them must be a proper list or nil."
  (let (merged)
    (while metalist1
      (push (nconc (pop metalist1) (pop metalist2))
            merged))
    (when metalist2 (error "Lists differed in length"))
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
(defun el-job-child--work (func benchmark)
  "Handle input from mother process `el-job--exec' and print a result.

Since `print' prints to standard output, it would be expected to
be passed to a function in the mother process, called the process
filter.

Assume the input is a list of arguments to pass to FUNC one at a time.
FUNC comes from the :funcall argument of `el-job-launch'.

If BENCHMARK t, benchmark how long FUNC took to handle each item, and
add that information to the final return value."
  (unless el-job-child--ready
    (setq el-job-child--ready t)
    (el-job-child--receive-injection))
  (catch 'die
    (while-let ((input (read-minibuffer "")))
      (when (eq input 'die)
        (throw 'die nil))
      (let (item start output meta results)
        (if input
            (while input
              (setq item (pop input))
              (setq start (time-convert nil t))
              (setq output (funcall func item))
              (when benchmark
                (push (time-since start) meta))
              ;; May affect the durations erratically, so do this step after.
              (setq results (el-job-child--zip output results)))
          (funcall func))
        ;; Ensure durations are in same order that ITEMS came in, letting us
        ;; associate which with which just by index.
        (setq meta (nreverse meta))
        ;; Timestamp the finish-time.  Will be the very `car' of the metadata.
        (push (time-convert nil t) meta)
        (let ((print-length nil)
              (print-level nil)
              (print-circle t)
              (print-escape-newlines t)
              (print-symbols-bare t))
          (print (cons meta results)))))))

(provide 'el-job-child)

;;; el-job-child.el ends here

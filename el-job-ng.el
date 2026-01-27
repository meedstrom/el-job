;;; el-job-ng.el --- Contrived way to call a function using all CPU cores -*- lexical-binding: t; -*-

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

;;; Commentary:

;; New Generation of el-job: simplified to be easier to reason about.

;; Example usage can be seen in https://github.com/meedstrom/org-roam-async

;;; Code:

(require 'cl-lib)
(require 'eieio)

;; https://github.com/meedstrom/el-job/pull/5
(defcustom el-job-ng-max-cores
  (max 1 (- (if (eq system-type 'windows-nt)
                (/ (num-processors) 2)
              (num-processors))
            1))
  "A limit on the number of subprocesses for one job.
Windows can get \"error: Could not create child process\"
if making too many processes, so capping it can help."
  :type 'integer
  :group 'processes)


;;;; Subroutines

(defalias 'el-job-ng--sit-until 'el-job-ng-sit-until)
(defmacro el-job-ng-sit-until (test max-secs &optional message)
  "Block until form TEST evaluates to non-nil, or MAX-SECS elapse.
Either way, return the last TEST result.
In other words, a nil return value means it has timed out.

While blocking input to Emacs, keep MESSAGE visible in the echo area.
MESSAGE can be a string, or a form that evaluates to a string.

Both TEST and MESSAGE should be cheap forms, since they are evaluated
repeatedly and cannot themselves trigger the time-out if they hang.
A typical TEST would check if something in the environment has changed."
  (let ((deadline (gensym "deadline"))
        (last (gensym "last")))
    `(let ((,deadline (time-add (current-time) ,max-secs))
           ,last)
       (catch 'timeout
         (while (null (setq ,last ,test))
           (when (time-less-p ,deadline (current-time))
             (throw 'timeout nil))
           ,(when message `(unless (current-message)
                             (message "%s" ,message)))
           (discard-input)
           (sit-for 0.1)))
       ,last)))

(defvar el-job-ng--debug-level 0
  "Increase this to 1 or 2 to see more debug messages.")

(defun el-job-ng--dbg (level fmt &rest args)
  "Maybe pass FMT and ARGS to `message'.
LEVEL is the threshold for `el-job-ng--debug-level'."
  (declare (indent 1))
  (when (<= level el-job-ng--debug-level)
    (apply #'message (concat "el-job-ng: " fmt) args)))

(defun el-job-ng--split-evenly (list n-slices &optional _)
  "Split LIST into up to N-SLICES sublists."
  (and list (seq-split list (ceiling (/ (length list) (float n-slices))))))

(defvar el-job-ng--splitter-complaint nil)
(defun el-job-ng--split-optimally (list n-slices benchmarks)
  "Split LIST into up to N-SLICES sublists.

If possible, use table BENCHMARKS to balance the sublists.
This reduces the risk that one sublist acquires all the heaviest items
from LIST, as that can make it an extreme outlier in terms of
wall-time needed to work through it.
It happens more often than you would think, and nullifies much of
the gain of el-job.

The order of elements in LIST is preserved across the sublists.
In other words, this equals LIST:

   \(apply \\='append (el-job-ng-split-optimally LIST ...)))"
  (cond
   ((null list) nil)
   ((= n-slices 1) (list list))
   ((or (= 0 (hash-table-count benchmarks))
        (length< list (1+ n-slices)))
    (el-job-ng--split-evenly list n-slices))
   ((let* ((dur-total 0)
           (n-benchmarks 0)
           (n-not-benchmarked 0)
           (items-and-durations
            (cl-loop for item in list
                     as benchmark = (gethash item benchmarks)
                     collect (cons item benchmark)
                     if benchmark
                     do (progn
                          (cl-incf n-benchmarks)
                          (setq dur-total (time-add dur-total benchmark)))
                     else do (cl-incf n-not-benchmarked)
                     finally do (setq dur-total (float-time dur-total)))))
      (if (= 0 n-benchmarks)
          (el-job-ng--split-evenly list n-slices)
        (let* ((dur-mean (/ dur-total n-benchmarks))
               (max-per-slice (/ (+ dur-total (* n-not-benchmarked dur-mean))
                                 n-slices))
               (sum 0)
               sublist
               sublists)
          (cl-loop
           while items-and-durations
           as (item . dur) = (pop items-and-durations)
           do (progn
                (when (and sublist dur (time-less-p max-per-slice dur))
                  ;; An item exceeding max by itself must get a dedicated
                  ;; process, because we will likely be still waiting on that
                  ;; process after the rest have finished.
                  ;; That's common for power-law distributed things.
                  ;; So finish the current sublist early, it's worth it.
                  (push (nreverse sublist) sublists)
                  (setq sublist nil)
                  (setq sum 0))
                (push item sublist)
                (setq sum (time-add sum (or dur dur-mean)))
                (when (time-less-p max-per-slice sum)
                  (push (nreverse sublist) sublists)
                  (setq sublist nil)
                  (setq sum 0)))
           finally do (when sublist
                        (push (nreverse sublist) sublists)))
          (if (<= (length sublists) n-slices)
              (nreverse sublists)
            ;; It is possible to have e.g. `n-slices'==8, a million tiny items,
            ;; and 9 items big enough to exceed `max-per-slice', giving us 10
            ;; sublists in the best case, if not upwards to 20 depending on
            ;; how those items are ordered.
            ;; Just give up in this aberrant case.
            (setq el-job-ng--splitter-complaint t)
            (el-job-ng--split-evenly list n-slices))))))))

(defun el-job-ng--locate-lib (name)
  "Try to find the full .eln or .elc filename for library NAME.
Unlike `locate-library', this can actually find the .eln."
  (let ((el (and (native-comp-available-p)
                 (locate-file name load-path '(".el" ".el.gz")))))
    (or (and el (comp-lookup-eln el))
        (locate-library name)
        (error "el-job-ng: Library not found: %S" name))))


;;;; Entry point

(defvar el-job-ng--jobs (make-hash-table :test 'eq))
(defclass el-job-ng-job ()
  ((id              :initarg :id)
   (stderr          :initform nil)
   (callback        :initform nil)
   (do-bench        :initform t)
   (process-outputs :initform nil)
   (benchmarks      :initform (make-hash-table :test 'equal))))

;;;###autoload
(cl-defun el-job-ng-run (&key id
                              inject-vars
                              require
                              eval
                              inputs
                              funcall-per-input
                              callback)
  "Use asynchronous subprocesses to map FUNCALL-PER-INPUT to INPUTS.

At a glance:

1. Split INPUTS into sub-lists up to `el-job-ng-max-cores', and spawn an
   Emacs subprocess for each.

2. In each subprocess, set INJECT-VARS, load REQUIRE, eval EVAL, then
   loop over its sub-list of INPUTS, calling FUNCALL-PER-INPUT
   on each item and collecting the return values.

3. When all processes finish, append the lists of return values and pass
   that to CALLBACK, a function called precisely once.
   In other words, CALLBACK should be expected to receive one list that
   is equal in length to INPUTS.
   Also, the order of values is preserved.

Details:
- INJECT-VARS is an alist of symbols and values to pass to `set'.
  It has some default members, including `load-path'.
- REQUIRE is a list of symbols for `require', or strings for `load'.
- EVAL is a list of quoted forms.
- FUNCALL-PER-INPUT must be a symbol with a function definition,
  not an anonymous lambda.
  That definition must be discoverable via `load-path' or REQUIRE.
  It is passed two arguments: the current item, and the remaining items.
  \(You probably will not need the second argument.\)

Finally, ID is an optional symbol.  Passing an ID has two effects:
- Automatically cancel a running job with the same ID, before starting.
- Use benchmarks from previous runs to better balance the INPUTS split.
  See `el-job-ng--split-optimally'.

ID can also be passed to these helpers:
- `el-job-ng-get-job'
- `el-job-ng-await'
- `el-job-ng-await-or-die'
- `el-job-ng-ready-p'
- `el-job-ng-busy-p'
- `el-job-ng-kill'
- `el-job-ng-kill-keep-bufs'
- `el-job-ng-processes'
- `el-job-ng-stderr'"
  (unless (symbolp funcall-per-input)
    (error "FUNCALL-PER-INPUT must be defined in some file loaded via REQUIRE"))
  (unless (and inputs (listp inputs))
    (error "INPUTS must be a non-empty list"))
  (when (numberp id)
    (error "Numeric ID is reserved for internal use"))
  ;; Clean-up defunct anonymous jobs
  (cl-loop for id being each hash-key of el-job-ng--jobs
           when (and (numberp id) (not (el-job-ng-busy-p id)))
           do (remhash id el-job-ng--jobs))
  (setq id (or id (abs (random))))
  (setq inject-vars (append (el-job-ng-vars '(load-path
                                              native-comp-eln-load-path))
                            inject-vars))

  (let ((job (with-memoization (gethash id el-job-ng--jobs)
               (make-instance 'el-job-ng-job :id id))))
    (el-job-ng-kill-keep-bufs id)
    (oset job callback callback)
    (with-slots (process-outputs stderr benchmarks do-bench) job
      ;; https://github.com/meedstrom/org-node/issues/98
      (with-temp-buffer
        (let* ((print-length nil)
               (print-level nil)
               (print-circle t)
               (print-escape-newlines t)
               ;; https://github.com/jwiegley/emacs-async/issues/165
               (coding-system-for-write 'utf-8-emacs-unix)
               (coding-system-for-read 'utf-8-emacs-unix)
               (vars (prin1-to-string inject-vars))
               (libs (prin1-to-string require))
               (forms (prin1-to-string eval))
               (func (prin1-to-string funcall-per-input))
               (input-sets
                (prog1 (el-job-ng--split-optimally inputs
                                                   el-job-ng-max-cores
                                                   benchmarks)
                  (when el-job-ng--splitter-complaint
                    (setq el-job-ng--splitter-complaint nil)
                    (setf do-bench nil)
                    (clrhash benchmarks))))
               (n (length input-sets))
               ;; Ensure a local working directory.
               ;; https://github.com/meedstrom/org-node/issues/46
               (default-directory invocation-directory)
               (command
                (list (expand-file-name invocation-name invocation-directory)
                      "--quick"
                      "--batch"
                      "--load" (el-job-ng--locate-lib "el-job-ng")
                      "--funcall" "el-job-ng--child-work")))
          (setf stderr (get-buffer-create (format " *el-job-ng:%s:err*" id) t))
          (with-current-buffer stderr (erase-buffer))
          (condition-case err
              (dotimes (i n)
                (let ((proc (make-process
                             :name (format "el-job-ng:%s:%d" id i)
                             :noquery t
                             :connection-type 'pipe
                             :stderr stderr
                             :buffer (get-buffer-create
                                      (format " *el-job-ng:%s:%d*" id i) t)
                             :command command
                             :sentinel #'el-job-ng--sentinel)))
                  (push (cons proc nil) process-outputs)
                  ;; Q: Why not a temp buffer? A: Have to `erase-buffer' in any
                  ;; case, and this buffer is easier to peek on during edebug.
                  (with-current-buffer (process-buffer proc)
                    (erase-buffer)
                    (insert vars "\n")
                    (insert libs "\n")
                    (insert forms "\n")
                    (insert func "\n")
                    (insert (prin1-to-string (pop input-sets)) "\n")
                    (process-send-region proc (point-min) (point-max))
                    (erase-buffer))))
            ;; https://github.com/meedstrom/org-node/issues/75
            (( file-error )
             (el-job-ng-kill-keep-bufs id)
             (el-job-ng--dbg 1 "Terminated because of: %S" err)))
          (setf process-outputs (nreverse process-outputs)))))))


;;;; Code used by child processes

(defvar el-job-ng--child-args 2)
(defun el-job-ng--child-work ()
  "Read a few lines from stdin, then work according to that info.
Finally print to stdout and die."
  (let* ((coding-system-for-write 'utf-8-emacs-unix)
         (coding-system-for-read  'utf-8-emacs-unix)
         (vars   (read-from-minibuffer "" nil nil t))
         (libs   (read-from-minibuffer "" nil nil t))
         (forms  (read-from-minibuffer "" nil nil t))
         (func   (read-from-minibuffer "" nil nil t))
         (inputs (read-from-minibuffer "" nil nil t))
         (current-time-list nil) ;; Fewer cons cells
         benchmarked-outputs)
    (dolist (var vars)
      (set (car var) (cdr var)))
    (dolist (lib libs)
      (if (stringp lib) (load lib nil t) (require lib)))
    (dolist (form forms)
      (eval form t))
    (while-let ((input (pop inputs)))
      (let ((start (current-time))
            (output (if (eq el-job-ng--child-args 1)
                        (funcall func input)
                      (funcall func input inputs))))
        (push (list input (time-since start) output) benchmarked-outputs)))
    (let ((print-length nil)
          (print-level nil)
          (print-circle t))
      (print (nreverse benchmarked-outputs)))))


;;;; Sentinel; receiving what the child printed

(defun el-job-ng--sentinel (proc event)
  "Handle changed state of a child process.

If PROC and EVENT look like the process is done,
assume the process buffer contains a readable Lisp expression
and run `el-job-ng--handle-finished-child'."
  (let* ((buf (process-buffer proc))
         (job (el-job-ng-get-job proc))
         (id (and job (oref job id)))
         (info (concat (format "Process %s" event) ;; EVENT contains "\n"
                       (format "status:      %S\n" (process-status proc))
                       (format "exit status: %d\n" (process-exit-status proc))
                       (format "buffer:      %S\n" buf)
                       (format "el-job id:   %S" id)))
         (info+tip (concat info "\n"
                           (format "tip:         check the hidden buffer named (note leading space): \"%s\""
                                   (buffer-name (and job (oref job stderr)))))))
    (cond ((not job)
           (el-job-ng--dbg 0 "Could not find job associated with process %S%s"
                           proc
                           ", did something kill the process buffer before sentinel could be called?"))

          ((or (eq (process-status proc) 'run)
               (equal event "killed\n")
               (equal event "deleted\n"))
           ;; Situation normal, often arrive here due to `delete-process'.
           (el-job-ng--dbg 2 "%s" info))

          ((and (eq (process-status proc) 'exit)
                (eq (process-exit-status proc) 0)
                (equal event "finished\n"))
           (cl-assert (buffer-live-p buf))
           (cl-assert (not (process-live-p proc)))
           ;; NOTE: No particular buffer should be current now, because this
           ;; may run the user-provided callback which should be free to do
           ;; whatever to the window configuration.
           (el-job-ng--handle-finished-child proc buf job))

          (t
           (el-job-ng--dbg 0 "%s" info+tip)
           (el-job-ng-kill-keep-bufs id)))))

(defun el-job-ng--handle-finished-child (proc buf job)
  "Handle output returned by PROC, presuming that is in buffer BUF.
Then kill BUF.
Once this has handled all outputs for JOB, run the CALLBACK function
specified in `el-job-ng-run'."
  (with-slots (id process-outputs callback benchmarks do-bench) job
    (with-current-buffer buf
      (unless (and (eobp) (> (point) 2) (eq (char-before) ?\n))
        (error "Process output looks incomplete or point moved"))
      (setcdr (assq proc process-outputs)
              (cl-loop for (input benchmark output) in (read (buffer-string))
                       when do-bench do (puthash input benchmark benchmarks)
                       collect output))
      (setcar (assq proc process-outputs) nil)
      (when (= 0 el-job-ng--debug-level)
        (kill-buffer)))
    ;; True when this was the last child to finish.
    (when (and callback (cl-every #'null (mapcar #'car process-outputs)))
      (let ((outputs (prog1 (mapcan #'cdr process-outputs)
                       (setf process-outputs nil))))
        ;; Allow quitting out of a hung or slow CALLBACK.  Since we're called
        ;; by a process sentinel, `inhibit-quit' is t at this time.
        (when (null (with-local-quit (funcall callback outputs) t))
          (el-job-ng--dbg 0 "Quit while executing :callback for %s" id))))))


;;;; API

(defun el-job-ng-await (id max-secs &optional message)
  "Like `el-job-ng-sit-until' but take ID and return t if job finishes.
MAX-SECS and MESSAGE as in `el-job-ng-sit-until'."
  (el-job-ng-sit-until (el-job-ng-ready-p id) max-secs message))

(defun el-job-ng-await-or-die (id max-secs &optional message)
  "Like `el-job-ng-await', but kill the job on timeout or any signal.
Otherwise, a keyboard quit would let it continue in the background.
ID, MAX-SECS and MESSAGE as in `el-job-ng-await'."
  (condition-case sig
      (if (el-job-ng-await id max-secs message)
          t
        (el-job-ng-kill-keep-bufs id)
        nil)
    (t
     (el-job-ng-kill id)
     (signal (car sig) (cdr sig))
     nil)))

(defun el-job-ng-ready-p (id)
  "Return t if job ID is not currently active."
  (not (el-job-ng-busy-p id)))

(defun el-job-ng-busy-p (id)
  "Return list of busy processes for job ID, if any."
  (seq-filter #'process-live-p (el-job-ng-processes id)))

(defun el-job-ng-kill (id)
  "Kill processes for job ID and their buffers."
  (dolist (proc (el-job-ng-processes id))
    (let ((buf (process-buffer proc)))
      (if (buffer-live-p buf)
          (kill-buffer buf)
        (delete-process proc))))
  (let ((stderr (el-job-ng-stderr id)))
    (when (buffer-live-p stderr)
      (kill-buffer stderr))))

(defun el-job-ng-kill-keep-bufs (id)
  "Kill processes for job ID."
  (dolist (proc (el-job-ng-processes id))
    (delete-process proc)))

(defun el-job-ng-stderr (id)
  "Get the stderr buffer for ID."
  (let ((job (el-job-ng-get-job id)))
    (and job (oref job stderr))))

(defun el-job-ng-processes (id)
  "Get the list of process objects for ID, dead or alive."
  (let ((job (el-job-ng-get-job id)))
    (and job (seq-filter #'processp
                         (mapcar #'car (oref job process-outputs))))))

(defun el-job-ng-get-job (id-or-process)
  "Get the job object associated with ID-OR-PROCESS."
  (if (processp id-or-process)
      (cl-loop for job being each hash-value of el-job-ng--jobs
               when (assq id-or-process (oref job process-outputs))
               return job)
    (gethash id-or-process el-job-ng--jobs)))

(defun el-job-ng-vars (mixed-list &optional lexical)
  "Replace each symbol in MIXED-LIST with a cons cell \(SYMBOL . VALUE\).
If the symbol is not bound or is the symbol nil, it is dropped.
Uses `symbol-value' to get VALUE.
If an element of MIXED-LIST is already a cons cell, it is kept as-is.

Argument LEXICAL is an experiment.  Do not rely on it."
  (cl-loop for var in mixed-list
           if (and var (symbolp var) (boundp var))
           collect (cons var (if lexical (eval var t) (symbol-value var)))
           else if (consp var) collect var))

(provide 'el-job-ng)

;;; el-job-ng.el ends here

;; Local Variables:
;; checkdoc-spellcheck-documentation-flag: nil
;; checkdoc-verb-check-experimental-flag: nil
;; checkdoc-force-docstrings-flag: nil
;; emacs-lisp-docstring-fill-column: 72
;; End:

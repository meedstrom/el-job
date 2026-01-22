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


;;; Subroutines

(define-obsolete-variable-alias 'el-job-ng--debug-lvl 'el-job-ng--debug-level "2.7.0 (2026-01-21)")
(defvar el-job-ng--debug-level 0
  "Increase this to 1 or 2 to see more debug messages.")

(defun el-job-ng--dbg (level fmt &rest args)
  "Maybe pass FMT and ARGS to `message'.
LEVEL is the threshold that `el-job-ng--debug-level' should meet or exceed
to unlock this message."
  (declare (indent 1))
  (when (<= level el-job-ng--debug-level)
    (apply #'message (concat "el-job-ng: " fmt) args)))

(defun el-job-ng--split-evenly (list n-slices &optional _)
  "Split LIST into up to N-SLICES sublists."
  (and list (seq-split list (ceiling (/ (length list) (float n-slices))))))

(defun el-job-ng--split-optimally (list n-slices benchmarks)
  "Split LIST into up to N-SLICES sublists.

If possible, use table BENCHMARKS to balance the sublists.
This reduces the risk that one sublist acquires all the heaviest items
from LIST, as that can make it an extreme outlier in terms of
wall-time needed to work through it.

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
                     collect (cons item (and benchmark (float-time benchmark)))
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
                (when (and sublist dur (> dur max-per-slice))
                  ;; An item exceeding max by itself must get a dedicated process,
                  ;; because we will likely be still waiting on that process after
                  ;; the rest have finished, even with the dedication!
                  ;; So finish the current sublist early, it's worth it.
                  (push (nreverse sublist) sublists)
                  (setq sublist nil)
                  (setq sum 0))
                (push item sublist)
                (cl-incf sum (or dur dur-mean))
                (when (> sum max-per-slice)
                  (push (nreverse sublist) sublists)
                  (setq sublist nil)
                  (setq sum 0)))
           finally do (when sublist
                        (push (nreverse sublist) sublists)))
          (when (> (length sublists) n-slices)
            (fset #'el-job-ng--split-optimally #'el-job-ng--split-evenly)
            (error "el-job: Internal error, degrading gracefully from now on"))
          (nreverse sublists)))))))

(defun el-job-ng--locate-lib (name)
  "Try to find the full .eln or .elc filename for library NAME.
Unlike `locate-library', this can actually find the .eln."
  (let ((el (and (native-comp-available-p)
                 (locate-file name load-path '(".el" ".el.gz")))))
    (or (and el (comp-lookup-eln el))
        (locate-library name)
        (error "el-job-ng: Library not found: %S" name))))

(defmacro el-job-ng--with (job slots &rest body)
  "Make SLOTS expand into object accessors for JOB inside BODY.
Cf. `with-slots' in the \"eieio\" library, or `let-alist'.

JOB is an object of type `el-job-ng--job'.
Each symbol name in SLOTS must be prepended with one character of your
choosing, such as a dot, so e.g. `.id' for \(el-job-ng--job-id job).
The extra character is meant to aid reading clarity inside BODY."
  (declare (indent 2) (debug (sexp sexp &rest form)))
  `(cl-symbol-macrolet
       ,(cl-loop
         for slot in slots
         collect `(,slot (,(intern (concat "el-job-ng--job-"
                                           (substring (symbol-name slot) 1)))
                          ,job)))
     ,@body))


;;; Entry point

(defvar el-job-ng--jobs (make-hash-table :test #'eq))
(cl-defstruct (el-job-ng--job (:constructor el-job-ng--make-job)
                              (:copier nil))
  id
  processes
  stderr
  callback
  (benchmarks-tbl (make-hash-table :test #'equal))
  outputs)

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

Details:
- INJECT-VARS is an alist of symbols and values to pass to `set'.
  It has some default members, including `load-path'.
- REQUIRE is a list of symbols for `require' or strings for `load'.
- EVAL is a list of quoted forms.
- FUNCALL-PER-INPUT must be a symbol with a function definition,
  not an anonymous lambda.
  It is passed two arguments: the current item, and the remaining items.
  \(You probably will not need the second argument.\)

Finally, ID is an optional symbol.  Passing an ID has two effects:
- Automatically cancel a running job with the same ID, before starting.
- Use benchmarks from previous runs to better balance the INPUTS split.

ID can also be passed to these helpers:
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
  (cl-loop for (var . val) in inject-vars
           when (string-prefix-p "#" (readablep val))
           do (error "Cannot inject variable `%s' with value: %s" var val))
  (setq id (or id (abs (random))))
  (let ((job (or (gethash id el-job-ng--jobs)
                 (puthash id (el-job-ng--make-job :id id) el-job-ng--jobs))))
    (el-job-ng--with job (.processes .benchmarks-tbl .callback .outputs .stderr)
      ;; Cancel any currently-running job with same ID
      (dolist (proc .processes)
        (delete-process proc))
      (setf .processes nil)
      (setf .callback callback)
      (setf .outputs nil)
      ;; https://github.com/meedstrom/org-node/issues/98
      (with-temp-buffer
        (let* ((print-length nil)
               (print-level nil)
               (print-circle t)
               (print-escape-newlines t)
               ;; https://github.com/jwiegley/emacs-async/issues/165
               (coding-system-for-write 'utf-8-emacs-unix)
               (coding-system-for-read 'utf-8-emacs-unix)
               (vars (prin1-to-string
                      (append (list (cons 'temporary-file-directory temporary-file-directory)
                                    (cons 'load-path load-path))
                              (and (boundp 'native-comp-eln-load-path)
                                   (list
                                    (cons 'native-comp-eln-load-path native-comp-eln-load-path)))
                              ;; NOTE: These go last so they can override the above.
                              inject-vars)))
               (libs (prin1-to-string require))
               (forms (prin1-to-string eval))
               (func (prin1-to-string funcall-per-input))
               (input-sets
                (el-job-ng--split-optimally inputs
                                            el-job-ng-max-cores
                                            .benchmarks-tbl))
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
          (setf .stderr (get-buffer-create (format " *el-job-ng:%s:err*" id) t))
          (with-current-buffer .stderr (erase-buffer))
          (condition-case err
              (dotimes (i n)
                (let ((proc (make-process
                             :name (format "el-job-ng:%s:%d" id i)
                             :noquery t
                             :connection-type 'pipe
                             :stderr .stderr
                             :buffer (get-buffer-create
                                      (format " *el-job-ng:%s:%d*" id i) t)
                             :command command
                             :sentinel #'el-job-ng--sentinel)))
                  (push proc .processes)
                  ;; Q: Why not a temp buffer? A: Have to `erase-buffer' in any
                  ;; case, and this buffer is easier to peek on during edebug.
                  (with-current-buffer (process-buffer proc)
                    (erase-buffer)
                    (insert vars "\n"
                            libs "\n"
                            forms "\n"
                            func "\n"
                            (prin1-to-string (pop input-sets)) "\n")
                    (process-send-region proc (point-min) (point-max))
                    (erase-buffer))))
            ;; https://github.com/meedstrom/org-node/issues/75
            (( file-error )
             (el-job-ng-kill-keep-bufs id)
             (el-job-ng--dbg 1 "Terminated because of: %S" err))))))))


;;; Code used by child processes

(defun el-job-ng--child-work ()
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
            (output (funcall func input inputs)))
        (push (list input (time-since start) output) benchmarked-outputs)))
    (let ((print-length nil)
          (print-level nil)
          (print-circle t))
      (print benchmarked-outputs))))


;;; Sentinel; receiving what the child printed

(defun el-job-ng--sentinel (proc event)
  "Handle changed state of a child process.

If PROC and EVENT look like the process is done,
assume the process buffer contains a readable Lisp expression
and run `el-job-ng--handle-finished-child'."
  (let* ((buf (process-buffer proc))
         (job (el-job-ng-get-job proc))
         (id (el-job-ng--job-id job))
         (info (concat (format "Process %s" event) ;; EVENT contains "\n"
                       (format "status:      %S\n" (process-status proc))
                       (format "exit status: %d\n" (process-exit-status proc))
                       (format "buffer:      %S\n" buf)
                       (format "el-job id:   %S" id)))
         (info+tip (concat info "\n"
                           (format "tip:         check the hidden buffer named (note leading space): \"%s\""
                                   (buffer-name (el-job-ng-stderr id))))))
    (cond ((or (eq (process-status proc) 'run)
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
  (el-job-ng--with job (.id .processes .benchmarks-tbl .outputs .callback)
    (setf .processes (delq proc .processes))
    (with-current-buffer buf
      (unless (and (eobp) (> (point) 2) (eq (char-before) ?\n))
        (error "Process output looks incomplete or point moved"))
      (goto-char (point-min))
      (cl-loop for (input duration output) in (read (current-buffer)) do
               (puthash input duration .benchmarks-tbl)
               (push output .outputs))
      (when (= 0 el-job-ng--debug-level)
        (kill-buffer)))
    (when (null .processes)
      (when (numberp .id) ;; Clean up anonymous job
        (remhash .id el-job-ng--jobs))
      (when .callback
        ;; Allow quitting out of a hung or slow CALLBACK.  Since we're called
        ;; by a process sentinel, `inhibit-quit' is t at this time.
        (when (null (with-local-quit (funcall .callback .outputs) t))
          (el-job-ng--dbg 0 "Quit while executing :callback for %s" .id))))))


;;; API

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

(defun el-job-ng-await (id max-secs &optional message)
  "Like `el-job-ng-sit-until' but take ID and return t if job finishes."
  (el-job-ng-sit-until (el-job-ng-ready-p id) max-secs message))

(defun el-job-ng-await-or-die (id max-secs &optional message)
  "Like `el-job-ng-await', but kill the job on timeout or any signal.
Otherwise, a keyboard quit would let it continue in the background."
  (condition-case signal
      (if (el-job-ng-await id max-secs message)
          t
        (el-job-ng-kill-keep-bufs id)
        nil)
    (t
     (el-job-ng-kill id)
     (apply #'signal signal)
     nil)))

(defun el-job-ng-ready-p (id)
  "Return t if job ID is not currently active."
  (not (el-job-ng-busy-p id)))

(defun el-job-ng-busy-p (id)
  "Return list of busy processes for job ID, if any."
  (seq-find #'process-live-p (el-job-ng-processes id)))

(defun el-job-ng-kill (id)
  "Kill processes for job ID and their buffers."
  (dolist (proc (el-job-ng-processes id))
    (let ((buf (process-buffer proc)))
      (if (buffer-live-p buf)
          (kill-buffer buf)
        (delete-process proc))))
  (let ((stderr (el-job-ng-stderr id)))
    (when (buffer-live-p stderr)
      (kill-buffer stderr)))
  (when (numberp id) ;; Clean up anonymous job
    (remhash id el-job-ng--jobs)))

(defun el-job-ng-kill-keep-bufs (id)
  "Kill processes for job ID."
  (dolist (proc (el-job-ng-processes id))
    (delete-process proc))
  (when (numberp id)  ;; Clean up anonymous job
    (remhash id el-job-ng--jobs)))

(defun el-job-ng-stderr (id)
  (let ((job (el-job-ng-get-job id)))
    (and job (el-job-ng--job-stderr job))))

(defun el-job-ng-processes (id)
  (let ((job (el-job-ng-get-job id)))
    (and job (el-job-ng--job-processes job))))

(define-obsolete-function-alias 'el-job-ng-job 'el-job-ng-get-job "2026-01-22")
(defun el-job-ng-get-job (id-or-process)
  (if (processp id-or-process)
      (cl-loop for job being each hash-value of el-job-ng--jobs
               when (memq id-or-process (el-job-ng--processes job))
               return job)
    (gethash id-or-process el-job-ng--jobs)))

(defun el-job-ng-vars (mixed-list &optional scope)
  "Replace each symbol in MIXED-LIST with a cons cell \(SYMBOL . VALUE\).
If SYMBOL is nil or not bound, it is dropped.
Uses `symbol-value' to get VALUE.
If an element of MIXED-LIST is already a cons cell, it is kept as-is."
  (cl-loop for var in mixed-list
           if (and var (symbolp var) (boundp var))
           ;; REVIEW: Not sure about the scope thing
           collect (cons var (if scope (eval var t) (symbol-value var)))
           else collect var))

(provide 'el-job-ng)

;;; el-job-ng.el ends here

;; Local Variables:
;; checkdoc-spellcheck-documentation-flag: nil
;; checkdoc-verb-check-experimental-flag: nil
;; checkdoc-force-docstrings-flag: nil
;; emacs-lisp-docstring-fill-column: 72
;; End:

;;; el-job-ng.el --- Contrived way to call a function using all CPU cores -*- lexical-binding: t; -*-

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

;; New Generation of el-job: simplified to be easier to reason about.

;; Example usage can be seen in https://github.com/meedstrom/org-roam-async

;;; Code:

(require 'cl-lib)

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

(defvar el-job-ng--debug-lvl 0
  "Increase this to 1 or 2 to see more debug messages.")

(defun el-job-ng--dbg (level fmt &rest args)
  "Maybe pass FMT and ARGS to `message'.
LEVEL is the threshold that `el-job-ng--debug-lvl' should meet or exceed
to unlock this message."
  (declare (indent 1))
  (when (<= level el-job-ng--debug-lvl)
    (apply #'message (concat "el-job-ng: " fmt) args)))

(defun el-job-ng--split-evenly (big-list n &optional _)
  "Split BIG-LIST into a list of up to N sublists.

If BIG-LIST is not big but contains N or fewer elements,
the consequence looks just like BIG-LIST except that
each element is wrapped in its own list.

E.g: \(1 2 3 4) becomes \((1) (2) (3) (4))."
  (let ((sublist-length (max 1 (/ (length big-list) n)))
        result)
    (dotimes (i n)
      (if (= i (1- n))
          ;; Let the last iteration just take what's left
          (push big-list result)
        (push (take sublist-length big-list) result)
        (setq big-list (nthcdr sublist-length big-list))))
    (delq nil result)))

;; Many things in life are power-law distributed, as with Org file sizes,
;; so this winds up mattering pretty much regardless of what ITEMS is.
(defun el-job-ng--split-optimally (items n-cores benchmarks)
  "Split ITEMS into up to N-CORES lists of items.

For all keys in table BENCHMARKS that match one of ITEMS, assume the
value holds a benchmark \(a Lisp time value) for how long it took in the
past to pass this item through the FUNCALL-PER-INPUT function specified
in `el-job-ng-run'.

Use these benchmarks to rebalance the lists so that each sub-list should
take a similar amount of wall-time to work through.

This reduces the risk that one child takes markedly longer due to
being saddled with a huge item in addition to the average workload."
  (let ((total-duration 0))
    (cond
     ((= n-cores 1)
      (list items))
     ((length< items (1+ n-cores))
      (el-job-ng--split-evenly items n-cores))
     ((progn
        (dolist (item items)
          (let ((dur (gethash item benchmarks)))
            (when dur
              (setq total-duration (time-add total-duration dur)))))
        (eq total-duration 0))
      ;; Probably a first-time run
      (el-job-ng--split-evenly items n-cores))
     (t
      (let ((max-per-core (/ (float-time total-duration) n-cores))
            (this-sublist-sum 0)
            this-sublist
            sublists
            untimed
            dur
            item)
        (catch 'filled
          (while (setq item (pop items))
            (if (length= sublists n-cores)
                (progn (push item items)
                       (throw 'filled t))
              (setq dur (gethash item benchmarks))
              (if (null dur)
                  (push item untimed)
                (setq dur (float-time dur))
                (if (> dur max-per-core)
                    ;; Dedicate huge items to their own cores
                    (push (list item) sublists)
                  ;; Grow a sublist unless it would exceed the max
                  (if (< dur (- max-per-core this-sublist-sum))
                      (progn
                        (push item this-sublist)
                        (setq this-sublist-sum (+ this-sublist-sum dur)))
                    ;; This sublist hit max, so it's done.  Cleanup for next
                    ;; iteration, which will begin a new sublist (or throw).
                    (push this-sublist sublists)
                    (setq this-sublist-sum 0)
                    (setq this-sublist nil)
                    (push item items)))))))
        (when (or (length= sublists 0)
                  (length> sublists n-cores))
          (fset 'el-job-ng--split-optimally #'el-job-ng--split-evenly)
          (error "Internal coding mistake, degrading gracefully from now"))
        ;; Spread leftovers evenly
        (let ((ctr 0)
              (len (length sublists)))
          (dolist (item (nconc this-sublist untimed items))
            (push item (nth
                        (% (cl-incf ctr) len)
                        sublists))))
        sublists)))))

(defun el-job-ng--locate-lib (name)
  "Try to find the full .eln or .elc filename for library NAME."
  (let ((el (and (native-comp-available-p)
                 (locate-file name load-path '(".el" ".el.gz")))))
    (or (and el (comp-lookup-eln el))
        (locate-library name)
        (error "el-job-ng: Library not found: %S" name))))

(defmacro el-job-ng--with (job slots &rest body)
  "Make SLOTS expand into object accessors for `el-job-ng' JOB inside BODY.
Cf. `with-slots' in the \"eieio\" library, or `let-alist'.

For clarity inside BODY, each symbol name in SLOTS must be prepended
with one character of your choosing, such as a dot."
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
(defvar el-job-ng--debug-last-splits nil)
(defvar-local el-job-ng--job-here nil)
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
   that to CALLBACK \(funcalled precisely once\) in the main process.
   In other words, CALLBACK should be expected to receive one list that
   is equal in length to INPUTS.

Details:
- INJECT-VARS is an alist of symbols and values to pass to `set'.
  It has some default members, including `load-path'.
- REQUIRE is a list of symbols like `features'.
- EVAL is a list of quoted forms.
- FUNCALL-PER-INPUT must be a symbol with a function definition,
  not an anonymous lambda.
  It is passed two arguments: the current item, and the remaining items.
  \(You probably will not need the second argument.\)

Finally, ID is an optional symbol.  It has two effects:
- Automatically cancel a running job with the same ID, before starting.
- Use benchmarks from previous runs to better balance the INPUTS split.

ID can also be passed to these helpers:
- `el-job-ng-await'
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
      (setq el-job-ng--debug-last-splits nil)
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
          (setq el-job-ng--debug-last-splits input-sets)
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
                  (with-current-buffer (process-buffer proc)
                    (erase-buffer)
                    (setq-local el-job-ng--job-here job)
                    (insert vars "\n"
                            libs "\n"
                            forms "\n"
                            func "\n"
                            (prin1-to-string (pop input-sets)) "\n")
                    (process-send-region proc (point-min) (point-max))
                    (when (>= el-job-ng--debug-lvl 2)
                      (clone-buffer (format "*cloned: %s*" (buffer-name))))
                    (erase-buffer))))
            ;; https://github.com/meedstrom/org-node/issues/75
            (( file-error )
             (el-job-ng-kill-keep-bufs id)
             (el-job-ng--dbg 1 "Terminated because of: %S" err))))))))


;;; Child: code run inside subprocess

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
      (require lib))
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


;;; Sentinel: receive what the child printed for us

(defun el-job-ng--sentinel (proc event)
  (let* ((info (concat (format "Process %s" event)
                       (format "status:      '%S\n" (process-status proc))
                       (format "exit status: %d\n" (process-exit-status proc))
                       (format "buffer:      %S" (process-buffer proc))))
         (buf (process-buffer proc))
         (job (buffer-local-value 'el-job-ng--job-here buf)))
    (cl-assert job)
    (cond ((or (eq (process-status proc) 'run)
               (equal event "killed\n")
               (equal event "deleted\n"))
           ;; Situation normal, often arrive here due to `delete-process'.
           (el-job-ng--dbg 2 "%s" info))

          ((and (eq (process-status proc) 'exit)
                (eq (process-exit-status proc) 0)
                (equal event "finished\n"))
           ;; NOTE: No particular buffer should be current now, because this
           ;; may run the user-provided callback which should be free to do
           ;; whatever to the window configuration.
           (el-job-ng--handle-finished-child proc buf job)
           (when (and (= 0 el-job-ng--debug-lvl)
                      (buffer-live-p buf))
             (kill-buffer buf)))

          (t
           (lwarn 'el-job-ng :warning "%s"
                  (concat info
                          (format "\ntip:         check the hidden buffer named (note leading space): \"%s\""
                                  (buffer-name (el-job-ng--job-stderr job)))))
           (el-job-ng-kill-keep-bufs (el-job-ng--job-id job))))))

(defun el-job-ng--handle-finished-child (proc buf job)
  (el-job-ng--with job (.id .processes .benchmarks-tbl .outputs .callback)
    (setf .processes (delq proc .processes))
    (with-current-buffer buf
      (unless (and (eobp) (> (point) 2) (eq (char-before) ?\n))
        (error "Process output looks incomplete or point moved"))
      (goto-char (point-min))
      (cl-loop for (input duration output) in (read (current-buffer)) do
               (puthash input duration .benchmarks-tbl)
               (push output .outputs)))
    (when (null .processes)
      (when (numberp .id) ;; Clean up anonymous job
        (remhash .id el-job-ng--jobs))
      (when .callback
        ;; Allow quitting out of a hung or slow CALLBACK.
        ;; A process sentinel ordinarily does not allow quitting at all.
        (when (null (with-local-quit (funcall .callback .outputs) t))
          (el-job-ng--dbg 0 "Quit while executing :callback for %s" .id))))))


;;; API

(defmacro el-job-ng-sit-until (test max-secs &optional message)
  "Block until form TEST evaluates to non-nil, or MAX-SECS elapse.
Either way, return the last TEST result.
In other words, a nil return value means it has timed out.

While blocking input to Emacs, keep MESSAGE visible in the echo area.
MESSAGE can be a string, or a form that evaluates to a string.

Neither TEST nor MESSAGE should be expensive forms, since they are
evaluated repeatedly and cannot themselves trigger the time-out if they
should happen to be expensive.
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
  "Like `el-job-ng-sit-until' but take ID and return t if job finishes.
MAX-SECS and MESSAGE as in `el-job-ng-sit-until'."
  (el-job-ng-sit-until (el-job-ng-ready-p id) max-secs message))

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
  (when-let* ((job (el-job-ng-job id)))
    (el-job-ng--job-stderr (el-job-ng-job id))))

(defun el-job-ng-processes (id)
  (when-let* ((job (el-job-ng-job id)))
    (el-job-ng--job-processes job)))

(defun el-job-ng-job (id)
  (gethash id el-job-ng--jobs))


;;; Debug tools

(defun el-job-ng-debug-kill-all ()
  "Kill all jobs and forget all metadata."
  (interactive)
  (cl-loop for id being each hash-key of el-job-ng--jobs
           do (el-job-ng-kill id))
  (clrhash el-job-ng--jobs))

(defun el-job-ng-cycle-debug ()
  "Cycle through values for `el-job-ng--debug-lvl'."
  (interactive)
  (message "Variable `el-job-ng--debug-lvl' set to %d"
           (setq el-job-ng--debug-lvl (% (1+ el-job-ng--debug-lvl) 3))))

(cl-defun el-job-ng-run-sync (&key id
                                   inject-vars
                                   require
                                   eval
                                   inputs
                                   funcall-per-input
                                   callback)
  "Like `el-job-ng-run' but synchronous.
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

(provide 'el-job-ng)

;;; el-job-ng.el ends here

;; Local Variables:
;; checkdoc-spellcheck-documentation-flag: nil
;; checkdoc-verb-check-experimental-flag: nil
;; checkdoc-force-docstrings-flag: nil
;; emacs-lisp-docstring-fill-column: 72
;; End:

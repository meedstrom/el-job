;;; el-job.el --- Call a function using all CPU cores -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Martin Edström
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Author:           Martin Edström <meedstrom91@gmail.com>
;; Created:          2024-10-30
;; URL:              https://github.com/meedstrom/el-job
;; Keywords:         processes
;; Package-Requires: ((emacs "28.1") (compat "30"))

;;; Commentary:

;; Imagine you have a function you'd like to run on a long list of inputs.  You
;; could run (mapcar #'FN INPUTS), but that hangs Emacs until done.

;; This library gives you the tools to split up the inputs and run the function
;; in many subprocesses (one per CPU core), then merges their outputs and
;; passes it back to the current Emacs.  In the meantime, current Emacs does
;; not hang at all.

;; The only public API is the function `el-job-launch'.

;;; Code:

(require 'cl-lib)
(require 'esh-proc)
(require 'compat)
(require 'el-job-child)
(declare-function eshell-wait-for-processes "esh-proc")

;;; Subroutines:

(defvar el-job--feature-mem nil)
(defun el-job--loaded-lib (feature)
  "Look for .eln, .elc or .el file corresponding to FEATURE.
FEATURE is a symbol as it shows up in `load'.

Guess which one was in fact loaded by the current Emacs,
and return it if it is .elc or .eln.

If it is .el, then opportunistically compile it and return the newly
compiled file instead.  This returns an .elc on the first call, then an
.eln on future calls.

Note: if you are currently editing the source code for FEATURE, use
`eval-buffer' and save to ensure this finds the correct file."
  (or
   (alist-get feature el-job--feature-mem)
   (let* ((hit
           (cl-loop
            for (file . elems) in load-history
            when (eq feature (cdr (assq 'provide elems)))
            return
            ;; Want two pieces of info: the file path according to
            ;; `load-history', and some function supposedly defined
            ;; there.  The function is a better source of info, for
            ;; discovering an .eln.
            (cons file (cl-loop
                        for elem in elems
                        when (and (consp elem)
                                  (eq 'defun (car elem))
                                  (not (consp (symbol-function (cdr elem))))
                                  (not (function-alias-p (cdr elem))))
                        return (cdr elem)))))
          ;; Perf. Not confirmed necessary.
          ;; TODO: Test if it can compile eln from el.gz with null handlers
          (file-name-handler-alist '(("\\.gz\\'" . jka-compr-handler)))
          (loaded (or (and (native-comp-available-p)
                           (ignore-errors
                             ;; REVIEW: `symbol-file' uses expand-file-name,
                             ;;         but I'm not convinced it is needed
                             (expand-file-name
                              (native-comp-unit-file
                               (subr-native-comp-unit
                                (symbol-function (cdr hit)))))))
                      (car hit)))
          blessed)
     (unless loaded
       (error "Current Lisp definitions must come from a file %S[.el/.elc/.eln]"
              feature))
     ;; HACK: Sometimes comp.el makes freefn- temp files; pretend we found .el.
     ;;       Bad hack, because load-path is NOT as trustworthy as load-history
     ;;       (current Emacs may not be using the thing in load-path).
     (when (string-search "freefn-" loaded)
       (setq loaded
             (locate-file (symbol-name feature) load-path '(".el" ".el.gz"))))
     (setq blessed
           (if (or (string-suffix-p ".el" loaded)
                   (string-suffix-p ".el.gz" loaded))
               (or (when (native-comp-available-p)
                     ;; If we built an .eln last time, return it now even
                     ;; though the current Emacs process is still running
                     ;; interpreted .el.
                     (comp-lookup-eln loaded))
                   (let* ((elc (file-name-concat temporary-file-directory
                                                 (concat (symbol-name feature)
                                                         ".elc")))
                          (byte-compile-dest-file-function
                           `(lambda (&rest _) ,elc)))
                     (when (native-comp-available-p)
                       (native-compile-async (list loaded)))
                     ;; Native comp may take a while, so return .elc this time.
                     ;; We should not pick an .elc from load path if Emacs is
                     ;; now running interpreted code, since the currently
                     ;; running code is likely newer.
                     (if (or (file-newer-than-file-p elc loaded)
                             (byte-compile-file loaded))
                         ;; NOTE: On Guix we should never end up here, but if
                         ;; we did, that'd be a problem as Guix will probably
                         ;; reuse the first .elc we ever made forever, even
                         ;; after upgrades to .el, due to 1970 timestamps.
                         elc
                       loaded)))
             ;; Either .eln or .elc was loaded, so use the same for the
             ;; children.  We should not opportunistically build an .eln if
             ;; Emacs had loaded an .elc for the current process, because we
             ;; cannot assume the source .el is equivalent code.
             ;; The .el could be in-development, newer than .elc, so
             ;; children should use the old .elc for compatibility right
             ;; up until the point the developer actually evals the .el buffer.
             loaded))
     (setf (alist-get feature el-job--feature-mem) blessed)
     ;; Expire memoization in 3 seconds - enough to be useful during a launch.
     (run-with-timer 3 () (lambda ()
                            (assq-delete-all feature el-job--feature-mem)))
     blessed)))

(defun el-job--split-optimally (items n table)
  "Split ITEMS into N lists of items.

Inspect TABLE for how long a batch took to execute on a given item last
time, to return balanced lists that should each take around the same
total wall-time to process.

This reduces the risk that one subprocess takes noticably longer due to
being saddled with a mega-item in addition to the average workload."
  (if (<= (length items) n)
      (el-job--split-evenly items n)
    (let ((total-duration (time-convert 0 t)))
      (if (length= items (hash-table-count table))
          ;; Shortcut (I think)
          (maphash (lambda (_ dur)
                     (setq total-duration (time-add total-duration dur)))
                   table)
        (let (dur)
          (dolist (item items)
            (when (setq dur (gethash item table))
              (setq total-duration (time-add total-duration dur))))))
      ;; Special case for first time
      (if (equal total-duration (time-convert 0 t))
          (el-job--split-evenly items n)
        (let ((max-per-core (/ (float-time total-duration) n))
              (this-sublist-sum 0)
              sublists
              this-sublist
              untimed
              dur)
          (catch 'filled
            (while-let ((item (pop items)))
              (setq dur (float-time (gethash item table)))
              (if (null dur)
                  (push item untimed)
                (if (> dur max-per-core)
                    ;; Dedicate huge items to their own cores
                    (push (list item) sublists)
                  (if (< dur (- max-per-core this-sublist-sum))
                      (progn
                        (push item this-sublist)
                        (setq this-sublist-sum (+ this-sublist-sum dur)))
                    (push this-sublist sublists)
                    (setq this-sublist-sum 0)
                    (setq this-sublist nil)
                    (push item items)
                    (when (= (length sublists) n)
                      (throw 'filled t)))))))
          ;; Let last sublist absorb all untimed
          (if this-sublist
              (progn
                (push (nconc untimed this-sublist) sublists)
                (when items
                  (message "el-job: ITEMS surprisingly not empty: %s" items)))
            ;; Last sublist already hit time limit, spread leftovers equally
            (let ((ctr 0)
                  (len (length sublists)))
              (if (= len 0)
                  ;; All items are untimed
                  ;; REVIEW: Code never ends up here, right?
                  (progn
                    (setq sublists (el-job--split-evenly untimed n))
                    (message "%s" "el-job: Unexpected code path. Not fatal, but report appreciated"))
                (dolist (item (nconc untimed items))
                  (push item (nth (% (cl-incf ctr) len) sublists))))))
          sublists)))))

(defun el-job--split-evenly (big-list n)
  "Split BIG-LIST equally into a list of N sublists.

In the unlikely case where BIG-LIST contains N or fewer elements,
that results in a value just like BIG-LIST except that
each element is wrapped in its own list."
  (let ((sublist-length (max 1 (/ (length big-list) n)))
        result)
    (dotimes (i n)
      (if (= i (1- n))
          ;; Let the last iteration just take what's left
          (push big-list result)
        (push (take sublist-length big-list) result)
        (setq big-list (nthcdr sublist-length big-list))))
    (delq nil result)))

(defvar el-job-force-cores nil
  "Explicit default for `el-job--cores'.
If non-nil, use this value instead of attempting to count CPU cores.")

(defvar el-job--cores nil
  "Max simultaneous processes for a given batch of jobs.")

(defun el-job--count-logical-cores ()
  "Return sum of available processor cores/hyperthreads, minus 1."
  (max (1- (string-to-number
            (pcase system-type
              ((or 'gnu 'gnu/linux 'gnu/kfreebsd 'berkeley-unix)
               (if (executable-find "nproc")
                   (shell-command-to-string "nproc --all")
                 (shell-command-to-string "lscpu -p | egrep -v '^#' | wc -l")))
              ((or 'darwin)
               (shell-command-to-string "sysctl -n hw.logicalcpu_max"))
              ;; No idea if this works
              ((or 'cygwin 'windows-nt 'ms-dos)
               (ignore-errors
                 (with-temp-buffer
                   (call-process "echo" nil t nil "%NUMBER_OF_PROCESSORS%")
                   (buffer-string)))))))
       1))

(defun el-job--zip-all (alists)
  "Zip all ALISTS into one, destructively.
See `el-job-child--zip' for details."
  (let ((merged (pop alists)))
    (while alists
      (setq merged (el-job-child--zip (pop alists) merged)))
    merged))


;;; Main logic:

(defvar el-job--batches (make-hash-table :test #'eq))
(cl-defstruct (el-job-batch (:constructor el-job-batch-make)
                            (:copier nil)
                            (:conc-name el-job-))
  lock
  processes
  inputs
  results
  stderr
  inhibit-wrapup
  (timestamps (list :accept-launch-request (time-convert nil t)))
  (elapsed-table (make-hash-table :test #'equal)))

;; TODO: How to share the same elapsed-table, without locking?
(cl-defun el-job-launch (&key load
                              inject-vars
                              eval-once
                              funcall
                              inputs
                              wrapup
                              await-max
                              lock
                              max-children ;; will deprecate
                              ;; TODO
                              ;;  use-file-handlers
                              debug)
  "Run FUNCALL in one or more headless Elisp processes.
Then merge the return values \(lists of N lists) into one list
\(of N lists) and pass it to WRAPUP.

i.e. each subprocess may return lists like

process 1: \((city1 city2) (road1) (museum1 museum2))
process 2: \((city3 city4 city5) (road2) (museum3))
process 3: ...

but at the end, these lists are merged into a single list shaped just like
any one of those above, with the difference that the sublists have more
elements:

\((city1 city2 city3 city4 city5)
  (road1 road2)
  (museum1 museum2 museum3))

which is why it's important that FUNCALL always returns a list with a
fixed number of sub-lists, enabling this merge.

Alternatively, FUNCALL may always return nil.


FUNCALL is a function symbol known to be defined in an Emacs Lisp file.
It is the only mandatory argument, but rarely useful on its own.

Usually, the list LOAD would indicate where to find that Emacs Lisp
file; that file should end with a `provide' call to the same feature.

While subprocesses do not inherit `load-path', it is the mother Emacs
process that locates that file \(by inspecting `load-history', see
`el-job--loaded-lib' for particulars), then gives the file to the
subprocess.

Due to the absence of `load-path', be careful writing `require'
statements into that Emacs Lisp file.  You can pass `load-path' via
INJECT-VARS, but consider that less requires means faster spin-up.


SPLITABLE-DATA is a list that will be split by up to the number
of CPU cores on your machine, and this determines how many
subprocesses will spawn.  If SPLITABLE-DATA is nil, only
one subprocess will spawn.

The subprocesses have no access to current Emacs state.  The only way
they can affect current state, is if FUNCALL returns data, which is then
handled by WRAPUP function in the current Emacs.

Emacs stays responsive to user input up until all subprocesses finish,
which is when their results are merged and WRAPUP is executed.  If you
prefer Emacs to freeze and wait for this momentous event, set
AWAIT-MAX to a number of seconds.

If all children finish before AWAIT-MAX, then the return value is the
same list of results that would have been passed to WRAPUP, and WRAPUP
is not executed.  Otherwise, the return value is nil.

WRAPUP receives two arguments: the results as mentioned before, and the
job batch metadata.  The latter is mainly useful to check timestamps,
which you can get from this form:

    \(el-job-timestamps JOB)


LOCK is a symbol identifying this batch of jobs, and prevents launching
another batch with the same LOCK if the previous batch has not
completed.  It can also be a keyword or an integer below 536,870,911
(suitable for `eq').

EVAL-ONCE is a string containing a Lisp form.  It is evaluated in the
child just before FUNCALL, but only once, even though FUNCALL may be
evaluated many times."
  (unless (symbolp funcall)
    (error "Argument :funcall only takes a symbol"))
  (setq load (ensure-list load))
  (if el-job-force-cores
      (setq el-job--cores el-job-force-cores)
    (unless el-job--cores
      (setq el-job--cores (el-job--count-logical-cores))))
  (let ( batch stop )
    (if lock
        (if (setq batch (gethash lock el-job--batches))
            (if (seq-some #'process-live-p (el-job-processes batch))
                (setq stop (message "el-job: Batch %s still at work"))
              (mapc #'delete-process (el-job-processes batch))
              (setf (el-job-processes batch)      nil)
              (setf (el-job-inputs batch)         nil)
              (setf (el-job-results batch)        nil)
              (setf (el-job-inhibit-wrapup batch) nil)
              (setf (el-job-lock batch)           lock)
              (setf (el-job-timestamps batch)
                    (list :accept-launch-request (time-convert nil t))))
          (setq batch
                (puthash lock (el-job-batch-make :lock lock)
                         el-job--batches)))
      ;; TODO: Do not benchmark inputs for anonymous job
      (setq batch (el-job-batch-make)))
    (cond
     (stop)

     ;; TODO: Run single-threaded in current Emacs to enable stepping
     ;;       through code with edebug.
     ;; NOTE: Must not `load' the feature files (would undo edebug
     ;;       instrumentations in them).
     (debug)

     (t

      (let* ((splits (el-job--split-optimally inputs
                                              (or max-children el-job--cores)
                                              (el-job-elapsed-table batch)))
             (n (if splits (length splits) 1))
             ;; Anonymous batch needs buffer names that will never be reused
             (name (or lock (format-time-string "%FT%H%M%S%N")))
             (shared-stderr
              (setf (el-job-stderr batch)
                    (with-current-buffer
                        (get-buffer-create (format " *el-job-%s:err*" name) t)
                      (erase-buffer)
                      (current-buffer))))
             print-length
             print-level
             (inject-vars-alist
              (cons (cons 'current-time-list current-time-list)
                    ;; TODO: Reuse allocated memory instead of building a new
                    ;; list since the values could possibly be huge.
                    (cl-loop
                     for var in inject-vars
                     if (symbolp var) collect (cons var (symbol-value var))
                     else collect var)))
             (inject-vars-expr (prin1-to-string
                                `(dolist (var ',inject-vars-alist)
                                   (set (car var) (cdr var)))))
             ;; Ensure the working directory is not remote (messes things up)
             (default-directory invocation-directory)
             items proc)
        (dotimes (i n)
          (setq items (pop splits))
          (setq proc
                (make-process
                 :name (format "el-job-%s:%d" name i)
                 :noquery t
                 ;; Pipe is the fallback on environments that don't support
                 ;; PTY, so I'll force pipe for now to reveal any footguns
                 :connection-type 'pipe
                 :stderr shared-stderr
                 :buffer (with-current-buffer (get-buffer-create
                                               (format " *el-job-%s:%d*" name i)
                                               t)
                           (erase-buffer)
                           (current-buffer))
                 :command
                 (nconc
                  (list
                   (file-name-concat invocation-directory invocation-name)
                   "--quick"
                   "--batch")
                  (cl-loop
                   for file in (mapcar #'el-job--loaded-lib load)
                   nconc (list "--load" file))
                  (if inject-vars (list "--eval" inject-vars-expr))
                  (if eval-once (list "--eval" eval-once))
                  (list
                   "--load" (el-job--loaded-lib 'el-job-child)
                   "--eval" (format "(el-job-child--work #'%S '%s)"
                                    funcall
                                    (prin1-to-string items))))
                 :sentinel
                 (lambda (proc event)
                   (pcase event
                     ("finished\n"
                      (el-job--handle-finished proc batch n wrapup))
                     ("deleted\n")
                     (_ (message "Process event: %s" event))))))
          (push proc (el-job-processes batch))
          (setf (alist-get proc (el-job-inputs batch))
                items))
        (plist-put (el-job-timestamps batch)
                   :launched-children (time-convert nil t)))
      ;; A big use-case for synchronous execution: return the results directly
      ;; to the caller.  It is still multi-core, so should be faster than a
      ;; normal funcall.
      (when await-max
        (setf (el-job-inhibit-wrapup batch) t)
        (if (eshell-wait-for-processes (el-job-processes batch) await-max)
            (el-job-results batch)
          (setf (el-job-inhibit-wrapup batch) nil)))))))

;; TODO: Sanitize/cleanup after error
(defun el-job--handle-finished (proc batch n &optional wrapup)
  "If PROC has exited, record its output in object BATCH.

Each batch-job is expected to call this a total of N times; if this is
the Nth call, then call function WRAPUP and pass it the merged outputs."
  (cond
   ((not (eq 'exit (process-status proc)))
    (message "%s" "Process said \"finished\" but process status is not `exit'"))
   ((/= 0 (process-exit-status proc))
    (message "%s" "Nonzero exit status"))
   (t
    (unless (<= 48 (string-to-char (substring (process-name proc) -1))
                57)
      ;; Name ends in an angle bracket e.g. "process-13<5>"
      (message "Unintended duplicate process name %s" proc))
    (let (output)
      (with-current-buffer (process-buffer proc)
        (condition-case err (setq output (read (buffer-string)))
          (( quit )
           (error "BUG: Received impossible quit during sentinel"))
          (( error )
           (error "Problems reading el-job child output: %S" err))
          (:success
           (when (el-job-lock batch)
             ;; Record the time spent by FUNCALL on each item in
             ;; INPUTS, for next time with `el-job--split-optimally'.
             (let ((durations (cdar output))
                   (input (alist-get proc (el-job-inputs batch))))
               (dolist (item input)
                 (puthash item (pop durations) (el-job-elapsed-table batch)))))
           ;; The `car' was just our own metadata
           (push (cdr output) (el-job-results batch)))))
      (when (= (length (el-job-results batch)) n)
        ;; We are in the last process sentinel
        (plist-put (el-job-timestamps batch)
                   :children-done (caar output))
        (unless (el-job-lock batch)
          (kill-buffer (el-job-stderr batch))
          (dolist (proc (el-job-processes batch))
            (kill-buffer proc)))
        ;; Would be nice if we could timestamp the moment where we /begin/
        ;; accepting results, i.e. the first sentinel, but this may occur
        ;; before the last child has exited, so it would be confusing.  At
        ;; least we can catch the moment before we merge the results.
        (plist-put (el-job-timestamps batch)
                   :got-all-results (time-convert nil t))
        (setf (el-job-results batch) (el-job--zip-all (el-job-results batch)))
        (when (and wrapup (not (el-job-inhibit-wrapup batch)))
          (funcall wrapup (el-job-results batch) batch)))))))

(provide 'el-job)

;;; el-job.el ends here

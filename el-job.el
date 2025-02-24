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

;; NOTE: Looking for Package-Version?
;;       Consult your package manager, or the Git tag.

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
(require 'compat)
(require 'el-job-child)

;;; Subroutines:

(defvar el-job--debug-level 0
  "Increase this to 1 or 2 to see more debug messages.")

(defun el-job--dbg (level fmt &rest args)
  "If debugging is enabled, format FMT with ARGS and print as message.
LEVEL is the threshold for `el-job--debug-level' to unlock this warning.
At LEVEL 0, don't just print a message, display a warning."
  (declare (indent 2))
  (if (<= level el-job--debug-level)
      (if (> level 0)
          (apply #'message fmt args)
        (display-warning 'el-job (apply #'format-message fmt args)))))

(defun el-job--locate-lib-in-load-history (feature)
  "Look for the .eln, .elc or .el file corresponding to FEATURE.
FEATURE is a symbol such as those seen in `features'.

Return whichever variant was in fact loaded by the current Emacs.

Unusually, this looks in `load-history', not `load-path', so the result
can change after you use `eval-buffer' in an .el file that you are
editing."
  (let ((hit
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
                      return (cdr elem))))))
    (or (and (native-comp-available-p)
             (ignore-errors
               ;; REVIEW: `symbol-file' uses expand-file-name,
               ;;         but I'm not convinced it is needed
               (expand-file-name
                (native-comp-unit-file
                 (subr-native-comp-unit
                  (symbol-function (cdr hit)))))))
        (car hit))))

(defun el-job--ensure-compiled-lib (feature)
  "Look for the .eln, .elc or .el file corresponding to FEATURE.
FEATURE is a symbol such as those seen in `features'.

Guess which variant was in fact loaded by the current Emacs,
and return it if it is .elc or .eln.

If it is .el, then opportunistically compile it and return the newly
compiled file instead.  This returns an .elc on the first call, then an
.eln on future calls.

Note: if you are currently editing the source code for FEATURE, save it
and use \\[eval-buffer] to ensure this will find the correct file."
  (let ((loaded (el-job--locate-lib-in-load-history feature)))
    (unless loaded
      (error "Current Lisp definitions must come from a file %S[.el/.elc/.eln]"
             feature))
    ;; HACK: Sometimes comp.el makes freefn- temp files; pretend we found the
    ;;       .el that's in load-path, instead.  Bad hack, because load-path is
    ;;       NOT as trustworthy as load-history (current Emacs may not be using
    ;;       the thing in load-path).
    (when (string-search "freefn-" loaded)
      (setq loaded
            (locate-file (symbol-name feature) load-path '(".el" ".el.gz"))))
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
                ;; FIXME: Guix strips the hash from the .eln filename, so
                ;; compiling now can result in an .eln in ~/.emacs.d that will
                ;; always take precedence over the one shipped by Guix.  If we
                ;; want to cover for that, it'd be safer to compile into /tmp
                ;; with a filename based on emacs-init-time or something.
                ;; https://github.com/meedstrom/org-node/issues/68
                (native-compile-async (list loaded)))
              ;; Native comp may take a while, so build and return .elc this
              ;; time.  We should not pick a preexisting .elc from load path if
              ;; Emacs is now running interpreted code, since that currently
              ;; running code is likely newer.
              (if (or (file-newer-than-file-p elc loaded)
                      (byte-compile-file loaded))
                  ;; NOTE: On Guix we should never end up here, but if
                  ;; we did, that'd be a problem as Guix will probably
                  ;; reuse the first .elc we ever made forever, even
                  ;; after upgrades to .el, due to 1970 timestamps.
                  elc
                loaded)))
      ;; Either .eln or .elc was loaded, so return the same.
      ;; We should not opportunistically build an .eln if current Emacs process
      ;; is using code from an .elc, because we cannot assume the source .el is
      ;; equivalent code.  It could be in-development, newer than the .elc,
      ;; so children should also use the .elc for compatibility right up until
      ;; the point the developer actually evals the .el buffer.
      loaded)))

(defun el-job--split-optimally (items n table)
  "Split ITEMS into up to N lists of items.

For all keys in TABLE that match one of ITEMS, assume the value holds a
benchmark \(a Lisp time value) for how long it took in the past to pass
this item through the FUNCALL function specified by `el-job-launch'.

Use these benchmarks to rebalance the lists so that each sub-list should
take around the same total wall-time to work through this time.

This reduces the risk that one child takes noticably longer due to
being saddled with a mega-item in addition to the average workload."
  (let ((total-duration (time-convert 0 t)))
    (cond
     ((= n 1)
      (list items))
     ((length< items (1+ n))
      (el-job--split-evenly items n))
     ((progn
        (dolist (item items)
          (when-let ((dur (gethash item table)))
            (setq total-duration (time-add total-duration dur))))
        (time-equal-p total-duration (time-convert 0 t)))
      ;; Probably a first-time run
      (el-job--split-evenly items n))
     (t
      (let ((max-per-core (/ (float-time total-duration) n))
            (this-sublist-sum 0)
            sublists
            this-sublist
            untimed
            dur)
        (catch 'filled
          (while-let ((item (pop items)))
            (if (length= sublists n)
                (progn (push item items)
                       (throw 'filled t))
              (setq dur (gethash item table))
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
        (if (length= sublists 0)
            (progn
              (fset 'el-job--split-optimally 'el-job--split-evenly)
              (error "el-job: Unexpected code path, report appreciated! Data: %S"
                     (list 'n n
                           'total-duration total-duration
                           'max-per-core max-per-core
                           'this-sublist-sum this-sublist-sum
                           'untimed untimed
                           'items items
                           'sublists sublists
                           'this-sublist this-sublist)))
          ;; Spread leftovers equally
          (let ((ctr 0)
                (len (length sublists)))
            (dolist (item (nconc this-sublist untimed items))
              (push item (nth
                          (% (cl-incf ctr) len)
                          sublists)))))
        sublists)))))

(defun el-job--split-evenly (big-list n &optional _)
  "Split BIG-LIST into a list of up to N sublists.

In the unlikely case where BIG-LIST contains N or fewer elements,
the result looks just like BIG-LIST except that
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

(defun el-job--zip-all (meta-lists)
  "Destructively zip all META-LISTS into one.
See subroutine `el-job-child--zip' for details."
  (let ((merged (pop meta-lists)))
    (while meta-lists
      (setq merged (el-job-child--zip (pop meta-lists) merged)))
    merged))


;;; Main logic:

;; If you use org-node, you can compare these methods' perfs on your machine.
;; 1. Eval: (progn (setq el-job-default-method 'poll) (el-job-kill-all))
;; 2. Do a few times: M-x org-node-reset
;; 3. Change the first eval form to a different method and repeat
(defvar el-job-default-method
  (if (bound-and-true-p fast-read-process-output) ;; emacs 30
      'change-hook
    'reap)
  "Method of getting output from subprocesses.
Three settings possible:

- `change-hook': Default on Emacs 30+.  Use `after-change-functions' in
                 each process buffer and watch for a newline.

- `reap': Default on Emacs <=29.  Tell the processes to die after one
          run, so process sentinels can collect the output.

- `poll': Keep the processes alive, and poll for finished output using
          a simple timer.  Appears relatively performant on Emacs <=29.

If you change this setting, remember to run \\[el-job-kill-all].")

;; TODO: Reuse in methods other than poll.  Maybe when launch detects busy.
(defvar el-job--global-timeout 15.0
  "Max wait-delay for `el-job--poll' after which it should give up.
Note that total wait time will be perhaps the double or triple; this is
only the max interval between two polls.")

(defvar el-job--cores nil
  "Max amount of processes to spawn for one job.
Usually the number of logical cores on your machine minus 1.")

(defvar el-jobs (make-hash-table :test #'eq)
  "Table of all el-job objects.")

(defun el-job--launch-anonymous ( load
                                  inject-vars
                                  funcall
                                  inputs
                                  callback )
  "Launch an anonymous job.
See `el-job-launch' for arguments."
  (let* ((id (intern (format-time-string "%FT%H%M%S%N")))
         (job (puthash id (el-job--make :id id
                                        :anonymous t
                                        :method 'reap
                                        :cores el-job--cores
                                        :callback callback
                                        :queue inputs)
                       el-jobs)))
    (el-job--spawn-processes job load inject-vars funcall)
    (el-job--exec job)))

(defmacro el-job--with (job slots &rest body)
  "Make SLOTS expand into object accessors for el-job JOB inside BODY.
Cf. `with-slots' in the eieio library, or `let-alist'.

For clarity inside BODY, each symbol name in SLOTS must be prepended
with one character of your choosing, such as a dot."
  (declare (indent 2) (debug ((&rest (symbolp sexp)))))
  `(cl-symbol-macrolet
       ,(cl-loop
         for slot in slots
         collect `(,slot (,(intern (concat "el-job:"
                                           (substring (symbol-name slot) 1)))
                          ,job)))
     ,@body))

(cl-defstruct (el-job (:constructor el-job--make)
                      (:copier nil)
                      (:conc-name el-job:))
  id
  anonymous
  (method el-job-default-method :documentation "See `el-job-default-method'.")
  (sig 0)
  (cores 1)
  callback
  (ready nil :documentation "Processes ready for input.  Becomes nil permanently if METHOD is `reap'.")
  (busy nil :documentation "Processes that have not yet returned output.")
  stderr
  (timestamps (list :accept-launch-request (current-time)))
  (poll-timer (timer-create))
  (timeout (timer-create))
  finish-times
  (past-elapsed (make-hash-table :test #'equal))
  spawn-args
  input-sets
  queue
  results)

;;;###autoload
(cl-defun el-job-launch (&key load
                              inject-vars
                              eval-once ;; REMOVED 2025-02-24
                              funcall
                              inputs
                              callback
                              id
                              if-busy
                              wrapup ;; RENAMED 2025-02-24 to callback
                              skip-benchmark ;; REMOVED 2025-02-24
                              method)
  "Run FUNCALL in one or more headless Elisp processes.
Then merge the return values \(lists of N lists) into one list
\(of N lists) and pass it to CALLBACK.

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
fixed-in-advance number of sub-lists, enabling this merge.  Of course,
these sub-lists are allowed to be empty, i.e. nil.

Alternatively, FUNCALL may always return nil.


FUNCALL is a function symbol known to be defined in an Emacs Lisp file.
It is the only mandatory argument, but rarely useful on its own.

Usually, you would need to pass the symbol LOAD to indicate where to
find that Emacs Lisp file; that file should end with a `provide' call on
the same symbol.  LOAD can also be a list of several symbols.

While subprocesses do not inherit `load-path', it is the mother Emacs
process that locates that file \(by inspecting `load-history', via
`el-job--ensure-compiled-lib'), then gives the file to the subprocess.

Due to the absence of `load-path', be careful writing `require'
statements into that Emacs Lisp file.  You can pass `load-path' via
INJECT-VARS, but consider that fewer dependencies means faster spin-up.


INPUTS is a list that will be split by up to the output of
`num-processors', and this determines how many subprocesses will spawn.
If INPUTS is omitted, only one subprocess will spawn.

The subprocesses have no access to current Emacs state.  The only way
they can affect current state, is if FUNCALL returns data, which is then
handled by CALLBACK function in the current Emacs.

Emacs stays responsive to user input up until all subprocesses finish,
which is when their results are merged and CALLBACK is executed.

CALLBACK receives two arguments: the results as mentioned before, and the
job object.  The latter is mainly useful to check timestamps,
which you can get from this form:

    \(el-job:timestamps JOB)


ID identifies this job, and is a symbol, a keyword or an integer below
536,870,911 \(suitable for `eq').  A non-nil ID has several purposes:

- Allow the processes to stay alive in the background after completion,
  to skip spin-up time on next call.
  May make a difference if they load a lot of libraries.
  This does not apply if METHOD is `reap'. See `el-job-default-method'.

- Allow repeated calls on the same inputs to optimize how those inputs
  are split, thanks to benchmarks from previous calls.

- The associated process buffers stick around and can be inspected for
  debugging purposes.  Seek buffer names that start with \" *el-job-\"
  \(note leading space).

- Prevent launching the same job twice, if the last invocation is not
  done yet.  Argument IF-BUSY regulates what happens instead, see below.


IF-BUSY comes into effect when the previous launch with the same ID is
still at work.  IF-BUSY may take on one of three symbols:

- `wait' \(default): append the inputs to a queue, to be handled
                     after all children are ready
- `noop': do nothing, drop inputs
- `takeover': kill and restart with the new inputs"
  ;; TODO: Uncomment these warnings sometime March 2025
  ;; (when skip-benchmark
  ;;   (message "el-job-launch: Obsolete argument :skip-benchmark does nothing"))
  ;; (when eval-once
  ;;   (message "el-job-launch: Obsolete argument :eval-once does nothing"))
  (when wrapup
    ;; (message "el-job-launch: Obsolete argument :wrapup interpreted as :callback")
    (setq callback wrapup))
  (unless el-job--cores
    (setq el-job--cores (max 1 (1- (num-processors)))))
  (setq load (ensure-list load))
  (setq if-busy (or if-busy 'wait))
  (unless (and (symbolp funcall) (functionp funcall))
    (error "Argument FUNCALL must be a symbol with a function definition"))
  (when callback
    (unless (and (symbolp callback) (functionp callback))
      (error "Argument CALLBACK must be a symbol with a function definition")))
  (if (null id)
      (el-job--launch-anonymous load inject-vars funcall inputs callback)
    (let ((arg-signature (+ (sxhash load)
                            (sxhash inject-vars)
                            (sxhash callback) ;; TODO: permit changing it
                            (sxhash method)
                            (sxhash funcall)))
          (job (or (gethash id el-jobs)
                   (puthash id (el-job--make
                                :id id :method (or method el-job-default-method))
                            el-jobs)))
          (respawn nil)
          (exec nil))
      (el-job--with job
          (.queue .busy .ready .sig .cores .method .spawn-args)
        (unless (and .busy (eq if-busy 'noop))
          (when (functionp inputs)
            (setq inputs (funcall inputs)))
          (if .busy
              (pcase if-busy
                ('takeover (setq respawn t)
                           (setq exec t)
                           (setf .queue inputs))
                ('wait (setf .queue (append inputs .queue))))
            (setf .queue inputs)
            (setq exec t))
          (when exec
            ;; Only increment to e.g. 7 standby processes if it was ever called
            ;; with 7+ inputs at the same time
            (when (< .cores el-job--cores)
              (setf .cores (min el-job--cores (max .cores (length .queue)))))
            (when (eq .method 'reap)
              (setq respawn t))
            (when (or (eq .method 'change-hook)
                      (eq .method 'poll))
              ;; TODO: Skip this check, just react on `process-send-string' fail
              (unless (and (= .cores (+ (length .busy) (length .ready)))
                           (seq-every-p #'process-live-p .ready)
                           (seq-every-p #'process-live-p .busy))
                (el-job--dbg 1 "Found dead processes, resetting job %s" id)
                (setq respawn t)))
            (setq arg-signature (+ arg-signature .cores))
            (when (/= .sig arg-signature)
              (setf .sig arg-signature)
              (setf .spawn-args (list job load inject-vars funcall))
              (el-job--dbg 2 "New arguments, resetting job %s" id)
              (setq respawn t))
            (setf .callback callback)
            (when respawn
              (el-job--terminate job)
              (when method
                (setf .method method))
              (el-job--spawn-processes job load inject-vars funcall))
            (el-job--exec job)
            t))))))

(defvar-local el-job-here nil)
(defun el-job--spawn-processes (job load inject-vars funcall)
  "Spin up processes for JOB, standing by for input.
For the rest of the arguments, see `el-job-launch'."
  (el-job--with job (.stderr .id .cores .ready .method)
    (let* ((print-length nil)
           (print-level nil)
           (print-circle t)
           (print-symbols-bare t)
           (print-escape-newlines t)
           (print-escape-nonascii t) ;; Prolly unnecessary
           (vars (prin1-to-string
                  (cl-loop for var in inject-vars
                           if (symbolp var)
                           collect (cons var (symbol-value var))
                           else collect var)))
           (libs (prin1-to-string (mapcar #'el-job--ensure-compiled-lib load)))
           (command
            (list
             (file-name-concat invocation-directory invocation-name)
             "--quick"
             "--batch"
             "--load" (el-job--ensure-compiled-lib 'el-job-child)
             "--eval" (format "(el-job-child--work #'%S)" funcall)))
           ;; Ensure the working directory is not remote (messes things up)
           (default-directory invocation-directory)
           proc)
      (setf .stderr
            (with-current-buffer
                (get-buffer-create (format " *el-job-%s:err*" .id) t)
              (erase-buffer)
              (current-buffer)))
      (condition-case err
          (dotimes (i .cores)
            (setq proc (make-process
                        :name (format "el-job:%s:%d" .id i)
                        :noquery t
                        :connection-type 'pipe
                        ;; https://github.com/jwiegley/emacs-async/issues/165
                        :coding 'utf-8-emacs-unix
                        :stderr .stderr
                        :buffer (get-buffer-create (format " *el-job-%s:%d*" .id i) t)
                        :command command
                        :sentinel #'ignore))
            (when (string-suffix-p ">" (process-name proc))
              (el-job--dbg 1 "Unintended duplicate process id for %s" proc))
            (with-current-buffer (process-buffer proc)
              (setq-local el-job-here job)
              (pcase .method
                ('change-hook (add-hook 'after-change-functions
                                        #'el-job--handle-output-in-buffer-if-done nil t))
                ('reap (set-process-sentinel proc #'el-job--sentinel))))
            (process-send-string proc vars)
            (process-send-string proc "\n")
            (process-send-string proc libs)
            (process-send-string proc "\n")
            (push proc .ready))
        ;; https://github.com/meedstrom/org-node/issues/75
        (( file-error )
         (el-job--terminate job)
         (error "el-job: Terminated job because of %S" err))))))

(defun el-job--exec (job)
  "Split the queued inputs in JOB and pass to all children.

This puts them to work.  Each successful child will print output
\(even nil output) to its associated process buffer, whereupon something
should trigger `el-job--handle-output'."
  (el-job--with job
      ( .ready .busy .input-sets .results .queue .cores .past-elapsed
        .timestamps .poll-timer .finish-times .anonymous .method
        .id .timeout )
    (cancel-timer .timeout)
    (setf .results nil)
    (setf .finish-times nil)
    (let ((splits (el-job--split-optimally .queue .cores .past-elapsed)))
      (unless (length< splits (1+ (length .ready)))
        (error "Items split in %d lists, but only %d ready processes"
               (length splits) (length .ready)))
      (let ((print-length nil)
            (print-level nil)
            (print-circle t)
            (print-symbols-bare t)
            (print-escape-newlines t)
            items proc)
        (while splits
          (setq items (pop splits))
          (cl-assert .ready)
          (setq proc (pop .ready))
          (push proc .busy)
          (setf (alist-get proc .input-sets) items)
          (with-current-buffer (process-buffer proc)
            (erase-buffer)
            (process-send-string proc (prin1-to-string items))
            (process-send-string proc "\n")
            (when (eq .method 'reap)
              (process-send-string proc "die\n"))))))
    (setf .queue nil)
    (plist-put .timestamps :launched (current-time))
    (setf .timeout (run-with-timer 30 nil #'el-job--timeout .id))
    (when (eq .method 'poll)
      (cancel-timer .poll-timer)
      (setf .poll-timer
            (run-with-timer 0.1 nil #'el-job--poll .busy .poll-timer 0.1)))))

(defun el-job--timeout (id)
  "Terminate job by ID, and print that it timed out."
  (let ((job (gethash id el-jobs)))
    (if (and job (el-job:busy job))
        (progn
          (el-job--terminate job)
          (message "el-job: Timed out, was busy for 30+ seconds: %s"
                   (el-job:id job)))
      (el-job--dbg 1
          "Timeout timer should have been cancelled for el-job ID %s" id))))

(defun el-job--poll (procs timer delay)
  "Try to run `el-job--handle-output' in each buffer associated with PROCS.

If any processes were not done yet, reassign the timer object TIMER to
call this function again after DELAY seconds, upped by 50%.  Pass the
increased delay along, so that it keeps increasing each time."
  (setq procs (cl-loop for busy in procs
                       unless (with-current-buffer (process-buffer busy)
                                (el-job--handle-output-in-buffer-if-done))
                       collect busy))
  (when procs
    (if (> delay el-job--global-timeout)
        (progn
          (el-job--dbg 0
              "Took too long (over %d seconds), killing processes.
If you see this during development, either override `el-job--global-timeout'
or check what is causing FUNCALL to never return.
Processes killed: %S" (truncate (* 2 el-job--global-timeout)) procs)
          (mapc #'el-job--kill-quietly procs))
      (setq delay (* delay 1.5))
      (timer-set-time timer (time-add delay (current-time)))
      (timer-set-function timer #'el-job--poll (list procs timer delay))
      (timer-activate timer))))

(defun el-job--sentinel (proc event)
  "Handle the output in buffer of finished process PROC.
For arguments PROC and EVENT, see Info node `(elisp) Sentinels'."
  (with-current-buffer (process-buffer proc)
    (if (and (equal event "finished\n")
             (eq (process-status proc) 'exit)
             (eq (process-exit-status proc) 0))
        (el-job--handle-output proc)
      (el-job--unhide-buffer (current-buffer))
      (el-job--unhide-buffer (el-job:stderr el-job-here))
      (message "Child had problems, check buffer %s" (buffer-name)))))

(defun el-job--handle-output-in-buffer-if-done (&rest _)
  "Handle output in current buffer if it appears complete.
Can be called in a process buffer at any time."
  (if (eq (char-before) ?\n)
      (el-job--handle-output)))

(defun el-job--handle-output (&optional proc)
  "Handle output in current buffer.

If this is the last output for the job, merge all outputs, maybe execute
the callback function, finally maybe run the job again if there is now
more input in the queue.

Argument PROC, if provided, should be the corresponding process.
If nil, infer it from the buffer, if process is still alive."
  (let* ((inhibit-quit t)
         (proc (or proc (get-buffer-process (current-buffer))))
         (job el-job-here)
         (output (condition-case-unless-debug err (read (buffer-string))
                   (( error )
                    (el-job--unhide-buffer (el-job:stderr job))
                    (dolist (proc (el-job--all-processes job))
                      (el-job--unhide-buffer (process-buffer proc))
                      (el-job--kill-quietly-keep-buffer proc))
                    (error "In buffer %S: problems reading child output: %S"
                           (current-buffer) err)))))
    (when output
      (el-job--with job
          ( .busy .ready .input-sets .past-elapsed .results .queue
            .timestamps .id .temp-hook .anonymous .method .finish-times
            .timeout )
        (push (caar output) .finish-times)
        ;; Record time spent by FUNCALL on each item in INPUTS,
        ;; for a better `el-job--split-optimally' in the future.
        (let ((durations (cdar output))
              (input (alist-get proc .input-sets)))
          (while durations
            (puthash (pop input) (pop durations) .past-elapsed)))
        ;; The `car' was just this library's metadata
        (push (cdr output) .results)
        (setf .busy (delq proc .busy))
        (unless (eq .method 'reap)
          (push proc .ready))

        ;; Extra actions when this was the last output
        (when (null .busy)
          (plist-put .timestamps :children-done
                     (car (last (sort .finish-times #'time-less-p))))
          ;; TODO: Rename this timestamp, I feel it's not intuitive.
          ;;       Maybe :callback-begin?
          (plist-put .timestamps :got-all-results (current-time))
          ;; Cleanup
          (cancel-timer .timeout)
          (when .anonymous
            (el-job--terminate job)
            (remhash .id el-jobs))
          ;; Finally the purpose of it all.
          ;; Did this really take 700 lines of code?
          (setf .results (el-job--zip-all .results))
          (when .callback (funcall .callback .results job))
          (when .queue
            ;; There's more in the queue, run again at next good opportunity.
            (when (eq .method 'reap)
              (el-job--terminate job)
              (apply #'el-job--spawn-processes (el-job:spawn-args job)))
            (el-job--exec job))))))
  t)

(defun el-job--terminate (job)
  "Kill processes in JOB and revert some state variables.
This kills all process buffers, but does not deregister the ID from
`el-jobs' nor clear queued input."
  (cancel-timer (el-job:poll-timer job))
  (cancel-timer (el-job:timeout job))
  (mapc #'el-job--kill-quietly (el-job:busy job))
  (mapc #'el-job--kill-quietly (el-job:ready job))
  (setf (el-job:busy job) nil)
  (setf (el-job:ready job) nil)
  (when-let ((stderr (el-job:stderr job)))
    (kill-buffer stderr)))

(defun el-job--unhide-buffer (buffer)
  "Rename BUFFER to omit initial space, and return the new name."
  (with-current-buffer buffer
    (rename-buffer (string-trim-left (buffer-name)))))

(defun el-job--kill-quietly-keep-buffer (proc)
  "Kill PROC while disabling its sentinel and filter.
See `el-job--kill-quietly' to also kill the buffer."
  (set-process-filter proc #'ignore)
  (set-process-sentinel proc #'ignore)
  (delete-process proc))

(defun el-job--kill-quietly (proc)
  "Delete process PROC and kill its buffer.
Prevent its sentinel and filter from reacting."
  (let ((buf (process-buffer proc)))
    (el-job--kill-quietly-keep-buffer proc)
    (kill-buffer buf)))


;;; Tools

(defun el-job--all-processes (job)
  "Return all processes for JOB, busy and ready."
  (append (el-job:busy job) (el-job:ready job)))

(defun el-job-show ()
  "Prompt for a job and show its metadata in a new buffer."
  (interactive)
  (let* ((id (intern (completing-read "Get info on job: " el-jobs)))
         (job (gethash id el-jobs)))
    (when job
      (switch-to-buffer (get-buffer-create "*el-job*" t))
      (erase-buffer)
      (cl-prin1 job (current-buffer))
      ;; Never print the above into echo area
      t)))

(defun el-job-kill-all ()
  "Kill all el-jobs and forget metadata."
  (interactive)
  (maphash (lambda (id job)
             (el-job--terminate job)
             (remhash id el-jobs))
           el-jobs))

(defun el-job-await (id timeout &optional message)
  "Block until all processes for job ID finished, then return t.

If the job has still not finished after TIMEOUT seconds, stop
blocking and return nil.

Meanwhile, ensure string MESSAGE is visible in the minibuffer."
  (let ((deadline (time-add (current-time) timeout)))
    (catch 'timeout
      (while (el-job-is-busy id)
        (discard-input)
        (if (time-less-p (current-time) deadline)
            (progn (unless (current-message) (message message))
                   (sit-for 0.1))
          (throw 'timeout nil)))
      t)))

(defun el-job-is-busy (id)
  "Return list of busy processes for job ID, if any.
Safely return nil otherwise, whether or not ID is known."
  (when-let ((job (gethash id el-jobs)))
    (el-job:busy job)))

(define-obsolete-function-alias 'el-job--await 'el-job-await "2024-12-29")

(provide 'el-job)

;;; el-job.el ends here

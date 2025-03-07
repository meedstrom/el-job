;;; el-job.el --- Contrived way to call a function using all CPU cores -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Martin Edström
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
;; URL:              https://github.com/meedstrom/el-job
;; Created:          2024-10-30
;; Keywords:         processes
;; Package-Version:  2.1.0
;; Package-Requires: ((emacs "29.1"))

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
(require 'el-job-child)

(defvar el-job-major-version 2
  "Number incremented for breaking changes.")


;;; Subroutines

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

(defvar el-job--onetime-canary nil)
(defun el-job--ensure-compiled-lib (feature)
  "Look for the .eln, .elc or .el file corresponding to FEATURE.
FEATURE is a symbol such as those seen in `features'.

See `el-job--locate-lib-in-load-history'.

If it is .el, then opportunistically compile it and return the newly
compiled file instead.  This returns an .elc on the first call, then
most likely an .eln on future calls.

Note: if you are currently editing the source code for FEATURE, save
that file of source code and use \\[eval-buffer] to ensure this will
find the correct file."
  (let ((loaded (el-job--locate-lib-in-load-history feature)))
    (unless loaded
      (error "Current Lisp definitions must come from a file %S[.el/.elc/.eln]"
             feature))
    ;; HACK: Sometimes comp.el makes freefn- temp files.  It sounds like we
    ;;       would not normally see it unless user is evalling defuns in a
    ;;       scratch buffer, but not sure.  Signal the first time this happens,
    ;;       then fall back on load-path.
    (when (string-search "freefn-" loaded)
      (unless el-job--onetime-canary
        (setq el-job--onetime-canary t)
        (error "Could not find real file for feature %S, found %s"
               feature loaded))
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
                ;; compiling now can result in an .eln in ~/.emacs.d/ that will
                ;; always take precedence over the one shipped by Guix.
                ;; If we want to cover for that, it'd be safer to compile into
                ;; /tmp with a filename based on e.g. `after-init-time'.
                ;; Users who install FEATURE thru Guix are prolly safe.
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

(defun el-job--split-optimally (items n-cores benchmarks)
  "Split ITEMS into up to N-CORES lists of items.

For all keys in table BENCHMARKS that match one of ITEMS, assume the
value holds a benchmark \(a Lisp time value) for how long it took in the
past to pass this item through the FUNCALL-PER-INPUT function specified
by `el-job-launch'.

Use these benchmarks to rebalance the lists so that each sub-list should
take around the same amount of wall-time to work through.

This reduces the risk that one child takes markedly longer due to
being saddled with a huge item in addition to the average workload."
  (let ((total-duration 0))
    (cond
     ((= n-cores 1)
      (list items))
     ((length< items (1+ n-cores))
      (el-job--split-evenly items n-cores))
     ((progn
        (dolist (item items)
          (let ((dur (gethash item benchmarks)))
            (when dur
              (setq total-duration (time-add total-duration dur)))))
        (eq total-duration 0))
      ;; Probably a first-time run
      (el-job--split-evenly items n-cores))
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
        (if (length= sublists 0)
            (progn ;; Degrade gracefully
              (fset 'el-job--split-optimally #'el-job--split-evenly)
              (cl-assert (not (length= sublists 0))))
          ;; Spread leftovers equally
          (let ((ctr 0)
                (len (length sublists)))
            (dolist (item (nconc this-sublist untimed items))
              (push item (nth
                          (% (cl-incf ctr) len)
                          sublists)))))
        sublists)))))

(defun el-job--zip-all (meta-lists)
  "Destructively zip all META-LISTS into one.
See subroutine `el-job-child--zip' for details."
  (let ((merged (pop meta-lists)))
    (while meta-lists
      (setq merged (el-job-child--zip (pop meta-lists) merged)))
    merged))


;;; Main logic

(defvar el-jobs (make-hash-table :test #'eq)
  "Table of all el-job objects.")

(defmacro el-job--with (job slots &rest body)
  "Make SLOTS expand into object accessors for `el-job' JOB inside BODY.
Cf. `with-slots' in the eieio library, or `let-alist'.

For clarity inside BODY, each symbol name in SLOTS must be prepended
with one character of your choosing, such as a dot."
  (declare (indent 2))
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
  callback
  (n-cores-to-use 1)
  (ready nil :documentation "Processes ready for input.")
  (busy nil :documentation "Processes that have not yet returned output.")
  stderr
  ;; Not an interesting timestamp, but `plist-put' needs a non-empty list.
  (timestamps (list :initial-job-creation (current-time)))
  (poll-timer (timer-create))
  finish-times
  spawn-args
  (past-elapsed (make-hash-table :test #'equal))
  queued-inputs
  input-sets
  result-sets
  merged-results)

;;;###autoload
(cl-defun el-job-launch (&key id
                              (if-busy 'wait)
                              load-features
                              inject-vars
                              inputs
                              funcall-per-input
                              callback)
  "Run FUNCALL-PER-INPUT in one or more headless Elisp processes.
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

which is why it's important that FUNCALL-PER-INPUT always returns a list
with a fixed-in-advance number of sub-lists, enabling this merge.
These sub-lists are allowed to be empty, i.e. nil, but not absent.

The fixed-in-advance number can also be zero, i.e. FUNCALL-PER-INPUT may
be designed to always return nil.


FUNCALL-PER-INPUT is a symbol known to be defined in some Emacs Lisp
file as a function of one argument.

Usually, it would be a function you have written yourself, and you pass
LOAD-FEATURES to indicate where to find that Emacs Lisp file, plus any
dependencies not built into Emacs.

LOAD-FEATURES is a list of symbols like those in `features'\; the files
in question should end with a `provide' call on the same symbols.

The subprocesses do not inherit `load-path', it is the current Emacs
process that locates files \(by inspecting `load-history', via
`el-job--ensure-compiled-lib'), then gives them to each subprocess.


INPUTS is a list that will be split by up to the output of
`num-processors', and this determines how many subprocesses will spawn.
If INPUTS is omitted, only one subprocess will spawn.

INPUTS can also be a function that returns a list.  In this case, the
function is deferred until needed, possibly saving on compute.


The subprocesses have no access to current Emacs state.  The only way
they can affect current state, is if FUNCALL-PER-INPUT returns data,
which is then handled by CALLBACK function in the current Emacs.

Emacs stays responsive to user input up until all subprocesses finish,
which is when their results are merged and CALLBACK is executed.

CALLBACK receives two arguments: the results as mentioned before, and the
job object.  The latter is mainly useful to check timestamps,
which you can get from this form:

    \(el-job:timestamps JOB)


ID is a symbol identifying this job.  It has several purposes:

- Prevent launching the same job twice, if the last invocation is not
  done yet.  Argument IF-BUSY regulates what happens instead.

- Allow repeated calls on the same inputs to optimize how those inputs
  are split, thanks to benchmarks from previous calls.


IF-BUSY comes into effect when the previous launch with the same ID is
still at work.  IF-BUSY may take on one of three symbols:

- `wait' \(default): append the inputs to a queue, to be handled
                     after all children are ready
- `noop': do nothing, drop inputs
- `takeover': kill and restart with the new inputs

For debugging, see these commands:
- `el-job-cycle-debug-level'
- `el-job-show-info'
- `el-job-kill-all'"
  (unless (and (symbolp funcall-per-input)
               (functionp funcall-per-input))
    (error "Argument FUNCALL-PER-INPUT must be a symbol with a function definition"))
  (when callback
    (unless (and (symbolp callback)
                 (functionp callback))
      (error "Argument CALLBACK must be a symbol with a function definition")))
  (unless (proper-list-p load-features)
    (error "Argument LOAD-FEATURES must be a list"))
  (unless id (error "Argument ID now mandatory"))
  (let ((job (or (gethash id el-jobs)
                 (puthash id (el-job--make :id id) el-jobs)))
        (do-respawn nil)
        (do-exec nil))
    (el-job--with job ( .queued-inputs .busy .ready .n-cores-to-use
                        .spawn-args .callback .timestamps )
      (unless (and .busy (eq if-busy 'noop))
        (plist-put .timestamps :launched (current-time))
        ;; TODO: Can we somehow defer this to even later?
        ;;       Maybe if-busy=wait could inhibit funcalling it?
        (when (functionp inputs)
          (setq inputs (funcall inputs)))
        (if .busy
            (pcase if-busy
              ('takeover (setq do-respawn t)
                         (setq do-exec t)
                         (setf .queued-inputs inputs))
              ('wait (setf .queued-inputs (append inputs .queued-inputs))))
          (setf .queued-inputs inputs)
          (setq do-exec t))
        (when do-exec
          (setf .callback callback)
          ;; Prevent spawning a dozen processes when you'll use only one or two
          (let ((machine-cores (max 1 (1- (num-processors)))))
            (setf .n-cores-to-use (if (length< .queued-inputs machine-cores)
                                      (length .queued-inputs)
                                    machine-cores))
            (when (or (length< .ready .n-cores-to-use)
                      (not (seq-every-p #'process-live-p .ready)))
              (setq do-respawn t)))
          (let ((new-spawn-args (list job
                                      load-features
                                      inject-vars
                                      funcall-per-input)))
            (unless (= (sxhash (cdr .spawn-args))
                       (sxhash (cdr new-spawn-args)))
              (setf .spawn-args new-spawn-args)
              (el-job--dbg 2 "New arguments, resetting processes for %s" id)
              (setq do-respawn t)))
          (when do-respawn
            (el-job--disable job)
            (apply #'el-job--spawn-processes .spawn-args))
          (el-job--exec-workload job))))))

(defvar-local el-job-here nil)
(defun el-job--spawn-processes (job load-features inject-vars funcall-per-input)
  "Spin up processes for JOB, standing by for input.
For arguments LOAD-FEATURES INJECT-VARS FUNCALL-PER-INPUT,
see `el-job-launch'."
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
         (libs (prin1-to-string
                (mapcar #'el-job--ensure-compiled-lib load-features)))
         (command
          (list
           (file-name-concat invocation-directory invocation-name)
           "--quick"
           "--batch"
           "--load" (el-job--ensure-compiled-lib 'el-job-child)
           "--eval" (format "(el-job-child--work #'%S)" funcall-per-input)))
         ;; Ensure the working directory is not remote (it messes things up)
         (default-directory invocation-directory))
    (el-job--with job (.stderr .id .ready .spawn-args .n-cores-to-use)
      (setf .stderr
            (with-current-buffer
                (get-buffer-create (format " *el-job:%s:err*" .id) t)
              (setq-local el-job-here job)
              (erase-buffer)
              (current-buffer)))
      (condition-case err
          (dotimes (i .n-cores-to-use)
            (let ((proc (make-process
                         :name (format "el-job:%s:%d" .id i)
                         :noquery t
                         :connection-type 'pipe
                         ;; https://github.com/jwiegley/emacs-async/issues/165
                         :coding 'utf-8-emacs-unix
                         :stderr .stderr
                         :buffer (get-buffer-create
                                  (format " *el-job:%s:%d*" .id i) t)
                         :command command
                         :sentinel #'ignore)))
              (when (string-suffix-p ">" (process-name proc))
                (el-job--dbg 1 "Unintended duplicate process id for %s" proc))
              (with-current-buffer (process-buffer proc)
                (setq-local el-job-here job)
                (process-send-string proc vars)
                (process-send-string proc "\n")
                (process-send-string proc libs)
                (process-send-string proc "\n"))
              (push proc .ready)))
        ;; https://github.com/meedstrom/org-node/issues/75
        (( file-error )
         (el-job--disable job)
         (el-job--dbg 1 "el-job: Terminated job because of: %S" err))))))

(defun el-job--exec-workload (job)
  "Split the queued inputs in JOB and pass to all children.

This puts them to work.  Each successful child will print output
\(even nil output) to its associated process buffer, whereupon something
should trigger `el-job--handle-output'."
  (el-job--with job
      ( .ready .busy .input-sets .result-sets .queued-inputs .n-cores-to-use
        .past-elapsed .timestamps .finish-times .id .stderr .poll-timer )
    (cancel-timer .poll-timer)
    (setf .input-sets nil)
    (setf .result-sets nil)
    (setf .finish-times nil)
    (let ((splits (el-job--split-optimally .queued-inputs
                                           .n-cores-to-use
                                           .past-elapsed))
          busy-bufs)
      (unless (length< splits (1+ (length .ready)))
        (error "Items split in %d lists, but only %d ready processes"
               (length splits) (length .ready)))
      (let ((print-length nil)
            (print-level nil)
            (print-circle t)
            (print-symbols-bare t)
            (print-escape-newlines t)
            items proc)
        (while (progn
                 (setq items (pop splits))
                 (setq proc (pop .ready))
                 (push proc .busy)
                 (push (process-buffer proc) busy-bufs)
                 (setf (alist-get proc .input-sets) items)
                 (with-current-buffer (process-buffer proc)
                   (erase-buffer)
                   (process-send-string proc (prin1-to-string items))
                   (process-send-string proc "\n"))
                 splits)))
      (setf .queued-inputs nil)
      (plist-put .timestamps :work-begun (current-time))
      (setf .poll-timer (run-with-timer .02 nil #'el-job--poll 1 busy-bufs)))))

;; Polling: simplistic but reliable.

;; Had the clever idea to add a hook to `after-change-functions' in each
;; process buffer, that checks (eq (char-before) ?\n).  Perf was good on my
;; machine...on Emacs 30, bad on 29.  Plus it just seems the kinda design that
;; invites variance from machine to machine.

;; So, poll.  We do a chain of timers that successively ups the delay.
;; To see what the delays would be, eval:
;; (--map (/ it (float (ash 1 5))) (-iterate '1+ 1 42))

;; And to see the cumulative sums:
;; (-reductions '+ (--map (/ it (float (ash 1 5))) (-iterate '1+ 1 42)))

;; As you can see, we do 7 polls inside the first second,
;; but spread out the last 7 polls between T-minus-20s and T-minus-30s.

(defun el-job--poll (n bufs)
  "Check process buffers BUFS for complete output.
For each where it is complete, handle it.  For the rest, check again
after a short delay.  N is the count of checks done so far."
  (cl-assert (not (null bufs)))
  (let (busy-bufs)
    (save-current-buffer
      (dolist (buf bufs)
        (if (not (buffer-live-p buf))
            (el-job--dbg 2 "Dead process buffer (this may be normal)")
          (set-buffer buf)
          (if (eq (char-before) ?\n)
              (el-job--handle-output)
            (push buf busy-bufs))))
      (cl-assert el-job-here)
      (if (and busy-bufs (<= n 42))
          (setf (el-job:poll-timer el-job-here)
                (run-with-timer
                 (/ n (float (ash 1 5))) nil #'el-job--poll (1+ n) busy-bufs))
        (let ((id (el-job:id el-job-here)))
          (el-job--disable el-job-here)
          (if busy-bufs
              (message "el-job: Timed out, was busy for 30+ seconds: %s" id)
            (el-job--dbg 2 "Reaped idle processes for %s" id)))))))

(defun el-job--handle-output ()
  "Handle output in current buffer.

If this is the last output for the job, merge all outputs, maybe execute
the callback function, finally maybe run the job again if there is now
more input in the queue."
  (let* ((inhibit-quit t)
         (proc (get-buffer-process (current-buffer)))
         (job el-job-here)
         finish-time
         durations
         results)
    (condition-case err (let ((output (read (buffer-string))))
                          (setq finish-time (caar output))
                          (setq durations (cdar output))
                          (setq results (cdr output)))
      (( error )
       (el-job--unhide-buffer (el-job:stderr job))
       (dolist (proc (append (el-job:busy job)
                             (el-job:ready job)))
         (el-job--unhide-buffer (process-buffer proc))
         (delete-process proc))
       (error "In buffer %s: problems reading child output: %S"
              (current-buffer) err)))
    (when results
      (el-job--with job
          ( .busy .ready .input-sets .past-elapsed .result-sets .queued-inputs
            .timestamps .id .finish-times .callback .merged-results )
        (push finish-time .finish-times)
        ;; Record time spent by FUNCALL-PER-INPUT on each item in INPUTS,
        ;; for a better `el-job--split-optimally' in the future.
        (let ((inputs (alist-get proc .input-sets)))
          (while durations
            (puthash (pop inputs) (pop durations) .past-elapsed)))
        (push results .result-sets)
        (setf .busy (delq proc .busy))
        (push proc .ready)

        ;; Extra actions when this was the last output
        (when (null .busy)
          (let ((last-done (car (last (sort .finish-times #'time-less-p)))))
            (plist-put .timestamps :work-done last-done))
          (plist-put .timestamps :callback-begun (current-time))
          ;; Finally the purpose of it all.
          ;; Somehow, it took 700 lines of code to get here.
          (setf .merged-results (el-job--zip-all .result-sets))
          (when .callback
            (funcall .callback .merged-results job))
          (when .queued-inputs
            (el-job--exec-workload job)))))))

(defun el-job--disable (job)
  "Kill processes in JOB and their process buffers.

This does not deregister the job ID.  That means the next launch with
same ID still has the benchmarks table and possibly queued input."
  (el-job--with job (.id .busy .ready .stderr .poll-timer)
    (cancel-timer .poll-timer)
    (dolist (proc (append .busy .ready))
      (let ((buf (process-buffer proc)))
        (delete-process proc)
        (when (= 0 el-job--debug-level) (kill-buffer buf))))
    (when (= 0 el-job--debug-level) (kill-buffer .stderr))
    (setf .busy nil)
    (setf .ready nil)))

(defun el-job--unhide-buffer (buffer)
  "Rename BUFFER to omit initial space, and return the new name."
  (with-current-buffer buffer
    (rename-buffer (string-trim-left (buffer-name)))))


;;; Tools / public API

(defun el-job-cycle-debug-level ()
  "Increment `el-job--debug-level'."
  (interactive)
  (message "Variable `el-job--debug-level' set to %d"
           (setq el-job--debug-level (% (1+ el-job--debug-level) 3))))

(defun el-job-show-info ()
  "Prompt for a job and show its data in a new buffer.
Tip: alternatively, you can preserve the process buffers for inspection.
Use \\[el-job-cycle-debug-level] and they are not killed from then on."
  (interactive)
  (let* ((id (intern (completing-read "Get info on job: " el-jobs)))
         (job (gethash id el-jobs)))
    (when job
      (set-buffer (get-buffer-create "*el-job debug info*" t))
      (so-long-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (prin1 job (current-buffer)))
      (switch-to-buffer (current-buffer)))))

(defun el-job-kill-all ()
  "Kill all el-jobs ever registered and forget metadata."
  (interactive)
  (maphash (lambda (id job)
             (el-job--disable job)
             (remhash id el-jobs))
           el-jobs))

(defun el-job-await (id max-secs &optional message)
  "Block until all processes for job ID finished, then return t.

If the job has still not finished after MAX-SECS seconds, stop
blocking and return nil.

Meanwhile, ensure string MESSAGE is visible in the minibuffer."
  (let ((deadline (time-add (current-time) max-secs)))
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
  (let ((job (gethash id el-jobs)))
    (and job (el-job:busy job))))

(provide 'el-job)

;;; el-job.el ends here

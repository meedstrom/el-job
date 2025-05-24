;;; el-job.el --- Contrived way to call a function using all CPU cores -*- lexical-binding: t; -*-

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

;; Author:           Martin Edström <meedstrom@runbox.eu>
;; URL:              https://github.com/meedstrom/el-job
;; Created:          2024-10-30
;; Keywords:         processes
;; Package-Version:  2.4.7
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Imagine you have a function you'd like to run on a long list of inputs.
;; You could run (mapcar #'FN INPUTS), but that hangs Emacs until done.

;; This library lets you split up the inputs and run the function in many
;; subprocesses---one per CPU core---then merge their outputs and handle the
;; result as if it had been returned by that `mapcar'.  In the meantime,
;; current Emacs does not hang at all.

;; You do need to grok the concept of a callback.

;; Public API:
;; - Function `el-job-launch' (main entry point)
;; - Function `el-job-await'
;; - Function `el-job-is-busy'
;; - Variable `el-job-major-version'

;; Dev tools:
;; - Command `el-job-cycle-debug-level'
;; - Command `el-job-show-info'
;; - Command `el-job-kill-all'

;;; Code:

;; Wish-list:
;; - Figure out how to allow more than one list of INPUTS.  That'd give this
;;   library much wider applicability!

;; - Figure out how to use `dump-emacs-portable' after loading all desired
;;   features, so we can reuse that to start the subprocesses nigh-instantly.
;;   Could even simplify to a sentinel-based workflow then, no polling.

(require 'cl-lib)
(require 'el-job-child)

(defvar el-job-major-version 2
  "Number incremented for breaking changes.")


;;; Subroutines

(defvar el-job--debug-level 0
  "Increase this to 1 or 2 to see more debug messages.")

(defun el-job--dbg (level fmt &rest args)
  "Maybe pass FMT and ARGS to `message'.
LEVEL is the threshold that `el-job--debug-level' should meet or exceed
to unlock this message."
  (declare (indent 1))
  (when (<= level el-job--debug-level)
    (apply #'message (concat "el-job: " fmt) args)))

(defun el-job--locate-lib-in-load-history (feature)
  "Look for the .eln, .elc or .el file corresponding to FEATURE.
FEATURE is a symbol such as those seen in `features'.

Return whichever variant was in fact loaded by the current Emacs.

Unusually, as a programmer convenience, this looks in `load-history'
instead of `load-path', so the result can change after you use
`eval-buffer' in an .el file that you are editing: it will change to
return that .el file.

See wrapper `el-job--ensure-compiled-lib' for a convenient way to return
an .eln anyway, without your having to recompile on save."
  (cl-loop
   for (file . elems) in load-history
   ;; FIXME: If a file has more than one `provide', this only finds the first.
   when (eq feature (cdr (assq 'provide elems)))
   return
   ;; Look for a natively-compiled function supposedly defined in FILE.
   ;; It's the most reliable way to find the .eln.
   ;; FILE may be .el or .elc even if the .eln exists and is being used.
   (or (and (fboundp 'subrp)
            (fboundp 'native-comp-unit-file)
            (fboundp 'subr-native-comp-unit)
            (cl-loop for elem in elems
                     when (and (consp elem)
                               (eq 'defun (car elem))
                               (symbolp (cdr elem))
                               (subrp (symbol-function (cdr elem)))
                               ;; Extra safety (sometimes files contain a patch
                               ;; overriding some other file's definition)
                               (string-prefix-p (symbol-name feature)
                                                (file-name-nondirectory file)))
                     return (let ((eln (native-comp-unit-file
                                        (subr-native-comp-unit
                                         (symbol-function (cdr elem))))))
                              ;; FIXME: comp sometimes deletes old eln during
                              ;; recompilation, but does not load the new eln,
                              ;; at least not in a way that updates the
                              ;; `native-comp-unit-file'.  Current workaround
                              ;; is return nil, ie fall back on FILE; not
                              ;; ideal.
                              (and (file-exists-p eln)
                                   eln))))
       file)))

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
      (error "el-job: Current Lisp definitions must come from a file %S[.el/.elc/.eln]"
             feature))
    ;; HACK: Sometimes comp.el makes freefn- temp files.  It sounds like we
    ;; would not normally see it unless user is evalling defuns in a scratch
    ;; buffer, but not sure.  Signal the first time this happens, then fall
    ;; back on load-path.
    (when (string-search "freefn-" loaded)
      (unless el-job--onetime-canary
        (setq el-job--onetime-canary t)
        (error "el-job: Could not find real file for feature %S, found %s"
               feature loaded))
      (setq loaded
            (locate-file (symbol-name feature) load-path '(".el" ".el.gz"))))
    (if (or (string-suffix-p ".el" loaded)
            (string-suffix-p ".el.gz" loaded))
        (or (and (native-comp-available-p)
                 ;; If we built an .eln last time, return it now even though
                 ;; the current Emacs process is still running interpreted .el.
                 ;;
                 ;; NOTE: Thanks to hashing file contents, `comp-lookup-eln'
                 ;; returns nil if the .el has changed on disk, even if the
                 ;; developer did not eval-buffer again.  Then we proceed to
                 ;; build another .eln.  Not sure I'm a fan... but eh, Works
                 ;; For Me, and removes the need to `eval-buffer' constantly.
                 (comp-lookup-eln loaded))
            (let* ((elc (file-name-concat temporary-file-directory
                                          (concat (symbol-name feature)
                                                  ".elc")))
                   (byte-compile-dest-file-function
                    `(lambda (&rest _) ,elc)))
              (when (native-comp-available-p)
                ;; FIXME: Guix overrides `comp-el-to-eln-rel-filename' to
                ;; output filenames with no hash!  So compiling now can result
                ;; in an .eln in ~/.emacs.d/ that will always take precedence
                ;; over the one shipped by Guix.  If we want to cover for that,
                ;; it'd be safer to compile into /tmp with a filename based on
                ;; e.g. `after-init-time'.  Users who install FEATURE purely
                ;; thru Guix are prolly safe.
                ;; https://github.com/meedstrom/org-node/issues/68
                (native-compile-async (list loaded)))
              ;; Native comp may take a while, so build and return .elc this
              ;; time.  We should not pick a preexisting .elc from `load-path'
              ;; if Emacs is running interpreted code now, since that currently
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
      ;; is using an .elc, because we cannot assume the source .el is the
      ;; version that produced that .elc.  It could be in-development, newer
      ;; than the .elc, so our child processes should also use the .elc for
      ;; compatibility right up until the point the developer actually evals
      ;; the .el buffer.
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
take a similar amount of wall-time to work through.

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
            (progn
              (fset 'el-job--split-optimally #'el-job--split-evenly)
              (error "Internal coding mistake, degrading gracefully from now"))
          ;; Spread leftovers equally
          (let ((ctr 0)
                (len (length sublists)))
            (dolist (item (nconc this-sublist untimed items))
              (push item (nth
                          (% (cl-incf ctr) len)
                          sublists)))))
        sublists)))))

(defun el-job--zip-all (lists)
  "Destructively zip all LISTS into one.
See subroutine `el-job-child--zip' for details."
  (let ((merged (pop lists)))
    (while lists
      (setq merged (el-job-child--zip (pop lists) merged)))
    merged))

(defun el-job--windows-cores ()
  "Try to get number of physical cores on Windows system."
  (with-temp-buffer
    (call-process "cmd.exe" nil t nil "/C" "wmic CPU get NumberOfCores /value")
    (goto-char 1)
    (or (and (re-search-forward "NumberOfCores=\\([0-9]+\\)" nil t)
             (number-to-string (match-string 1)))
        1)))


;;; Main logic

(defcustom el-job-max-cores nil
  "A limit on the number of subprocesses for one job.
Windows can get \"error: Could not create child process\" if making too
many processes, so capping it can help."
  :type '(choice integer (const :tag "Auto" nil))
  :group 'processes)

(defvar el-job--all-jobs (make-hash-table :test #'eq)
  "Table of all el-job objects.")

(defmacro el-job--with (job slots &rest body)
  "Make SLOTS expand into object accessors for `el-job' JOB inside BODY.
Cf. `with-slots' in the \"eieio\" library, or `let-alist'.

For clarity inside BODY, each symbol name in SLOTS must be prepended
with one character of your choosing, such as a dot."
  (declare (indent 2) (debug (sexp sexp &rest form)))
  `(cl-symbol-macrolet
       ,(cl-loop
         for slot in slots
         collect `(,slot (,(intern (concat "el-job-"
                                           (substring (symbol-name slot) 1)))
                          ,job)))
     ,@body))

(cl-defstruct (el-job (:constructor el-job--make)
                      (:copier nil))
  id
  callback
  (n-cores-to-use 1)
  (ready nil :documentation "Processes ready for input.")
  (busy nil :documentation "Processes that have not yet returned output.")
  stderr
  ;; Not an interesting timestamp, but `plist-put' needs a non-empty list.
  (timestamps (list :initial-job-creation (current-time)))
  (timer (timer-create))
  finish-times
  spawn-args
  (past-elapsed (make-hash-table :test #'equal))
  queued-inputs
  input-sets
  result-sets)

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

Tip: To let them inherit `load-path' anyway, add it to INJECT-VARS.


INPUTS is a list that will be split by up to the output of
`num-processors', and this determines how many subprocesses will spawn.

INPUTS can also be a function that returns a list.  In this case, the
function is deferred until needed, possibly saving on compute.

If INPUTS is a function and it returns nil, do nothing and return the
symbol `inputs-were-empty'.


The subprocesses have no access to current Emacs state.  The only way
they can affect current state, is if FUNCALL-PER-INPUT returns data,
which is then handled by CALLBACK function in the current Emacs, as
described earlier.

Emacs stays responsive to user input up until all subprocesses finish,
which is when their results are merged and CALLBACK is executed.

CALLBACK receives two arguments: the results as mentioned before, and the
job object.  The latter is mainly useful to check timestamps,
which you can get from this form:

    \(el-job-timestamps JOB)


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
      ;; This one doesn't actually need to be a symbol, but easier to debug
      ;; and user is not surprised since it seems consistent
      (error "Argument CALLBACK must be a symbol with a function definition")))
  (unless (proper-list-p load-features)
    (error "Argument LOAD-FEATURES must be a list"))
  (unless id (error "Argument ID now mandatory"))
  (let ((job (or (gethash id el-job--all-jobs)
                 (puthash id (el-job--make :id id) el-job--all-jobs)))
        (do-respawn nil)
        (do-exec nil))
    (el-job--with job ( .queued-inputs .busy .ready .n-cores-to-use
                        .spawn-args .callback .timestamps )
      (unless (and .busy (eq if-busy 'noop))
        (plist-put .timestamps :launched (current-time))
        ;; REVIEW: Can we somehow defer this to even later?
        ;;         Maybe if-busy=wait could inhibit funcalling it?
        ;;         In fact, if we mandate that `inputs' be a function,
        ;;         might be able to get rid of the concept of queued-inputs.
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
        (if (null .queued-inputs)
            'inputs-were-empty
          (when do-exec
            (setf .callback callback)
            ;; Prevent spawning a dozen processes when we need only one or two
            (let ((machine-cores (max 1 (or el-job-max-cores
                                            (and (eq system-type 'windows-nt)
                                                 (- (el-job--windows-cores) 1))
                                            (- (num-processors) 1)))))
              (setf .n-cores-to-use
                    (if (length< .queued-inputs machine-cores)
                        (length .queued-inputs)
                      machine-cores))
              (when (or (length< .ready .n-cores-to-use)
                        (not (cl-every #'process-live-p .ready)))
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
            (let ((error-maybe
                   (when do-respawn
                     (el-job--disable job)
                     (apply #'el-job--spawn-processes .spawn-args))))
              (unless error-maybe
                (el-job--exec-workload job)))))))))

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
         ;; Ensure the working directory is not remote.
         ;; https://github.com/meedstrom/org-node/issues/46
         (default-directory invocation-directory)
         return-value)
    (el-job--with job (.stderr .id .ready .spawn-args .n-cores-to-use)
      (setf .stderr
            (with-current-buffer
                (get-buffer-create (format " *el-job:%s:err*" .id) t)
              (setq-local el-job-here job)
              (erase-buffer)
              (current-buffer)))
      (with-temp-buffer ;; https://github.com/meedstrom/org-node/issues/98
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
           (el-job--dbg 1 "Terminated job because of: %S" err)
           (setq return-value err))))
      ;; Return non-nil on error, so caller can choose to fail quietly.
      ;; We suppressed the error signal because for some users, it only occurs
      ;; intermittently and does not break things.
      return-value)))

(defun el-job--exec-workload (job)
  "Split the queued inputs in JOB and pass to all children.

This puts them to work.  Each successful child will print output
\(even nil output) to its associated process buffer, whereupon something
should trigger `el-job--handle-output'."
  (el-job--with job
      ( .ready .busy .input-sets .result-sets .queued-inputs .n-cores-to-use
        .past-elapsed .timestamps .finish-times .id .stderr .timer )
    (when .busy
      (error "Unexpected still-busy processes at this time: %S" .busy))
    (cancel-timer .timer)
    (setf .input-sets nil)
    (setf .result-sets nil)
    (setf .finish-times nil)
    (let ((splits (el-job--split-optimally .queued-inputs
                                           .n-cores-to-use
                                           .past-elapsed))
          busy-bufs)
      ;; Sanity check
      (when (length> splits (length .ready))
        (if (or (null .ready) (null splits))
            (error "el-job: Items split in %d lists, but only %d ready processes "
                   (length splits) (length .ready))
          (warn "el-job: Items split in %d lists, but only %d ready processes. %s"
                (length splits) (length .ready)
                "Falling back to use 1 process")
          (mapc #'delete-process (cdr .ready))
          (setf .ready (list (car .ready)))
          (setq splits (list .queued-inputs))))
      (let ((print-length nil)
            (print-level nil)
            (print-circle t)
            (print-symbols-bare t)
            (print-escape-newlines t)
            proc)
        (dolist (items splits)
          (setq proc (pop .ready))
          (push proc .busy)
          (push (process-buffer proc) busy-bufs)
          (setf (alist-get proc .input-sets) items)
          (with-current-buffer (process-buffer proc)
            (erase-buffer)
            (process-send-string proc (prin1-to-string items))
            (process-send-string proc "\n"))))
      (setf .queued-inputs nil)
      (plist-put .timestamps :work-begun (current-time))
      (setf .timer (run-with-timer .02 nil #'el-job--poll 1 busy-bufs)))))

;; Polling: simplistic but reliable.

;; Had the clever idea to add a hook to `after-change-functions' in each
;; process buffer, that checks (eq (char-before) ?\n).  Perf was good on my
;; machine...on Emacs 30, bad on 29.  Plus it just seems the kinda design that
;; invites variance from machine to machine.

;; So, poll.  We do a chain of timers that successively ups the delay.
;; To see what the delays would be, eval:
;; (--map (/ it 32.0) (-iterate '1+ 1 42))

;; And to see the cumulative sums:
;; (-reductions '+ (--map (/ it 32.0) (-iterate '1+ 1 42)))

;; As you can see, we do 7 polls inside the first second,
;; but spread out the last 7 polls between T=20s and T=30s.

(defun el-job--poll (n bufs)
  "Check process buffers BUFS for complete output.
For each where it is complete, handle it.  For the rest, check again
after a short delay.  N is the count of checks done so far."
  (cl-assert (not (null bufs)))
  (if (cl-notany #'buffer-live-p bufs)
      (el-job--dbg 0 "No process buffers alive when `el-job--poll' called")
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
        (when (member (el-job-timer el-job-here) timer-list)
          ;; It does, somehow, happen
          ;; https://github.com/meedstrom/org-node/issues/94
          (el-job--dbg 1 "Timer still active (this is a bug), recovering")
          (cancel-timer (el-job-timer el-job-here)))
        (if busy-bufs
            (if (<= n 42)
                (setf (el-job-timer el-job-here)
                      (run-with-timer
                       (/ n 32.0) nil #'el-job--poll (1+ n) busy-bufs))
              (el-job--disable el-job-here)
              (el-job--dbg 0 "Timed out, was busy for 30+ seconds: %s"
                           (el-job-id el-job-here)))
          (setf (el-job-timer el-job-here)
                (run-with-timer 30 nil #'el-job--reap (current-buffer))))))))

(defun el-job--reap (buf)
  "If BUF is still alive, kill processes in the job associated with it."
  (when (buffer-live-p buf)
    (let ((job (buffer-local-value 'el-job-here buf)))
      (el-job--disable job)
      (el-job--dbg 2 "Reaped idle processes for %s" (el-job-id job)))))

;; REVIEW: Consider setting `inhibit-quit' to nil (called by timer means t now)
(defun el-job--handle-output ()
  "Handle output in current buffer.

If this is the last output for the job, merge all outputs, maybe execute
the callback function, finally maybe run the job again if there is
more input in the queue."
  (let ((proc (get-buffer-process (current-buffer)))
        (job el-job-here)
        finish-time
        durations
        results)
    (el-job--with job
        ( .busy .ready .input-sets .past-elapsed .result-sets .stderr
          .queued-inputs .timestamps .id .finish-times .callback )
      (condition-case _ (let ((output (read (buffer-string))))
                          (setq finish-time (caar output))
                          (setq durations (cdar output))
                          (setq results (cdr output)))
        (( error )
         (dolist (proc (append .busy .ready))
           (delete-process proc))
         (error "In buffer %s: problems reading child output: %s"
                (current-buffer) (buffer-string))))
      (push finish-time .finish-times)
      (setf .busy (delq proc .busy))
      (push proc .ready)
      (when results
        (push results .result-sets)
        ;; Record time spent by FUNCALL-PER-INPUT on each item in INPUTS,
        ;; for a better `el-job--split-optimally' in the future.
        (let ((inputs (alist-get proc .input-sets)))
          (while durations
            (puthash (pop inputs) (pop durations) .past-elapsed))))

      ;; Extra actions when this was the last output
      (when (null .busy)
        (let ((last-done (car (last (sort .finish-times #'time-less-p)))))
          (plist-put .timestamps :work-done last-done))
        (plist-put .timestamps :callback-begun (current-time))
        ;; Finally the purpose of it all.
        ;; Somehow, it took 700 lines of code to get here.
        (when .callback
          ;; Allow quitting out of a hung or slow CALLBACK.
          ;; TODO: Recover well.  How?
          (with-local-quit
            (funcall .callback (el-job--zip-all .result-sets) job)
            t))
        ;; REVIEW: Either
        ;; 1. get rid of this code path (see comments in `el-job-launch' re.
        ;;    queued inputs); or
        ;; 2. consider if we need to check length of .queued-inputs to maybe
        ;;    respawn all processes (see what `el-job-launch' does).
        (when .queued-inputs
          (el-job--exec-workload job))))))

(defun el-job--disable (job)
  "Kill processes in JOB and their process buffers.

This does not deregister the job ID.  That means the next launch with
same ID still has the benchmarks table and possibly queued input."
  (el-job--with job (.id .busy .ready .stderr .timer)
    (cancel-timer .timer)
    (dolist (proc (append .busy .ready))
      (let ((buf (process-buffer proc)))
        ;; Why can BUF be nil?  And why is `kill-buffer' so unsafe?  Can we
        ;; upstream a `kill-buffer-safe' that errors when given nil argument?
        (if (buffer-live-p buf)
            (if (= 0 el-job--debug-level)
                (kill-buffer buf)
              ;; Keep the buffer for inspection
              (delete-process proc))
          (el-job--dbg 1 "Process had no buffer: %s" proc)
          ;; Prolly a no-op
          (delete-process proc))))
    (and (= 0 el-job--debug-level)
         (buffer-live-p .stderr)
         (kill-buffer .stderr))
    (setf .busy nil)
    (setf .ready nil)))


;;; Tools / public API

(defun el-job-cycle-debug-level ()
  "Increment `el-job--debug-level'."
  (interactive)
  (message "Variable `el-job--debug-level' set to %d"
           (setq el-job--debug-level (% (1+ el-job--debug-level) 3))))

(defun el-job-show-info ()
  "Prompt for a job and show its data in a new buffer.

Tip: you can also inspect the contents of the process buffers.
Use \\[el-job-cycle-debug-level] so the debug level is 1+, then look
for buffer names starting with \" *el-job\" - note leading space."
  (interactive)
  (when-let*
      ((id (intern (completing-read "Get info on job: " el-job--all-jobs)))
       (job (gethash id el-job--all-jobs))
       (print-function
        (if (y-or-n-p "Pretty-print with `cl-prin1' (choose no if slow)? ")
            (if (y-or-n-p "Print with `pp' (even prettier)? ")
                'pp
              'cl-prin1)
          'prin1)))
    (set-buffer (get-buffer-create "*el-job debug info*" t))
    (so-long-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (funcall print-function job (current-buffer)))
    (switch-to-buffer (current-buffer))
    (when (eq print-function 'prin1)
      (message "Tip: Type C-h o el-job RET
     to look up which order these fields come in.
     For example, first field is ID, second is CALLBACK etc."))))

(defun el-job-kill-all ()
  "Kill all el-jobs ever registered and forget metadata."
  (interactive)
  (maphash (lambda (id job)
             (el-job--disable job)
             (remhash id el-job--all-jobs))
           el-job--all-jobs))

(defmacro el-job--sit-until-not (test max-secs &optional message)
  "Block until form TEST evaluates to nil, or time MAX-SECS has elapsed.
Either way, return the last TEST result, i.e. non-nil if timed out.

While blocking input, keep MESSAGE visible in the echo area.
MESSAGE can be a string, or a form that evaluates to a string.

Neither TEST nor MESSAGE should be expensive forms, since they are
evaluated rapidly and cannot themselves trigger the time-out.
A typical TEST would check if something in the environment has changed."
  (let ((deadline (gensym "deadline"))
        (last (gensym "last")))
    `(let ((,deadline (time-add (current-time) ,max-secs))
           ,last)
       (catch 'timeout
         (while (setq ,last ,test)
           (unless (time-less-p (current-time) ,deadline)
             (throw 'timeout nil))
           ,(when message `(unless (current-message)
                             (message "%s" ,message)))
           (discard-input)
           (sit-for 0.1)))
       ,last)))

(defun el-job-await (id max-secs &optional message)
  "Block until all processes for job ID finished, then return t.

If the job has still not finished after MAX-SECS seconds, stop
blocking and return nil.

Meanwhile, ensure string MESSAGE is visible in the minibuffer.

If there is no job that matches ID, immediately return t."
  (when (el-job-p id)
    (error "el-job-is-busy: Passed a job object, but expected only a job ID"))
  (let ((job (gethash id el-job--all-jobs)))
    (if job
        (not (el-job--sit-until-not (el-job-busy job) max-secs message))
      t)))

(defun el-job-is-busy (id)
  "Return list of busy processes for job ID, if any.
Safely return nil otherwise, whether or not ID is known."
  (when (el-job-p id)
    (error "el-job-is-busy: Passed a job object, but expected only a job ID"))
  (let ((job (gethash id el-job--all-jobs)))
    (and job (el-job-busy job))))


;;; Obsolete

;; Accelerate the timeframe until we can clean the namespace
(defvar el-jobs :obsolete)
(let ((complainer
       (lambda (_) (error "Some renames in el-job 2.3.0, update your code"))))
  (fset 'el-job:id             complainer)
  (fset 'el-job:callback       complainer)
  (fset 'el-job:n-cores-to-use complainer)
  (fset 'el-job:ready          complainer)
  (fset 'el-job:busy           complainer)
  (fset 'el-job:stderr         complainer)
  (fset 'el-job:timestamps     complainer)
  (fset 'el-job:poll-timer     complainer)
  (fset 'el-job:finish-times   complainer)
  (fset 'el-job:spawn-args     complainer)
  (fset 'el-job:past-elapsed   complainer)
  (fset 'el-job:queued-inputs  complainer)
  (fset 'el-job:input-sets     complainer)
  (fset 'el-job:result-sets    complainer))

(provide 'el-job)

;;; el-job.el ends here

;; Local Variables:
;; checkdoc-spellcheck-documentation-flag: nil
;; checkdoc-verb-check-experimental-flag: nil
;; emacs-lisp-docstring-fill-column: 72
;; End:

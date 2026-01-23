;;; el-job-test.el --- Test suite -*- lexical-binding: t; -*-

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

;;; Code:

(require 'ert)
(require 'map)
(require 'subr-x)
(require 'cl-lib)
(require 'el-job)

;; NOTE: This does not work in test suite, so test this manually!
(defun test-it-preserves-order-of-elements ()
  (let ((nonsense '("foo" "bar" "baz"
                    "qux" "quux" "quuux" "quuuux"
                    "bazola" "ztesch"
                    "foo" "bar" "thud" "grunt"
                    "foo" "bar" "bletch"
                    "foo" "bar" "fum"
                    "fred" "jim" "sheila" "barney"
                    "flarp"
                    "xyzzy"
                    "fnord"
                    "zxc" "spqr" "wombat"
                    "shme"
                    "foo" "bar" "baz" "bongo"
                    "spam" "eggs"
                    "snork"
                    "foo" "bar" "zot"
                    "blarg" "wibble"
                    "toto" "titi" "tata" "tutu"
                    "pippo" "pluto" "paperino"
                    "aap" "noot" "mies"
                    "oogle" "foogle" "boogle"
                    "zork" "gork" "bork")))
    (equal (mapcar #'upcase nonsense)
           (el-job-parallel-mapcar #'upcase nonsense))))

(cl-defun test-split-optimally (items n &optional (table (make-hash-table :test 'equal)))
  (let ((sublists (el-job-ng--split-optimally items n table)))
    (should (equal items (apply 'append sublists)))
    (should-not (> (length sublists) n))
    (should-not (memq nil sublists))
    sublists))

(defun test-split-evenly (items n &optional _)
  (let ((sublists (el-job-ng--split-evenly items n)))
    (should (equal items (apply 'append sublists)))
    (should-not (> (length sublists) n))
    (should-not (memq nil sublists))
    sublists))

(ert-deftest splitting ()
  (should-error (test-split-evenly '(1 2 3) 0))
  (should-error (test-split-optimally '(1 2 3) 0))
  (test-split-optimally nil 5)

  (test-split-evenly '(1 2 3 4 5 6) 4) ;; Even, lesser even
  (test-split-evenly '(1 2 3 4 5 6) 5) ;; Even, lesser odd
  (test-split-evenly '(1 2 3 4 5 6) 6) ;; Even, equal
  (test-split-evenly '(1 2 3 4 5 6) 7) ;; Even, greater odd
  (test-split-evenly '(1 2 3 4 5 6) 8) ;; Even, greater even
  (test-split-evenly '(1 2 3 4 5) 3) ;; Odd, lesser odd
  (test-split-evenly '(1 2 3 4 5) 4) ;; Odd, lesser even
  (test-split-evenly '(1 2 3 4 5) 5) ;; Odd, equal
  (test-split-evenly '(1 2 3 4 5) 6) ;; Odd, greater even
  (test-split-evenly '(1 2 3 4 5) 7) ;; Odd, greater odd

  (let* ((alist ;; Simulated benchmarks
          (cl-loop
           for i below 50
           collect (cons (format "filename-%d.org" i)
                         (seconds-to-time
                          ;; (/ (random 1000) 100.0)
                          (- (expt 1.2 (/ 10.0 (1+ (random 100)))) 1)
                          ;; (/ 4.0 (1+ (random 10)))
                          ))))
         (items (map-keys alist))
         (big-table (map-into (take 100 alist) '(hash-table :test equal)))
         (tiny-table (map-into (take 10 alist) '(hash-table :test equal)))
         (empty-table (make-hash-table :test #'equal)))
    ;; (cl-loop for sublist in (test-split-optimally items 15 big-table)
    ;;          collect (cons (gethash (car sublist) big-table) sublist))
    (should (>= 15 (length (test-split-optimally items 15 big-table))))
    (should (>= 15 (length (test-split-optimally items 15 tiny-table))))
    (should (= 5 (length (test-split-optimally (take 5 items) 15 big-table))))
    (should (= 5 (length (test-split-optimally (take 5 items) 15 tiny-table))))
    (should (= 5 (length (test-split-optimally (take 5 items) 15 empty-table)))))

  ;; (let ((table (map-into (list "a" (seconds-to-time 0.00003214)
  ;;                              "b" (seconds-to-time 2.002)
  ;;                              "c" (seconds-to-time 0.5)
  ;;                              "d" (seconds-to-time 0.5)
  ;;                              "e" (seconds-to-time 0.5)
  ;;                              "f" (seconds-to-time 0.5)
  ;;                              "g" (seconds-to-time 0.5)
  ;;                              "h" (seconds-to-time 0.05))
  ;;                        '(hash-table :test equal)))))

  )

(ert-deftest el-job-old--split-optimally ()
  ;; All benchmarks at zero
  (let* ((plist (cl-loop for i below 50
                         nconc (list (format "file-%d.org" i)
                                     (time-convert 0 t))))
         (items (map-keys plist))
         (big-table (map-into (take 100 plist) '(hash-table :test equal)))
         (tiny-table (map-into (take 10 plist) '(hash-table :test equal)))
         (empty-table (make-hash-table :test #'equal)))
    (should (>= 15 (length (el-job-old--split-optimally items 15 big-table))))
    (should (>= 15 (length (el-job-old--split-optimally items 15 tiny-table))))
    (should (= 15 (length (el-job-old--split-optimally items 15 empty-table))))
    (should (= 5 (length (el-job-old--split-optimally (take 5 items) 15 big-table))))
    (should (= 5 (length (el-job-old--split-optimally (take 5 items) 15 tiny-table))))
    (should (= 5 (length (el-job-old--split-optimally (take 5 items) 15 empty-table)))))

  ;; All benchmarks at random nonzero duration (up to 2.0s)
  (let* ((plist (cl-loop for i below 50
                         nconc (list (format "file-%d.org" i)
                                     (time-convert (/ 2.0 (1+ (random 100))) t))))
         (items (map-keys plist))
         (big-table (map-into (take 100 plist) '(hash-table :test equal)))
         (tiny-table (map-into (take 10 plist) '(hash-table :test equal)))
         (empty-table (make-hash-table :test #'equal)))
    (should (>= 15 (length (el-job-old--split-optimally items 15 big-table))))
    (should (>= 15 (length (el-job-old--split-optimally items 15 tiny-table))))
    (should (= 15 (length (el-job-old--split-optimally items 15 empty-table))))
    (should (= 5 (length (el-job-old--split-optimally (take 5 items) 15 big-table))))
    (should (= 5 (length (el-job-old--split-optimally (take 5 items) 15 tiny-table))))
    (should (= 5 (length (el-job-old--split-optimally (take 5 items) 15 empty-table))))))

(ert-deftest el-job-old--ensure-compiled-lib ()
  (when (and (require 'comp nil t)
             (native-comp-available-p)
             (boundp 'comp-async-compilations)
             (hash-table-p comp-async-compilations)
             (fboundp #'comp-lookup-eln))
    (let (skip-test)
      (let ((loaded (el-job-old--locate-lib-in-load-history
                     'el-job-old-child)))
        (when (string-suffix-p ".el" loaded)
          (setq loaded (comp-lookup-eln loaded)))
        (when (and loaded (string-suffix-p ".eln" loaded))
          (condition-case _
              (delete-file loaded)
            (file-error (setq skip-test t)))))
      (unless skip-test
        (load (locate-library "el-job-old-child.el"))
        (should (string-suffix-p ".el" (el-job-old--locate-lib-in-load-history
                                        'el-job-old-child)))
        (should (string-suffix-p ".elc" (el-job-old--ensure-compiled-lib
                                         'el-job-old-child)))
        (when (let ((procs (hash-table-values comp-async-compilations)))
                (not (el-job-old--sit-until-not
                      (cl-some #'process-live-p procs) 10)))
          (should (string-suffix-p ".eln" (el-job-old--ensure-compiled-lib
                                           'el-job-old-child))))))))

;;; el-job-test.el ends here

;;; el-job-test.el --- Test suite -*- lexical-binding: t; -*-

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

;;; Code:

(require 'ert)
(require 'map)
(require 'subr-x)
(require 'cl-lib)
(require 'el-job-old-child)
(require 'el-job-old)

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

;;; el-job-test.el --- Test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Martin Edström

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
(require 'cl-lib)
(require 'el-job)

(ert-deftest el-job--split-optimally ()
  ;; All benchmarks at zero
  (let* ((plist (cl-loop for i below 50
                         nconc (list (format "file-%d.org" i)
                                     (time-convert 0 t))))
         (items (map-keys plist))
         (big-table (map-into (take 100 plist) '(hash-table :test equal)))
         (tiny-table (map-into (take 10 plist) '(hash-table :test equal)))
         (empty-table (make-hash-table :test #'equal)))
    (should (>= 15 (length (el-job--split-optimally items 15 big-table))))
    (should (>= 15 (length (el-job--split-optimally items 15 tiny-table))))
    (should (= 15 (length (el-job--split-optimally items 15 empty-table))))
    (should (= 5 (length (el-job--split-optimally (take 5 items) 15 big-table))))
    (should (= 5 (length (el-job--split-optimally (take 5 items) 15 tiny-table))))
    (should (= 5 (length (el-job--split-optimally (take 5 items) 15 empty-table)))))

  ;; All benchmarks at random nonzero duration (up to 2.0s)
  (let* ((plist (cl-loop for i below 50
                         nconc (list (format "file-%d.org" i)
                                     (time-convert (/ 2.0 (1+ (random 100))) t))))
         (items (map-keys plist))
         (big-table (map-into (take 100 plist) '(hash-table :test equal)))
         (tiny-table (map-into (take 10 plist) '(hash-table :test equal)))
         (empty-table (make-hash-table :test #'equal)))
    (should (>= 15 (length (el-job--split-optimally items 15 big-table))))
    (should (>= 15 (length (el-job--split-optimally items 15 tiny-table))))
    (should (= 15 (length (el-job--split-optimally items 15 empty-table))))
    (should (= 5 (length (el-job--split-optimally (take 5 items) 15 big-table))))
    (should (= 5 (length (el-job--split-optimally (take 5 items) 15 tiny-table))))
    (should (= 5 (length (el-job--split-optimally (take 5 items) 15 empty-table))))))

;;; el-job-test.el ends here

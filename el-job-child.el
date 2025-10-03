;;; el-job-child.el --- Worker code for children  -*- lexical-binding: t; -*-

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

;; The part of the codebase that child processes will need, and no more.

;;; Code:

(require 'el-job-old-child)

(defalias 'el-job-child--zip  #'el-job-old-child--zip)
(defalias 'el-job-child--work #'el-job-old-child--work)

(provide 'el-job-child)

;;; el-job-child.el ends here

;; Local Variables:
;; checkdoc-spellcheck-documentation-flag: nil
;; checkdoc-verb-check-experimental-flag: nil
;; emacs-lisp-docstring-fill-column: 72
;; End:

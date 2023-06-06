;;; ob-gore.el --- Org Babel for Go using Gore REPL -*- lexical-binding: t -*-

;; Author: Andrea
;; Maintainer: Andrea
;; Version: 0.0.0
;; Package-Requires: ((s "1.11.0") (gorepl-mode "1.0.0"))
;; Homepage: homepage
;; Keywords: tools, org-babel



;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Org-Babel support for evaluating go code.
;;
;; This needs `gorepl-mode' to run commands in `gore', the Go REPL.
;;
;; An alternative to this is ob-go (https://github.com/pope/ob-go): it
;; differs on not needing a main function and letting gore take care of
;; those details.

;;; Code:

(require 'org)
(require 'ob)
(require 'ob-eval)
(require 'ob-ref)
(require 'gorepl-mode)


;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("gore" . "go"))
(add-to-list 'org-src-lang-modes '("gore" . go))

(defvar org-babel-default-header-args:gore '())

(defun ob-gore-gorepl-get-comint-output ()
  "Get last output for `gorepl-mode' in comint."
  (let ((last-command (substring-no-properties gorepl-last-command)))
    (with-current-buffer gorepl-buffer
      (save-excursion
        (goto-char (point-max))
        (search-backward last-command)
        (forward-line)
        (--> (buffer-substring-no-properties (point) (point-max))
             (s-split "gore>" it t)
             (--remove (s-starts-with-p "gore version" it) it)
             (s-join "" it))))))

(defun ob-gore-gorepl-retrieve-output-from-process ()
  ""
  (accept-process-output (get-buffer-process gorepl-buffer) 5)
  (s-trim (ob-gore-gorepl-get-comint-output)))

(defun ob-gore-gorepl-eval-sync (orig-fun &rest args)
  "Make `gorepl-eval' return the output and interpret import statements as :import, which gore then imports."
  (let ((comint-move-point-for-output t)
        (args (--> (setq gorepl-last-command (car args))
                   s-lines
                   (--map
                    (if (s-starts-with-p "import" (s-trim it))
                        (s-replace "import" ":import" it)
                      it)
                    it)
                   (s-join "\n" it)
                   list)))
    (apply orig-fun args)
    (accept-process-output (get-buffer-process gorepl-buffer) 5)
    (let ((output (ob-gore-gorepl-retrieve-output-from-process)))
      (if (equal output "")
          ;; retrying because gore was probably loading
          (ob-gore-gorepl-retrieve-output-from-process)
        output))))

(advice-add 'gorepl-eval :around #'ob-gore-gorepl-eval-sync)

(defun org-babel-execute:gore (body params)
  "Run Gore repl on BODY. Ignore PARAMS for now."
  (message "executing Go source code block")
  (let* ((tmp-src-file (org-babel-temp-file "go-src-" ".go"))
         (processed-params (org-babel-process-params params))
         (flags (cdr (assoc :flags processed-params)))
         (args (cdr (assoc :args processed-params)))
         ;; expand the body with `org-babel-expand-body:go'
         (coding-system-for-read 'utf-8) ;; use utf-8 with subprocesses
         (coding-system-for-write 'utf-8))
    (let
        ((results
          (gorepl-eval body)))
      (if results
          (org-babel-reassemble-table
           (if (or (member "table" (cdr (assoc :result-params processed-params)))
                   (member "vector" (cdr (assoc :result-params processed-params))))
               (let ((tmp-file (org-babel-temp-file "go-")))
                 (with-temp-file tmp-file (insert (org-babel-trim results)))
                 (org-babel-import-elisp-from-file tmp-file))
             (org-babel-read (org-babel-trim results) t))
           (org-babel-pick-name
            (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
           (org-babel-pick-name
            (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))
        ))))

(provide 'ob-gore)

;;; ob-gore.el ends here

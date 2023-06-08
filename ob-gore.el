;;; ob-gore.el --- Org Babel for Go using Gore REPL -*- lexical-binding: t -*-

;; Author: Andrea
;; Maintainer: Andrea
;; Version: 0.1.0
;; Package-Requires: ((s "1.11.0") (gorepl-mode "1.0.0") (dash "2.19.1"))
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
(require 'dash)


(add-to-list 'org-babel-tangle-lang-exts '("gore" . "go"))
(add-to-list 'org-src-lang-modes '("gore" . go))

(defvar org-babel-default-header-args:gore '())

(defun ob-gore-gorepl-get-comint-output ()
  "Get last output for `gorepl-mode' in comint."
  (with-demoted-errors
      (let ((last-command (substring-no-properties gorepl-last-command)))
        (with-current-buffer gorepl-buffer
          (save-excursion
            (goto-char (point-max))
            (search-backward last-command)
            (forward-line)
            (--> (buffer-substring-no-properties (point) (point-max))
                 (s-split "gore>" it t)
                 (--remove (s-starts-with-p "gore version" it) it)
                 (s-join "" it)))))))

(defun ob-gore-gorepl-retrieve-output-from-process ()
  ""
  (while (accept-process-output (get-buffer-process gorepl-buffer) 0 5 t))
  (--> (ob-gore-gorepl-get-comint-output)
       s-trim
       (s-replace-all (--map (cons it "") (s-lines gorepl-last-command)) it))
  )

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
    (accept-process-output (get-buffer-process gorepl-buffer) 1)
    (let ((output (ob-gore-gorepl-retrieve-output-from-process)))
      (if (equal output "")
          ;; retrying because gore was probably loading
          (ob-gore-gorepl-retrieve-output-from-process)
        output))))

(advice-add 'gorepl-eval :around #'ob-gore-gorepl-eval-sync)

(defvar ob-gore-gorepl-dont-show nil
  "Don't show gore comint buffer on sending of command.")

(defun gorepl--run-gore (args)
  "Run an inferior instance of `gore' inside Emacs."
  (let* ((buffer (comint-check-proc gorepl-buffer-name))
         (buf (get-buffer-create (or buffer gorepl-buffer))))
    ;; pop to the "*GO REPL Buffer*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (unless ob-gore-gorepl-dont-show
      (display-buffer
       (if (or buffer (not (derived-mode-p 'gorepl-mode))
               (comint-check-proc (current-buffer)))
           buf
         (current-buffer))))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer gorepl-buffer-name buffer
             gorepl-command nil args)
      (gorepl-mode))))

(defun org-babel-execute:gore (body params)
  "Run Gore repl on BODY. Ignore PARAMS for now."
  (message "executing Go source code block")
  (let* ((processed-params (org-babel-process-params params))
         (coding-system-for-read 'utf-8) ;; use utf-8 with subprocesses
         (coding-system-for-write 'utf-8)
         (ob-gore-gorepl-dont-show t)
         )
    (--> body
         s-lines
         (--keep
          (let ((r (gorepl-eval it)))
            (and (not
                  (or (s-equals-p "....." r)
                      (s-blank-p r)))
                 (format ": %s" r)))
          it)
         (s-join "\n" it))))
(provide 'ob-gore)

;;; ob-gore.el ends here

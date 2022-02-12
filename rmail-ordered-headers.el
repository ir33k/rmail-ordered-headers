;;; rmail-ordered-headers.el --- Print Rmail headers in order -*- lexical-binding: t -*-

;; Version: 1.0
;; Keywords: rmail, mail
;; Author: irek <mail@gumen.pl>
;; URL: https://github.com/ir33k/rmail-ordered-headers

;; This file is part of rmail-ordered-headers.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;;; Problem that this code tries to fix
;;
;; The problem is that Rmail message buffer displays each message
;; headers in different order.  It's different because message can
;; arrive to mbox with headers in any order and Rmail only filters out
;; headers that we want to ignore with `rmail-ignored-headers' or
;; prints those selected with `rmail-displayed-headers'.
;;
;; In my opinion this makes headers harder to read especially when you
;; are going through messages by spamming n or p key.  There is no
;; build in way of controlling headers order.  Topic is discussed
;; further in "Fixed order of headers in Rmail messages" thread of
;; help-gnu-emacs@gnu.org mailing list.  You can read archive here:
;; 
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2022-02/msg00253.html

;;;; What this code does
;;
;; Code introduces `rmail-ordered-headers' variable.  It's a list of
;; header field names that will be printed in Rmail message buffer.
;; Given order of headers in list is preserved.  Variable comes with
;; default value so feature should work right away after code is
;; evaluated.
;;
;; Rmail prints headers with `rmail-copy-headers' function.  To make
;; `rmail-ordered-headers' variable work as described, advice function
;; `rmail-ordered-headers--advice' is added around original Rmail
;; `rmail-copy-headers' function.
;;
;; Feature can be disabled by setting `rmail-ordered-headers' to nil.
;; With that `rmail-ignored-headers' and `rmail-displayed-headers'
;; will work again because added advice preserves original function.
;;
;; To disable it completely remove advice with:
;;
;; (advice-remove 'rmail-copy-headers 'rmail-ordered-headers--advice)

;;;; Notes
;; 
;; - Thanks to Eli, Tassilo and Jean for help.
;; - Tested on Emacs 25.2.2 and 29.0.50.

;;; Code:

(defvar rmail-ordered-headers
  '("Date" "From" "To" "Reply-To" "Cc" "Bcc" "Thread-Topic" "Subject")
  "List of Header field names that Rmail should display in given order.
Headers with empty values are omitted.  Note that controlling
headers using this variable is least performent comparing to
`rmail-displayed-headers' and `rmail-ignored-headers' which are
ignored when this var is not nil.")

(defun rmail-ordered-headers--advice (oldfun beg _end &optional ignored-headers)
  "Advice around `rmail-copy-headers' that handles `rmail-ordered-headers'.
OLDFUN is called if `rmail-ordered-headers' value is nil.  For
documentation of BEG _END and IGNORED-HEADERS please read
`rmail-copy-headers' doc."
  (if (or (null rmail-ordered-headers)
          ignored-headers
          (eq rmail-header-style 'full))
      ;; Call original `rmail-copy-headers'.
      (funcall oldfun beg _end ignored-headers)
    (let ((re-search-next-header
           (apply-partially 're-search-forward "\n[^ \t]" nil t)))
      ;; Implementation based on `rmail-copy-headers'.
      (with-current-buffer rmail-buffer
        (when (search-forward "\n\n" nil t)
          (forward-char -1)
          (save-restriction
            (narrow-to-region beg (point))
            (goto-char (point-min))
            (unless (funcall re-search-next-header)
              (rmail-error-bad-format))
            (forward-char -1)
            ;; Handle `rmail-ordered-headers' logic.
            (seq-each
             (lambda (header)
               (when (re-search-forward (format "^%s:" header) nil t)
                 (append-to-buffer rmail-view-buffer
                                   (match-beginning 0)
                                   (if (funcall re-search-next-header)
                                       (1- (match-end 0))
                                     (point-max))))
               (goto-char (point-min)))
             rmail-ordered-headers)))))))

(advice-add 'rmail-copy-headers :around 'rmail-ordered-headers--advice)

;;; rmail-ordered-headers.el ends here

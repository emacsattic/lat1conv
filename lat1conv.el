;;; lat1conv.el --- convert latin-1 chars to 7bit ascii equivalents

;; Copyright (C) 1998 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions
;; Status: Works in Emacs/XEmacs 19 or later
;; Created: 1998-03-03

;; $Id: lat1conv.el,v 1.4 2006/11/01 01:31:22 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 51 Franklin Street, Fifth Floor; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Perform conversion of some Latin-1 (same as iso8859-1) to equivalent
;; 7-bit ascii sequences when possible.  I wrote this for converting mail
;; and news article reply buffers so they would not contain 8bit characters
;; needlessly.  Otherwise, the buffer has to be converted to
;; quoted-printable before sending, and that makes it harder to read for
;; the recipient if they do not have a MIME-aware reader.
;;
;; These functions should not be used carelessly.  Sometimes it is not
;; appropriate to convert the original text of someone's message, and
;; sometimes there is nothing to gain since other 8-bit characters may be
;; present anyway.  One possible suggestion is to do something like:
;;
;;    (add-hook 'mail-citation-hook
;;              #'(lambda (&optional beg end)
;;                  (lat1conv-to-ascii-query beg end t)
;;                  (and (lat1conv-other-8bit-in-region-p beg end)
;;                       (message "warning: 8-bit chars in included text."))))

;; Updates of this program may be available via the URL
;; http://www.splode.com/~friedman/software/emacs-lisp/

;;; Code:

(defvar lat1conv-table
  '(
    ;; These aren't really latin-1 characters, but PC editors seem to use
    ;; them all the time.
    (?\221 . "`")
    (?\222 . "'")
    (?\223 . "``")
    (?\224 . "''")

    (?\240 . " ")
    (?\246 . "|")
    (?\250 . "\"")
    (?\251 . " (C) ")
    (?\253 . "<<")
    (?\254 . "~")
    (?\255 . "-")
    (?\256 . " (R) ")
    (?\261 . " +/- ")
    (?\262 . "^2")
    (?\263 . "^3")
    (?\264 . "'")
    (?\267 . "*")
    (?\270 . ",")
    (?\271 . "^1")
    (?\273 . ">>")
    (?\274 . " 1/4 ")
    (?\275 . " 1/2 ")
    (?\276 . " 3/4 ")
    (?\306 . "AE")
    (?\327 . "x")
    (?\346 . "ae")
    (?\367 . "/"))
  "Mapping between Latin-1 8-bit characters and 7-bit ascii equivalents.
Each member of the list is a cell containing a character and a string which
that character maps to.")

;; These aren't latin-1 characters at all, but if naked ctrl chars appear
;; in a text buffer it may be desirable to convert them.
(defvar lat1conv-table-ctl-chars
  '((?\C-@  . "^@")
    (?\C-a  . "^A")
    (?\C-b  . "^B")
    (?\C-c  . "^C")
    (?\C-d  . "^D")
    (?\C-e  . "^E")
    (?\C-f  . "^F")
    (?\C-g  . "^G")
    (?\C-h  . "^H")
    (?\C-k  . "^K")
    (?\C-l  . "^L")
    (?\C-m  . "^M")
    (?\C-n  . "^N")
    (?\C-o  . "^O")
    (?\C-p  . "^P")
    (?\C-q  . "^Q")
    (?\C-r  . "^R")
    (?\C-s  . "^S")
    (?\C-t  . "^T")
    (?\C-u  . "^U")
    (?\C-v  . "^V")
    (?\C-w  . "^W")
    (?\C-x  . "^X")
    (?\C-y  . "^Y")
    (?\C-z  . "^Z")
    (?\C-\[ . "^[")
    (?\C-\\ . "^\\")
    (?\C-\] . "^]")
    (?\C-^  . "^^")
    (?\C-_  . "^_")))

;;;###autoload
(defun lat1conv-to-ascii (&optional beg end totalp)
  "In region, map 8-bit characters to 7-bit sequences when possible.
When called interactively with a prefix argument, do no conversion at all
if any unhandled 8-bit characters would still remain in the region.

When called as a function, the args BEG and END delimit the region on which
to act.  If args are nil, use the region delimited by point and mark.
Optional arg TOTALP corresponds to the interactive prefix arg: non-nil
means do no conversion at all if other, unhandled 8-bit characters would
still remain in the region.

Return `t' if any modifications were made to the region, nil otherwise."
  (interactive "r\nP")
  (or beg (setq beg (region-beginning)))
  (or end (setq end (region-end)))
  (cond
   ((and totalp
         (lat1conv-other-8bit-in-region-p beg end))
    nil)
   (t
    (setq end (lat1conv-make-marker end nil t))
    (let ((re (lat1conv-regexp))
          (c nil)
          (changep nil))
      (save-excursion
        (goto-char beg)
        (save-match-data
          (while (re-search-forward re end t)
            (setq c (char-after (match-beginning 0)))
            (replace-match (lat1conv-7bit c))
            (setq changep t))))
      changep))))

;;;###autoload
(defun lat1conv-to-ascii-query (&optional beg end totalp)
  "In region, query mapping 8-bit characters to 7-bit sequences when possible.
When called interactively with a prefix argument, do no conversion at all
if any unhandled 8-bit characters would still remain in the region.

When called as a function, the args BEG and END delimit the region on which
to act.  If args are nil, use the region delimited by point and mark.
Optional arg TOTALP corresponds to the interactive prefix arg: non-nil
means do no conversion at all if other, unhandled 8-bit characters would
still remain in the region.

Return `t' if any modifications were made to the region, nil otherwise."
  (interactive "r\nP")
  (or beg (setq beg (region-beginning)))
  (or end (setq end (region-end)))
  (cond
   ((and totalp
         (lat1conv-other-8bit-in-region-p beg end))
    (and (interactive-p)
         (message "region contains unhandled 8-bit chars; %s"
                  "no 7-bit conversion"))
    nil)
   (t
    ;; Turn this into a marker so that as characters are replaced with
    ;; strings which may be of greater length, the position of the region
    ;; end moves forward appropriately.
    (setq end (lat1conv-make-marker end nil t))
    (let ((re (lat1conv-regexp))
          (changep nil))
      (save-excursion
        (goto-char beg)
        (save-match-data
          (map-y-or-n-p
           (function (lambda (obj)
                       (format
                        "Replace `%c' (0%o, 0x%s) at point with \"%s\": "
                        obj obj (upcase (format "%x" obj))
                        (lat1conv-7bit obj))))
           (function (lambda (char)
                       (replace-match (lat1conv-7bit char))
                       (setq changep t)))
           (function (lambda ()
                       (and (re-search-forward re end t)
                            (char-after (match-beginning 0)))))
           '("character" "characters" "replace")
           nil t)))
      changep))))

(defun lat1conv-other-8bit-in-region-p (&optional beg end)
  "Return `t' if region contains unhandled 8-bit characters.
Return `t' if region contains any 8-bit characters not handled by
`lat1conv-table'."
  (or beg (setq beg (region-beginning)))
  (or end (setq end (region-end)))
  (save-excursion
    (save-match-data
      (goto-char beg)
      (re-search-forward (lat1conv-regexp-other-8bit) end t))))

(defun lat1conv-regexp (&optional tbl)
  "Return regexp for 8-bit characters handled by lat1conv-table."
  (or tbl (setq tbl lat1conv-table))
  (let* ((str (make-string (length tbl) ?0))
         (idx 0))
    (while tbl
      (aset str idx (car (car tbl)))
      (setq idx (1+ idx))
      (setq tbl (cdr tbl)))
    (format "[%s]" (regexp-quote str))))

;; Warning: this function treats characters as integers, which is not
;; really correct for XEmacs 20 (although it may still work).
;; Look for type conversion functions.
(defun lat1conv-regexp-other-8bit ()
  "Return regexp for 8-bit characters not handled by lat1conv-table."
  (let ((str (make-string (- 128 (length lat1conv-table)) ?0))
        (i 0)
        (c 128))
    (while (< c 256)
      (cond ((lat1conv-7bit c))
            (t
             (aset str i c)
             (setq i (1+ i))))
      (setq c (1+ c)))
    (format "[%s]" (regexp-quote str))))

(defun lat1conv-7bit (char)
  (cdr (assoc char lat1conv-table)))

(defun lat1conv-make-marker (pos &optional buffer insertion-type)
  "Copy existing marker, or make a new one from point.
POS may be a marker, in which case the marker is copied verbatim.
Otherwise, args POS and BUFFER are like those used by `set-marker'.
Arg INSERTION-TYPE is like that used by `set-marker-insertion-type',
which is present in Emacs 19.30 and later."
  (let ((new-marker nil))
    (cond ((markerp pos)
           (setq new-marker (copy-marker pos))
           (and buffer
                (set-marker new-marker (marker-position pos) buffer)))
          (t
           (setq new-marker (make-marker))
           (set-marker new-marker pos buffer)))
    (and (fboundp 'set-marker-insertion-type)
         (set-marker-insertion-type new-marker insertion-type))
    new-marker))

(provide 'lat1conv)

;;; lat1conv.el ends here

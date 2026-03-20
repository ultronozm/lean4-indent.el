;;; lean4-indent-newline-lower-bound.el --- Check C-j lower-bound indentation -*- lexical-binding: t; -*-

;;; Commentary:

;; This file provides a small harness for checking a practical newline property
;; against real Lean files:
;;
;;   after calling `newline-and-indent' at the end of a line, the inserted
;;   line's indentation should be at least the indentation of the line that
;;   actually followed in the file.
;;
;; The intent is to validate that `C-j' picks a "maximally plausible" initial
;; indentation, leaving `TAB' to cycle to shallower alternatives.

;;; Code:

(require 'lean4-syntax)
(require 'lean4-indent)

(defun lean4-indent-newline-lower-bound--line-string ()
  "Return the current line without text properties."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun lean4-indent-newline-lower-bound--eligible-line-p (pos)
  "Return non-nil when line at POS is eligible for the lower-bound check."
  (save-excursion
    (goto-char pos)
    (let ((text (lean4-indent-newline-lower-bound--line-string)))
      (and (not (lean4-indent--line-blank-p text))
           (not (lean4-indent--comment-line-p pos))
           (not (lean4-indent--string-line-p pos))))))

(defun lean4-indent-newline-lower-bound-check-buffer ()
  "Return a list of lower-bound newline failures for the current Lean buffer.

Each element is a plist containing:
- `:line'       the original line number where `newline-and-indent' was tried
- `:got'        indentation of the inserted line
- `:expected'   indentation of the original following line
- `:current'    text of the line where the newline was inserted
- `:next'       text of the original following line"
  (let ((failures nil)
        (line-count (line-number-at-pos (point-max) t)))
    ;; Work bottom-up so inserted lines do not disturb later probes.
    (dotimes (offset (max 0 (1- line-count)))
      (let ((line (- line-count offset 1)))
        (goto-char (point-min))
        (forward-line (1- line))
        (let ((current-pos (point)))
          (when (and (lean4-indent-newline-lower-bound--eligible-line-p current-pos)
                     (save-excursion
                       (forward-line 1)
                       (and (not (eobp))
                            (lean4-indent-newline-lower-bound--eligible-line-p (point)))))
            (let* ((current-text (lean4-indent-newline-lower-bound--line-string))
                   (next-info
                    (save-excursion
                      (forward-line 1)
                      (list :indent (current-indentation)
                            :text (lean4-indent-newline-lower-bound--line-string))))
                   (expected (plist-get next-info :indent))
                   (next-text (plist-get next-info :text)))
              (end-of-line)
              (call-interactively #'newline-and-indent)
              (let ((got (current-indentation)))
                (when (< got expected)
                  (push (list :line line
                              :got got
                              :expected expected
                              :current current-text
                              :next next-text)
                        failures))))))))
    (nreverse failures)))

(defun lean4-indent-newline-lower-bound-check-file (file)
  "Check FILE for lower-bound newline failures and return the failure list."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (set-syntax-table lean4-syntax-table)
    (setq-local indent-tabs-mode nil)
    (setq-local lean4-indent-offset 2)
    (lean4-indent-setup-buffer)
    (lean4-indent-newline-lower-bound-check-buffer)))

(defun lean4-indent-newline-lower-bound-batch-file ()
  "Batch entry point for checking one Lean file.

Usage:
  emacs --batch -L . -L /path/to/lean4-mode \\
    -l test/lean4-indent-newline-lower-bound.el \\
    -f lean4-indent-newline-lower-bound-batch-file FILE"
  (let* ((file (car command-line-args-left))
         (failures (and file
                        (lean4-indent-newline-lower-bound-check-file file))))
    (unless file
      (error "missing FILE argument"))
    (if failures
        (progn
          (princ (format "FAILURES %d %s\n" (length failures) file))
          (dolist (failure failures)
            (princ
             (format "line %d: got %d expected >= %d\n  current: %s\n  next:    %s\n"
                     (plist-get failure :line)
                     (plist-get failure :got)
                     (plist-get failure :expected)
                     (plist-get failure :current)
                     (plist-get failure :next))))
          (kill-emacs 1))
      (princ (format "OK %s\n" file))
      (kill-emacs 0))))

(provide 'lean4-indent-newline-lower-bound)
;;; lean4-indent-newline-lower-bound.el ends here

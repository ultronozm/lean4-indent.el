;;; lean4-indent-newline-upper-bound.el --- Check C-j upper-bound indentation -*- lexical-binding: t; -*-

;;; Commentary:

;; This file provides a conservative harness for checking a practical
;; overindent property against real Lean files:
;;
;;   after calling `newline-and-indent' at the end of a line, the inserted
;;   line's indentation should not exceed the indentation of the original
;;   following line when both lines plainly belong to the same enclosing
;;   top-level item.
;;
;; This is intentionally narrower than the lower-bound harness.  The next real
;; line is only treated as an upper bound when it is a trustworthy sibling or
;; continuation in the same existing context.

;;; Code:

(require 'lean4-syntax)
(require 'lean4-indent)

(defun lean4-indent-newline-upper-bound--line-string ()
  "Return the current line without text properties."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun lean4-indent-newline-upper-bound--eligible-line-p (pos)
  "Return non-nil when line at POS is eligible for the upper-bound check."
  (save-excursion
    (goto-char pos)
    (let ((text (lean4-indent-newline-upper-bound--line-string)))
      (and (not (lean4-indent--line-blank-p text))
           (not (lean4-indent--comment-line-p pos))
           (not (lean4-indent--string-line-p pos))))))

(defun lean4-indent-newline-upper-bound--trust-next-line-p (current-pos next-pos)
  "Return non-nil when NEXT-POS is a trustworthy upper bound for CURRENT-POS."
  (let* ((current-context (lean4-indent--top-level-context current-pos lean4-indent-offset))
         (next-context (lean4-indent--top-level-context next-pos lean4-indent-offset))
         (current-text
          (if (lean4-indent--comment-line-p current-pos)
              ""
            (lean4-indent--line-text-no-comment current-pos)))
         (next-indent (lean4-indent--line-indent next-pos))
         (current-indent (lean4-indent--line-indent current-pos)))
    (and current-context
         next-context
         (= (plist-get current-context :pos)
            (plist-get next-context :pos))
         (>= next-indent 0)
         (>= next-indent (plist-get current-context :body-indent))
         ;; Skip obviously open-ended lines where a deeper blank continuation is
         ;; often legitimate even if the next line in the file later dedents.
         (not (lean4-indent--line-ends-with-op-p current-text))
         (not (lean4-indent--line-ends-with-comma-p current-text))
         (not (and (> current-indent next-indent)
                   (lean4-indent--line-starts-with-paren-p current-text))))))

(defun lean4-indent-newline-upper-bound--collect-candidates (&optional start-line end-line)
  "Collect upper-bound newline candidates in the current buffer.

When START-LINE and/or END-LINE are non-nil, only include original lines in
that inclusive range.

Return a list of plists.  Each plist contains:
- `:line'       original line number
- `:pos'        original line beginning position
- `:expected'   indentation of the original following line
- `:current'    text of the line where `newline-and-indent' will be tried
- `:next'       text of the original following line"
  (save-excursion
    (goto-char (point-min))
    (let ((line 1)
          (candidates nil))
      (while (not (eobp))
        (let ((current-pos (point)))
          (when (and (or (not start-line) (<= start-line line))
                     (or (not end-line) (<= line end-line))
                     (lean4-indent-newline-upper-bound--eligible-line-p current-pos)
                     (save-excursion
                       (forward-line 1)
                       (and (not (eobp))
                            (lean4-indent-newline-upper-bound--eligible-line-p (point))
                            (lean4-indent-newline-upper-bound--trust-next-line-p
                             current-pos (point)))))
            (let* ((current-text (lean4-indent-newline-upper-bound--line-string))
                   (next-info
                    (save-excursion
                      (forward-line 1)
                      (list :indent (current-indentation)
                            :text (lean4-indent-newline-upper-bound--line-string)))))
              (push (list :line line
                          :pos current-pos
                          :expected (plist-get next-info :indent)
                          :current current-text
                          :next (plist-get next-info :text))
                    candidates))))
        (setq line (1+ line))
        (forward-line 1))
      candidates)))

(defun lean4-indent-newline-upper-bound-check-buffer (&optional start-line end-line)
  "Return upper-bound newline failures for the current Lean buffer.

When START-LINE and/or END-LINE are non-nil, only check original lines in that
inclusive range.

Each element is a plist containing:
- `:line'       the original line number where `newline-and-indent' was tried
- `:got'        indentation of the inserted line
- `:expected'   indentation of the original following line
- `:current'    text of the line where the newline was inserted
- `:next'       text of the original following line"
  (let* ((lean4-indent--region-line-contexts
          (lean4-indent--build-region-line-context-cache))
         (lean4-indent--region-top-level-contexts
          (lean4-indent--build-top-level-context-cache lean4-indent-offset))
         (candidates
          (lean4-indent-newline-upper-bound--collect-candidates
           start-line end-line))
         (failures nil))
    ;; Probe bottom-up so the temporary inserted line never disturbs later
    ;; candidate positions from the original buffer.
    (dolist (candidate candidates)
      (let ((line (plist-get candidate :line))
            (pos (plist-get candidate :pos))
            (expected (plist-get candidate :expected))
            (current-text (plist-get candidate :current))
            (next-text (plist-get candidate :next)))
        (goto-char pos)
        (end-of-line)
        (let ((lean4-indent--region-line-contexts nil)
              (lean4-indent--region-top-level-contexts nil))
          (call-interactively #'newline-and-indent))
        (let ((got (current-indentation)))
          (when (> got expected)
            (push (list :line line
                        :got got
                        :expected expected
                        :current current-text
                        :next next-text)
                  failures)))
        (delete-region (line-beginning-position) (line-beginning-position 2))))
    (nreverse failures)))

(defun lean4-indent-newline-upper-bound-check-file (file &optional start-line end-line)
  "Check FILE for upper-bound newline failures and return the failure list.

When START-LINE and/or END-LINE are non-nil, only check original lines in that
inclusive range."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (set-syntax-table lean4-syntax-table)
    (setq-local indent-tabs-mode nil)
    (setq-local lean4-indent-offset 2)
    (lean4-indent-setup-buffer)
    (lean4-indent-newline-upper-bound-check-buffer start-line end-line)))

(defun lean4-indent-newline-upper-bound-batch-file ()
  "Batch entry point for checking one Lean file.

Usage:
  emacs --batch -L . -L /path/to/lean4-mode \\
    -l test/lean4-indent-newline-upper-bound.el \\
    -f lean4-indent-newline-upper-bound-batch-file FILE [START [END]]"
  (let* ((file (car command-line-args-left))
         (start-line (and (cadr command-line-args-left)
                          (string-to-number (cadr command-line-args-left))))
         (end-line (and (caddr command-line-args-left)
                        (string-to-number (caddr command-line-args-left))))
         (failures (and file
                        (lean4-indent-newline-upper-bound-check-file
                         file start-line end-line))))
    (unless file
      (error "missing FILE argument"))
    (if failures
        (progn
          (princ (format "FAILURES %d %s%s\n"
                         (length failures)
                         file
                         (if start-line
                             (format " [%d,%s]" start-line
                                     (if end-line (number-to-string end-line) "end"))
                           "")))
          (dolist (failure failures)
            (princ
             (format "line %d: got %d expected <= %d\n  current: %s\n  next:    %s\n"
                     (plist-get failure :line)
                     (plist-get failure :got)
                     (plist-get failure :expected)
                     (plist-get failure :current)
                     (plist-get failure :next))))
          (kill-emacs 1))
      (princ (format "OK %s%s\n"
                     file
                     (if start-line
                         (format " [%d,%s]" start-line
                                 (if end-line (number-to-string end-line) "end"))
                       "")))
      (kill-emacs 0))))

(provide 'lean4-indent-newline-upper-bound)
;;; lean4-indent-newline-upper-bound.el ends here

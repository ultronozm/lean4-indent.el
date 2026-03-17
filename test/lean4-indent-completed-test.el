;;; lean4-indent-completed-test.el --- Shared completed-code indentation tests -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'lean4-syntax)
(require 'lean4-indent)
(require 'lean4-indent-ts)

(defconst lean4-indent-completed-test--test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing shared indentation test files.")

(defconst lean4-indent-completed-test--excluded-cases
  '(lean4-indent--bare-have-line-indents-continuation
    lean4-indent--bare-suffices-line-indents-continuation)
  "Heuristic final-line fixtures that intentionally exercise incomplete syntax.")

(defun lean4-indent-completed-test--heuristic-file ()
  "Return the heuristic indentation test file path."
  (expand-file-name "lean4-indent-test.el"
                    lean4-indent-completed-test--test-dir))

(defun lean4-indent-completed-test--collect-cases ()
  "Return completed-code final-line cases from the heuristic suite.

Each entry is a plist with keys `:name' and `:contents'."
  (with-temp-buffer
    (insert-file-contents (lean4-indent-completed-test--heuristic-file))
    (goto-char (point-min))
    (let (cases form)
      (condition-case nil
          (while t
            (setq form (read (current-buffer)))
            (when (and (listp form)
                       (eq (car form) 'lean4-define-final-line-indent-test))
              (let ((name (nth 1 form))
                    (contents (nth 2 form)))
                (unless (memq name lean4-indent-completed-test--excluded-cases)
                  (push (list :name name :contents contents) cases)))))
        (end-of-file nil))
      (nreverse cases))))

(defun lean4-indent-completed-test--with-buffer (contents indent-fn use-ts)
  "Create a temporary test buffer with CONTENTS and run INDENT-FN.

When USE-TS is non-nil, create a Lean tree-sitter parser first."
  (with-temp-buffer
    (insert contents)
    (goto-char (point-min))
    (set-syntax-table lean4-syntax-table)
    (setq-local indent-tabs-mode nil)
    (setq-local lean4-indent-offset 2)
    (setq-local indent-line-function indent-fn)
    (when use-ts
      (let ((treesit-extra-load-path (lean4-indent-ts--extra-load-path)))
        (treesit-parser-create 'lean))
      (setq-local lean4-indent-ts--buffer-ready t))
    (goto-char (point-max))
    (when (and (bolp) (not (bobp)))
      (forward-line -1))
    (end-of-line)
    (let ((before (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
      (funcall indent-line-function)
      (should (equal (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))
                     before)))))

(defun lean4-indent-completed-test--run-case (case indent-fn use-ts)
  "Run shared completed-code CASE with INDENT-FN.

USE-TS selects whether to create a tree-sitter parser."
  (ert-info ((symbol-name (plist-get case :name)))
    (lean4-indent-completed-test--with-buffer
     (plist-get case :contents) indent-fn use-ts)))

(defun lean4-indent-completed-test--shared-cases ()
  "Return shared completed-code cases."
  (or (get 'lean4-indent-completed-test--shared-cases 'value)
      (let ((cases (lean4-indent-completed-test--collect-cases)))
        (put 'lean4-indent-completed-test--shared-cases 'value cases)
        cases)))

(ert-deftest lean4-indent-completed--heuristic-suite ()
  (dolist (case (lean4-indent-completed-test--shared-cases))
    (lean4-indent-completed-test--run-case
     case #'lean4-indent-line-function nil)))

(ert-deftest lean4-indent-completed--tree-sitter-suite ()
  (dolist (case (lean4-indent-completed-test--shared-cases))
    (lean4-indent-completed-test--run-case
     case #'lean4-indent-ts-line-function t)))

(ert-deftest lean4-indent-completed--case-count ()
  (should (= (length (lean4-indent-completed-test--shared-cases)) 44)))

(provide 'lean4-indent-completed-test)
;;; lean4-indent-completed-test.el ends here

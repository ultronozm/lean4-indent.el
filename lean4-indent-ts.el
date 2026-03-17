;;; lean4-indent-ts.el --- Experimental tree-sitter indentation for Lean 4 -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Paul D. Nelson

;; Author: Paul D. Nelson <ultrono@gmail.com>
;; Keywords: text

;; This program is free software; you can redistribute it and/or modify
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
;; This file is an experimental tree-sitter-backed alternative to
;; `lean4-indent.el'.  It is intentionally incomplete: when the tree-sitter
;; logic cannot classify the current line confidently, it falls back to
;; `lean4-indent-line-function'.
;;
;; The goal is to replace repeated backward scanning with syntax-driven
;; indentation for tactic-free Lean code.  Current coverage focuses on:
;;
;; - top-level commands and declarations
;; - wrapped declaration bodies
;; - `where' declarations and structure fields
;; - `fun' bodies and multiline applications
;; - `match' alternatives
;;
;; Tactic blocks, comments, blank lines, and incomplete syntax still defer to
;; the existing indenter for now.
;;
;; To try it:
;;
;;   (add-hook 'lean4-mode-hook
;;             (lambda ()
;;               (setq-local indent-line-function
;;                           #'lean4-indent-ts-line-function)))
;;
;; If Emacs does not yet have a Lean tree-sitter grammar installed, this file
;; will automatically fall back to `lean4-indent-line-function'.

;;; Code:

(require 'cl-lib)
(require 'lean4-indent)
(require 'subr-x)
(require 'treesit nil t)

(defgroup lean4-indent-ts nil
  "Experimental tree-sitter indentation for Lean 4."
  :group 'lean4-indent)

(defcustom lean4-indent-ts-fallback-function #'lean4-indent-line-function
  "Fallback function used when the tree-sitter indenter declines to decide."
  :type 'function
  :safe #'functionp)

(defcustom lean4-indent-ts-grammar-dir nil
  "Local checkout of a Lean tree-sitter grammar repository.

When non-nil, `lean4-indent-ts-register-grammar-source' adds it to
`treesit-language-source-alist' for language `lean'."
  :type '(choice (const :tag "Unset" nil)
                 directory))

(defconst lean4-indent-ts--vendored-grammar-dir
  (expand-file-name "vendor/tree-sitter-lean-indent"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Vendored Lean tree-sitter grammar fork used by this repository.")

(defconst lean4-indent-ts--vendored-grammar-build-dir
  (expand-file-name ".build" lean4-indent-ts--vendored-grammar-dir)
  "Repo-local directory for compiled Lean tree-sitter grammar libraries.")

(defconst lean4-indent-ts--top-level-types
  '("declaration" "section" "namespace" "variable" "attribute" "hash_command"
    "abbrev" "def" "example" "instance" "structure" "theorem" "definition")
  "Node types that act like flush-left top-level commands.")

(defconst lean4-indent-ts--decl-types
  '("abbrev" "def" "example" "instance" "structure" "theorem" "definition")
  "Declaration node types that carry a `body' field.")

(defconst lean4-indent-ts--body-intro-types
  '("fun" "if" "if_then_else" "match" "do" "do_if" "do_try")
  "Expression nodes whose body usually indents one step.")

(defconst lean4-indent-ts--apply-types
  '("apply" "application")
  "Node types representing multiline applications.")

(defconst lean4-indent-ts--match-alt-types
  '("match_alt" "match_arm" "do_match_arm")
  "Node types representing a single `match` branch.")

(defconst lean4-indent-ts--top-level-continuation-types
  '("variable" "attribute")
  "Top-level command node types whose wrapped lines indent one step.")

(defconst lean4-indent-ts--tactic-block-types
  '("tactic_focus" "tactic_case")
  "Tactic nodes whose bodies indent one step.")

(defconst lean4-indent-ts--calc-step-types
  '("calc_step")
  "Node types representing a single `calc` step.")

(defconst lean4-indent-ts--tactic-binding-types
  '("tactic_have" "tactic_let")
  "Tactic nodes whose bound value may continue on following lines.")

(defconst lean4-indent-ts--tactic-config-types
  '("tactic_config")
  "Node types representing tactic config lists like `rw [a, b]'.")

(defconst lean4-indent-ts--constructor-types
  '("anonymous_constructor")
  "Node types representing `⟨..., ...⟩` constructor terms.")

(defconst lean4-indent-ts--tactic-body-intro-types
  '("tactic_show")
  "Tactic nodes whose body continues on following lines.")

(defconst lean4-indent-ts--declaration-binding-types
  '("let" "have")
  "Declaration-body bindings whose value may continue on following lines.")

(defconst lean4-indent-ts--field-assignment-types
  '("field_assignment")
  "Node types representing structure field assignments.")

(defun lean4-indent-ts-register-grammar-source ()
  "Register the configured Lean grammar source for tree-sitter installs."
  (interactive)
  (let ((dir (or lean4-indent-ts-grammar-dir
                 (and (file-directory-p lean4-indent-ts--vendored-grammar-dir)
                      lean4-indent-ts--vendored-grammar-dir))))
    (unless dir
      (user-error "No Lean tree-sitter grammar directory is configured"))
    (setf (alist-get 'lean treesit-language-source-alist)
          (list dir))))

(defun lean4-indent-ts--extra-load-path ()
  "Return extra tree-sitter load paths for Lean indentation.

Prefer the repo-local compiled vendored grammar when present."
  (let ((vendored lean4-indent-ts--vendored-grammar-build-dir))
    (append
     (and (file-directory-p vendored) (list vendored))
     treesit-extra-load-path)))

(defun lean4-indent-ts--available-p ()
  "Return non-nil when Lean tree-sitter parsing is available."
  (and (featurep 'treesit)
       (fboundp 'treesit-parser-create)
       (let ((treesit-extra-load-path (lean4-indent-ts--extra-load-path)))
         (treesit-ready-p 'lean t))))

(defun lean4-indent-ts--line-start-pos ()
  "Return the first nonblank position on the current line, or line start."
  (save-excursion
    (back-to-indentation)
    (point)))

(defun lean4-indent-ts--line-text ()
  "Return the current line's text."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun lean4-indent-ts--line-blank-p ()
  "Return non-nil when the current line is blank."
  (string-match-p "\\`[ \t]*\\'" (lean4-indent-ts--line-text)))

(defun lean4-indent-ts--line-comment-p ()
  "Return non-nil when the current line is a comment line."
  (save-excursion
    (back-to-indentation)
    (or (looking-at-p "--")
        (looking-at-p "/-"))))

(defun lean4-indent-ts--line-number (pos)
  "Return line number at POS."
  (line-number-at-pos pos t))

(defun lean4-indent-ts--node-start-line (node)
  "Return the line number where NODE starts."
  (lean4-indent-ts--line-number (treesit-node-start node)))

(defun lean4-indent-ts--node-indent (node)
  "Return indentation column of NODE's starting line."
  (save-excursion
    (goto-char (treesit-node-start node))
    (current-indentation)))

(defun lean4-indent-ts--current-node ()
  "Return the smallest useful node at the current line."
  (let ((node (or (treesit-node-at (lean4-indent-ts--line-start-pos) 'named)
                  (treesit-node-at (line-beginning-position) 'named))))
    (while (and node (not (treesit-node-check node 'named)))
      (setq node (treesit-node-parent node)))
    node))

(defun lean4-indent-ts--ancestor-where (node pred)
  "Return the nearest ancestor of NODE satisfying PRED."
  (let ((cur node)
        found)
    (while (and cur (not found))
      (when (funcall pred cur)
        (setq found cur))
      (setq cur (treesit-node-parent cur)))
    found))

(defun lean4-indent-ts--ancestor-type (node types)
  "Return the nearest ancestor of NODE whose type is in TYPES."
  (lean4-indent-ts--ancestor-where
   node
   (lambda (n) (member (treesit-node-type n) types))))

(defun lean4-indent-ts--ancestor-type-starting-before-line (node types)
  "Return nearest ancestor of NODE in TYPES that starts before current line."
  (let ((current-line (line-number-at-pos (line-beginning-position) t)))
    (lean4-indent-ts--ancestor-where
     node
     (lambda (n)
       (and (member (treesit-node-type n) types)
            (< (lean4-indent-ts--node-start-line n) current-line))))))

(defun lean4-indent-ts--unwrap-declaration (node)
  "Return the inner declaration node for NODE."
  (if (equal (treesit-node-type node) "declaration")
      (or (cl-loop for i from 0 below (treesit-node-child-count node t)
                   for child = (treesit-node-child node i t)
                   when (member (treesit-node-type child) lean4-indent-ts--decl-types)
                   return child)
          node)
    node))

(defun lean4-indent-ts--top-level-command (node)
  "Return the nearest enclosing top-level command node for NODE."
  (lean4-indent-ts--ancestor-type node lean4-indent-ts--top-level-types))

(defun lean4-indent-ts--inside-tactics-p (node)
  "Return non-nil when NODE is inside a `tactics' block."
  (and node
       (or (lean4-indent-ts--ancestor-type node '("by"))
           (lean4-indent-ts--ancestor-type node '("tactics")))))

(defun lean4-indent-ts--top-level-line-p (node)
  "Return non-nil when the current line starts a top-level command NODE."
  (and node
       (= (lean4-indent-ts--node-start-line node)
          (line-number-at-pos (line-beginning-position) t))))

(defun lean4-indent-ts--top-level-continuation-indent (node)
  "Return indentation for a wrapped top-level command line, or nil."
  (let ((top (lean4-indent-ts--top-level-command node)))
    (when (and top
               (member (treesit-node-type top)
                       lean4-indent-ts--top-level-continuation-types)
               (> (line-number-at-pos (line-beginning-position) t)
                  (lean4-indent-ts--node-start-line top)))
      (+ (lean4-indent-ts--node-indent top) lean4-indent-offset))))

(defun lean4-indent-ts--declaration-body-indent (node)
  "Return declaration-body indentation for NODE, or nil."
  (let* ((decl0 (lean4-indent-ts--ancestor-type node (append '("declaration") lean4-indent-ts--decl-types)))
         (decl (and decl0 (lean4-indent-ts--unwrap-declaration decl0)))
         (body (and decl (treesit-node-child-by-field-name decl "body"))))
    (when (and decl body
               (> (line-number-at-pos (line-beginning-position) t)
                  (lean4-indent-ts--node-start-line decl)))
      (+ (lean4-indent-ts--node-indent decl) lean4-indent-offset))))

(defun lean4-indent-ts--where-decl-indent (node)
  "Return indentation for a `where_decl' line or body, or nil."
  (let ((where-decl (lean4-indent-ts--ancestor-type node '("where_decl"))))
    (when where-decl
      (let ((current-line (line-number-at-pos (line-beginning-position) t))
            (where-line (lean4-indent-ts--node-start-line where-decl)))
        (if (= current-line where-line)
            (let ((owner (or (lean4-indent-ts--ancestor-type (treesit-node-parent where-decl)
                                                             (append '("declaration")
                                                                     lean4-indent-ts--decl-types))
                             (treesit-node-parent where-decl))))
              (+ (lean4-indent-ts--node-indent owner) lean4-indent-offset))
          (+ (lean4-indent-ts--node-indent where-decl) lean4-indent-offset))))))

(defun lean4-indent-ts--fun-body-indent (node)
  "Return indentation for a `fun' body line, or nil."
  (let* ((fun (lean4-indent-ts--ancestor-type node '("fun")))
         (body (and fun (treesit-node-child-by-field-name fun "body"))))
    (when (and fun body
               (> (line-number-at-pos (line-beginning-position) t)
                  (lean4-indent-ts--node-start-line fun))
               (<= (treesit-node-start body) (line-beginning-position))
               (< (line-beginning-position) (treesit-node-end body)))
      (+ (lean4-indent-ts--node-indent fun) lean4-indent-offset))))

(defun lean4-indent-ts--apply-argument-indent (node)
  "Return indentation for a multiline application argument, or nil."
  (let ((apply (lean4-indent-ts--ancestor-type-starting-before-line
                node lean4-indent-ts--apply-types)))
    (when apply
      (+ (lean4-indent-ts--node-indent apply) lean4-indent-offset))))

(defun lean4-indent-ts--match-alt-indent (node)
  "Return indentation for a `match_alt' line, or nil."
  (let ((alt (lean4-indent-ts--ancestor-type node lean4-indent-ts--match-alt-types)))
    (when alt
      (let ((match (or (lean4-indent-ts--ancestor-type alt '("match"))
                       alt)))
        (if (= (line-number-at-pos (line-beginning-position) t)
               (lean4-indent-ts--node-start-line alt))
            (lean4-indent-ts--node-indent match)
          (+ (lean4-indent-ts--node-indent match) lean4-indent-offset))))))

(defun lean4-indent-ts--tactic-block-indent (node)
  "Return indentation for a tactic focus/case body line, or nil."
  (let ((block (lean4-indent-ts--ancestor-type node
                                               lean4-indent-ts--tactic-block-types)))
    (when (and block
               (> (line-number-at-pos (line-beginning-position) t)
                  (lean4-indent-ts--node-start-line block)))
      (+ (lean4-indent-ts--node-indent block) lean4-indent-offset))))

(defun lean4-indent-ts--tactic-apply-argument-indent (node)
  "Return indentation for multiline tactic arguments, or nil."
  (when (and (lean4-indent-ts--inside-tactics-p node)
             (lean4-indent-ts--ancestor-type node '("tactic_apply" "tactic_rewrite")))
    (lean4-indent-ts--apply-argument-indent node)))

(defun lean4-indent-ts--calc-step-indent (node)
  "Return indentation for a `calc` step or its multiline body, or nil."
  (let ((step (lean4-indent-ts--ancestor-type node lean4-indent-ts--calc-step-types)))
    (when step
      (let ((calc (or (lean4-indent-ts--ancestor-type (treesit-node-parent step)
                                                      '("tactic_calc"))
                      step)))
        (if (= (line-number-at-pos (line-beginning-position) t)
               (lean4-indent-ts--node-start-line step))
            (+ (lean4-indent-ts--node-indent calc) lean4-indent-offset)
          (+ (lean4-indent-ts--node-indent step) lean4-indent-offset))))))

(defun lean4-indent-ts--tactic-binding-indent (node)
  "Return indentation for multiline tactic `have`/`let` values, or nil."
  (let ((binding (lean4-indent-ts--ancestor-type node
                                                 lean4-indent-ts--tactic-binding-types)))
    (when (and binding
               (> (line-number-at-pos (line-beginning-position) t)
                  (lean4-indent-ts--node-start-line binding)))
      (+ (lean4-indent-ts--node-indent binding) lean4-indent-offset))))

(defun lean4-indent-ts--tactic-config-indent (node)
  "Return indentation for multiline tactic config lists, or nil."
  (let ((config (lean4-indent-ts--ancestor-type node
                                                lean4-indent-ts--tactic-config-types)))
    (when (and config
               (> (line-number-at-pos (line-beginning-position) t)
                  (lean4-indent-ts--node-start-line config)))
      (+ (lean4-indent-ts--node-indent config) lean4-indent-offset))))

(defun lean4-indent-ts--anonymous-constructor-indent (node)
  "Return indentation for multiline anonymous-constructor elements, or nil."
  (let ((ctor (lean4-indent-ts--ancestor-type node
                                              lean4-indent-ts--constructor-types)))
    (when (and ctor
               (> (line-number-at-pos (line-beginning-position) t)
                  (lean4-indent-ts--node-start-line ctor)))
      (1+ (lean4-indent-ts--node-indent ctor)))))

(defun lean4-indent-ts--tactic-body-intro-indent (node)
  "Return indentation for multiline tactic body-intro nodes, or nil."
  (let ((intro (lean4-indent-ts--ancestor-type node
                                               lean4-indent-ts--tactic-body-intro-types)))
    (when (and intro
               (> (line-number-at-pos (line-beginning-position) t)
                  (lean4-indent-ts--node-start-line intro)))
      (+ (lean4-indent-ts--node-indent intro) lean4-indent-offset))))

(defun lean4-indent-ts--declaration-binding-indent (node)
  "Return indentation for multiline declaration `let`/`have` values, or nil."
  (let ((binding (lean4-indent-ts--ancestor-type node
                                                 lean4-indent-ts--declaration-binding-types)))
    (when (and binding
               (not (lean4-indent-ts--ancestor-type node '("do")))
               (> (line-number-at-pos (line-beginning-position) t)
                  (lean4-indent-ts--node-start-line binding)))
      (+ (lean4-indent-ts--node-indent binding) lean4-indent-offset))))

(defun lean4-indent-ts--field-assignment-indent (node)
  "Return indentation for multiline structure field bodies, or nil."
  (let* ((field (lean4-indent-ts--ancestor-type node
                                                lean4-indent-ts--field-assignment-types))
         (instance (and field
                        (lean4-indent-ts--ancestor-type
                         (treesit-node-parent field)
                         '("structure_instance"))))
         (extends-instance
          (and instance
               (treesit-node-child-by-field-name instance "extends"))))
    (when (and field
               (> (line-number-at-pos (line-beginning-position) t)
                  (lean4-indent-ts--node-start-line field)))
      (+ (if extends-instance
             (lean4-indent-ts--node-indent instance)
           (lean4-indent-ts--node-indent field))
         (* 2 lean4-indent-offset)))))

(defun lean4-indent-ts--body-intro-indent (node)
  "Return indentation for a body introduced by a structural term node."
  (let ((intro (lean4-indent-ts--ancestor-type node lean4-indent-ts--body-intro-types)))
    (when (and intro
               (> (line-number-at-pos (line-beginning-position) t)
                  (lean4-indent-ts--node-start-line intro)))
      (+ (lean4-indent-ts--node-indent intro) lean4-indent-offset))))

(defun lean4-indent-ts--compute-indent ()
  "Return tree-sitter-based indentation for the current line, or nil."
  (when (lean4-indent-ts--available-p)
    (let ((node (lean4-indent-ts--current-node)))
      (cond
       ((or (lean4-indent-ts--line-blank-p)
            (lean4-indent-ts--line-comment-p)
            (null node))
        nil)
       ((let ((top (lean4-indent-ts--top-level-command node)))
          (and top (lean4-indent-ts--top-level-line-p top)))
        0)
       ((lean4-indent-ts--tactic-block-indent node))
       ((lean4-indent-ts--tactic-apply-argument-indent node))
       ((lean4-indent-ts--calc-step-indent node))
       ((lean4-indent-ts--tactic-binding-indent node))
       ((lean4-indent-ts--tactic-config-indent node))
       ((lean4-indent-ts--anonymous-constructor-indent node))
       ((lean4-indent-ts--tactic-body-intro-indent node))
       ((lean4-indent-ts--declaration-binding-indent node))
       ((lean4-indent-ts--field-assignment-indent node))
       ((lean4-indent-ts--inside-tactics-p node)
        nil)
       ((lean4-indent-ts--top-level-continuation-indent node))
       ((lean4-indent-ts--where-decl-indent node))
       ((lean4-indent-ts--match-alt-indent node))
       ((lean4-indent-ts--fun-body-indent node))
       ((lean4-indent-ts--apply-argument-indent node))
       ((lean4-indent-ts--body-intro-indent node))
       ((lean4-indent-ts--declaration-body-indent node))))))

(defun lean4-indent-ts-line-function ()
  "Indent current line using the experimental tree-sitter indenter."
  (interactive)
  (let* ((computed (lean4-indent-ts--compute-indent))
         (eolp (eolp)))
    (if computed
        (indent-line-to (max 0 computed))
      (funcall lean4-indent-ts-fallback-function))
    (when eolp
      (end-of-line))))

(provide 'lean4-indent-ts)
;;; lean4-indent-ts.el ends here

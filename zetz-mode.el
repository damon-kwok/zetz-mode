;; -*- lexical-binding: t -*-
;;; zetz-mode.el --- A major mode for the ZetZ programming language
;;
;; Authors: Damon Kwok <damon-kwok@outlook.com>
;; Version: 0.0.1
;; URL: https://github.com/damon-kwok/zetz-mode
;; Keywords: languages programming
;; Package-Requires: ((dash "2.17.0") (hydra "0.15.0") (hl-todo "3.1.2") (yafolding "0.4.1") (yasnippet "0.14.0") (rainbow-delimiters "2.1.4") (fill-column-indicator "1.90"))
;;
;; This file is not part of GNU Emacs.
;;
;; Copyright (c) 2020 Damon Kwok
;;
;;; Commentary:
;;
;; Description:
;;
;; This is a major mode for the ZetZ programming language
;;
;; For more details, see the project page at
;; https://github.com/damon-kwok/zetz-mode
;;
;; Installation:
;;
;; The simple way is to use package.el:
;;
;;   M-x package-install zetz-mode
;;
;; Or, copy zetz-mode.el to some location in your emacs load
;; path. Then add "(require 'zetz-mode)" to your emacs initialization
;; (.emacs, init.el, or something).
;;
;; Example config:
;;
;;   (require 'zetz-mode)
;;
;;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'cl)
(require 'js)
(require 'dash)
(require 'xref)
(require 'hydra)
(require 'imenu)
(require 'hl-todo)
(require 'easymenu)
(require 'yafolding)
(require 'yasnippet)
(require 'whitespace)
(require 'rainbow-delimiters)
(require 'fill-column-indicator)

(defvar zetz-mode-hook nil)
(defcustom zetz-indent-trigger-commands ;
  '(indent-for-tab-command yas-expand yas/expand)
  "Commands that might trigger a `zetz-indent-line' call."
  :type '(repeat symbol)
  :group 'zetz)

(defconst zetz-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; fontify " using zetz-keywords

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?% ?& ?| ?= ?! ?< ?>))
      (modify-syntax-entry i "." table))

    ;; / is punctuation, but // is a comment starter
    (modify-syntax-entry ?/ ". 124" table)

    ;; /* */ comments, which can be nested
    (modify-syntax-entry ?* ". 23bn" table)

    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)

    ;; string
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)

    ;; Don't treat underscores as whitespace
    (modify-syntax-entry ?_ "w" table) table))

(defvar zetz-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    ;; (define-key map (kbd "<C-return>") 'yafolding-toggle-element) ;
    map)
  "Keymap for ZetZ major mode")

(defconst zetz-keywords
  '("if" "else" "switch" "case" "while" "for" "do" ;
    "default" "sizeof")
  "ZetZ language keywords.")

(defconst zetz-preprocessor-keywords
  '("using" "export" "pub"              ;
     "const" "test" "theory"            ;
     "assert" "closure")
  "ZetZ declaration keywords.")

(defconst zetz-declaration-keywords
  '("let"                               ;
     "enum" "struct" "fn" "trait" "symbol")
  "ZetZ declaration keywords.")

(defconst zetz-careful-keywords
  '("goto" "continue" "break" "return"  ;
     "mut" "as" "new"                   ;
     "where"                            ;
     "model"                            ;
     "unsafe")
  "ZetZ language careful keywords.")

(defconst zetz-builtin-keywords
  '("u8" "i8" "u16" "i16" "u32" "i32" "u64" "i64"    ;
     "isize" "usize" "int" "uint" "f32" "f64" "bool" ;
     "void" "char" "byte")
  "ZetZ language keywords.")

(defconst zetz-constants                ;
  '("false" "true" "self")
  "Common constants.")

(defconst zetz-operator-functions       ;
  '("len" "safe" "static_attest" "static_assert" "nullterm")
  "ZetZ language operators functions.")

;;; create the regex string for each class of keywords

(defconst zetz-keywords-regexp (regexp-opt zetz-keywords 'words)
  "Regular expression for matching keywords.")

(defconst zetz-declaration-keywords-regexp ;
  (regexp-opt zetz-declaration-keywords 'words)
  "Regular expression for matching declaration keywords.")

(defconst zetz-preprocessor-keywords-regexp ;
  (regexp-opt zetz-preprocessor-keywords 'words)
  "Regular expression for matching preprocessor keywords.")

(defconst zetz-careful-keywords-regexp  ;
  (regexp-opt zetz-careful-keywords 'words)
  "Regular expression for matching careful keywords.")

(defconst zetz-builtin-keywords-regexp (regexp-opt zetz-builtin-keywords 'words)
  "Regular expression for matching builtin type.")

(defconst zetz-constant-regexp          ;
  (regexp-opt zetz-constants 'words)
  "Regular expression for matching constants.")

(defconst zetz-operator-functions-regexp ;
  (regexp-opt zetz-operator-functions 'words)
  "Regular expression for matching operator functions.")

(defconst zetz-font-lock-keywords
  `(
     ;; builtin
     (,zetz-builtin-keywords-regexp . font-lock-builtin-face)

     ;; careful
     (,zetz-careful-keywords-regexp . font-lock-warning-face)

     ;; declaration
     (,zetz-declaration-keywords-regexp . font-lock-keyword-face)

     ;; preprocessor
     (,zetz-preprocessor-keywords-regexp . font-lock-preprocessor-face)

     ;; delimiter: modifier
     ("\\(->\\|=>\\|\\.>\\|:>\\|::\\||\\)" 1 'font-lock-keyword-face)

     ;; delimiter: . , ; separate
     ("\\($?[.,;]+\\)" 1 'font-lock-comment-delimiter-face)

     ;; delimiter: operator symbols
     ("\\($?[+-/*//%~=<>]+\\)$?,?" 1 'font-lock-negation-char-face)
     ("\\($?[?^!&]+\\)" 1 'font-lock-warning-face)

     ;; delimiter: = : separate
     ("[^+-/*//%~^!=<>]\\([=:]\\)[^+-/*//%~^!=<>]" 1 'font-lock-comment-delimiter-face)

     ;; delimiter: brackets
     ("\\(\\[\\|\\]\\|[(){}]\\)" 1 'font-lock-comment-delimiter-face)

     ;; operator methods
     (,zetz-operator-functions-regexp . font-lock-builtin-face)

     ;; macro
     ("#\\(?:include\\|if\\|ifdef\\|else\\|elif\\|endif\\)" . 'font-lock-builtin-face)

     ;; method definitions
     ("\\(?:fn\\)\s+\\($?[a-z_][A-Za-z0-9_]*\\)" 1 'font-lock-function-name-face)

     ;; type
     ;; ("\\(?:struct\\|trait\\|type\\)\s+\\($?_?[A-Z][A-Za-z0-9_]*\\)" 1 'font-lock-type-face)
     ("\\([A-Z][A-Za-z0-9_]*\\)" 1 'font-lock-type-face)
     ("\\([A-Za-z0-9_]*_t\\)" 1 'font-lock-type-face)

     ;; constants references
     (,zetz-constant-regexp . font-lock-constant-face)

     ;; @
     ("@[A-Za-z_]*[A-Z-a-z0-9_]*" . 'font-lock-builtin-face)

     ;; method references
     ("\\([a-z_]$?[a-z0-9_]?+\\)$?[ \t]?(+" 1 'font-lock-function-name-face)

     ;; parameter
     ("\\(?:(\\|,\\)\\([a-z_][a-z0-9_']*\\)\\([^ \t\r\n,:)]*\\)" 1 'font-lock-variable-name-face)
     ("\\(?:(\\|,\\)[ \t]+\\([a-z_][a-z0-9_']*\\)\\([^ \t\r\n,:)]*\\)" 1
       'font-lock-variable-name-face)

     ;; tuple references
     ("[.]$?[ \t]?\\($?_[1-9]$?[0-9]?*\\)" 1 'font-lock-variable-name-face)

     ;; keywords
     (,zetz-keywords-regexp . font-lock-keyword-face) ;;font-lock-keyword-face

     ;; character literals
     ("\\('[\\].'\\)" 1 'font-lock-constant-face)

     ;; numeric literals
     ("[ \t/+-/*//=><([,;]\\([0-9]+[0-9a-zA-Z_]*\\)+" 1 'font-lock-constant-face)

     ;; variable references
     ("\\([a-z_]+[a-z0-9_']*\\)+" 1 'font-lock-variable-name-face))
  "An alist mapping regexes to font-lock faces.")

(defun zetz-beginning-of-defun
  (&optional
    count)
  "Go to line on which current function starts."
  (interactive)
  (let ((orig-level (odin-paren-level)))
    (while (and (not (odin-line-is-defun))
             (not (bobp))
             (> orig-level 0))
      (setq orig-level (odin-paren-level))
      (while (>= (odin-paren-level) orig-level)
        (skip-chars-backward "^{")
        (backward-char))))
  (if (odin-line-is-defun)
    (beginning-of-line)))

(defun zetz-end-of-defun ()
  "Go to line on which current function ends."
  (interactive)
  (let ((orig-level (odin-paren-level)))
    (when (> orig-level 0)
      (odin-beginning-of-defun)
      (end-of-line)
      (setq orig-level (odin-paren-level))
      (skip-chars-forward "^}")
      (while (>= (odin-paren-level) orig-level)
        (skip-chars-forward "^}")
        (forward-char)))))

(defalias 'zetz-parent-mode             ;
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode zetz-mode zetz-parent-mode
  "ZetZ"
  "Major mode for editing V files."
  :syntax-table zetz-mode-syntax-table
  (setq bidi-paragraph-direction 'left-to-right)
  (setq-local require-final-newline mode-require-final-newline)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)
  (setq-local buffer-file-coding-system 'utf-8-unix)

  (setq-local parse-sexp-ignore-comments t)
  (setq-local comment-start "/*")
  (setq-local comment-start "*/")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local electric-indent-chars (append "{}():;," electric-indent-chars))
  (setq-local beginning-of-defun-function 'zetz-beginning-of-defun)
  (setq-local end-of-defun-function 'zetz-end-of-defun)
  (setq-local indent-line-function 'js-indent-line)

  ;; (setq-local font-lock-defaults        ;
  ;; '(zetz-font-lock-keywords ;
  ;; nil nil nil nil         ;
  ;; (font-lock-syntactic-face-function . zetz-mode-syntactic-face-function)))
  (setq-local font-lock-defaults '(zetz-font-lock-keywords))
  (font-lock-fontify-buffer)

  ;; (setq-local syntax-propertize-function zetz-syntax-propertize-function)
  ;;
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zz\\'" . zetz-mode))

;;
(provide 'zetz-mode)
;;; zetz-mode.el ends here

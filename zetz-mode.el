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
;; Or, copy zetz-mode.el to some location in your Emacs load
;; path.  Then add "(require 'zetz-mode)" to your Emacs initialization
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
  "Keymap for ZetZ major mode.")

(defconst zetz-keywords '("if" "else" "switch" "case" "while" "for" "do" ;
                           "default" "sizeof")
  "ZetZ language keywords.")

(defconst zetz-preprocessor-keywords
  '("using"                             ;
     "export" "pub"                     ;
     "inline" "packed")
  "ZetZ declaration keywords.")

(defconst zetz-declaration-keywords
  '("let"                               ;
     "enum" "struct" "trait"            ;
     "symbol"                           ;
     "fn" "macro" "closure")
  "ZetZ declaration keywords.")

(defconst zetz-careful-keywords
  '("new"                                       ;
     "goto" "continue" "break" "return"         ;
     "mut" "mutable" "unsafe"                   ;
     "is" "as" "needs"                          ;
     "test"                                     ;
     "assert" "assert2" "assert3" "assert4"     ;
     "static_attest" "static_assert" "nullterm" ;
     "where" "model" "theory"                   ;
     "const" "static" "atomic" "thread_local")
  "ZetZ language careful keywords.")

(defconst zetz-builtin-keywords
  '("u8" "i8" "u16" "i16" "u32" "i32" "u64" "i64"    ;
     "isize" "usize" "int" "uint" "f32" "f64" "bool" ;
     "void" "char" "byte")
  "ZetZ language keywords.")

(defconst zetz-constants                ;
  '("false" "true" "self")
  "Common constants.")

(defconst zetz-operator-functions '("len" "sizeof" "safe" )
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

     ;; C
     ;; ("#\\(?:include\\|if\\|def\\|undef\\|else\\|elif\\|endif\\)" . 'font-lock-builtin-face)
     ("#\\([A-Za-z ]+\\)" . 'font-lock-builtin-face)

     ;; method definitions
     ;; ("\\(?:fn\\)\s+\\($?[a-z_][A-Za-z0-9_]*\\)" 1 'font-lock-function-name-face)

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
    COUNT)
  "Go to line on which current function start.
Optional argument COUNT ."
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

(defun zetz-project-root-p (PATH)
  "Return t if directory `PATH' is the root of the ZetZ project."
  (setq-local files '("zz.toml" "make.bat" "Makefile" ;
                       "Dockerfile" ".editorconfig" ".gitignore"))
  (setq-local foundp nil)
  (while (and files
           (not foundp))
    (let* ((filename (car files))
            (filepath (concat (file-name-as-directory PATH) filename)))
      (setq-local files (cdr files))
      (setq-local foundp (file-exists-p filepath))))
  foundp)

(defun zetz-project-root
  (&optional
    PATH)
  "Return the root of the ZetZ project.
Optional argument PATH: project path."
  (let* ((bufdir (if buffer-file-name   ;
                   (file-name-directory buffer-file-name) default-directory))
          (curdir (if PATH (file-name-as-directory PATH) bufdir))
          (parent (file-name-directory (directory-file-name curdir))))
    (if (or (not parent)
          (string= parent curdir)
          (string= parent "/")
          (zetz-project-root-p curdir)) ;
      curdir                            ;
      (zetz-project-root parent))))

(defun zetz-project-name ()
  "Return ZetZ project name."
  (file-name-base (directory-file-name (zetz-project-root))))

(defun zetz-project-file-exists-p (FILENAME)
  "Return t if file `FILENAME' exists."
  (file-exists-p (concat (zetz-project-root) FILENAME)))

(defun zetz-run-command (COMMAND &optional PATH)
  "Return `COMMAND' in the root of the ZetZ project.
Optional argument PATH: project path."
  (setq default-directory (if PATH PATH (zetz-project-root PATH)))
  (compile COMMAND))

(defun zetz-project-build ()
  "Build project with veronac."
  (interactive)
  (if (zetz-project-file-exists-p "Makefile")
    (zetz-run-command "make")
    (zetz-run-command "veronac .")))

(defun zetz-project-open ()
  "Open `zz.toml' file."
  (interactive)
  (if (zetz-project-file-exists-p "zz.toml")
    (find-file (concat (zetz-project-root) "zz.toml"))))

(defun zetz-buffer-dirname ()
  "Return current buffer directory file name."
  (directory-file-name (if buffer-file-name (file-name-directory buffer-file-name)
                         default-directory)))

(defun zetz-project-run ()
  "Run project."
  (interactive)
  (let* ((bin1 (concat (zetz-project-root) "bin/" (zetz-project-name)))
          (bin2 (concat (zetz-project-root) "/" (zetz-project-name)))
          (bin3 (concat (zetz-buffer-dirname) "/" (zetz-project-name))))
    (if (file-exists-p bin1)
      (zetz-run-command bin1)
      (if (file-exists-p bin2)
        (zetz-run-command bin2)
        (if (file-exists-p bin3)
          (zetz-run-command bin3))))))

(easy-menu-define zetz-mode-menu zetz-mode-map ;
  "Menu for ZetZ mode."                        ;
  '("ZetZ"                                     ;
     ["Build" zetz-project-build t]            ;
     ["Run" zetz-project-run t]                ;
     "---"                                     ;
     ("Community"                              ;
       ["Home" (zetz-run-command "xdg-open https://github.com/zetzit/zz") t]
       ["ZetZ Tweets" ;;
         (zetz-run-command "xdg-open https://twitter.com/zetztweets") t])))

(defun zetz-banner-default ()
  "ZetZ banner."
  "
           _
          | |
   _______| |_ ____
  |_  / _ \\ __|_  /
   / /  __/ |_ / /
  /___\\___|\\__/___|
")

(defhydra zetz-hydra-menu
  (:color blue
    :hint none)
  "
%s(zetz-banner-default)
  Project     |  _b_: Build     _r_: Run
  Community   |  _1_: Home      _2_: News
  _q_: Quit"                            ;
  ("b" zetz-project-build "Build")
  ("r" zetz-project-run "Run")
  ("1" (v-run-command "xdg-open https://github.com/zetzit/zz") "Home")
  ("2" (v-run-command "xdg-open https://twitter.com/zetztweets") "News")
  ("q" nil "Quit"))

(defun zetz-menu ()
  "Open v hydra menu."
  (interactive)
  (zetz-hydra-menu/body))

(defun zetz-folding-hide-element
  (&optional
    RETRY)
  "Hide current element.
Optional argument RETRY."
  (interactive)
  (let* ((region (yafolding-get-element-region))
          (beg (car region))
          (end (cadr region)))
    (if (and (eq RETRY nil)
          (= beg end))
      (progn (yafolding-go-parent-element)
        (yafolding-hide-element 1))
      (yafolding-hide-region beg end))))

(defun zetz-build-tags ()
  "Build tags for current project."
  (interactive)
  (let ((tags-buffer (get-buffer "TAGS"))
         (tags-buffer2 (get-buffer (format "TAGS<%s>" (zetz-project-name)))))
    (if tags-buffer (kill-buffer tags-buffer))
    (if tags-buffer2 (kill-buffer tags-buffer2)))
  (let* ((zetz-path (string-trim (shell-command-to-string "which zz")))
          (zetz-executable (string-trim (shell-command-to-string (concat "readlink -f "
                                                                   zetz-path))))
          (packages-path (expand-file-name (concat (file-name-directory zetz-executable)
                                             "../modules")))
          (ctags-params                 ;
            (concat  "ctags " "-e -R . " packages-path)))
    (if (file-exists-p packages-path)
      (progn
        (setq default-directory (zetz-project-root))
        (shell-command ctags-params)
        (zetz-load-tags)))))

(defun zetz-load-tags
  (&optional
    BUILD)
  "Visit tags table.
Optional argument BUILD If the tags file does not exist, execute the build."
  (interactive)
  (let* ((tags-file (concat (zetz-project-root) "TAGS")))
    (if (file-exists-p tags-file)
      (progn (visit-tags-table (concat (zetz-project-root) "TAGS")))
      (if BUILD (zetz-build-tags)))))

(defun zetz-after-save-hook ()
  "After save hook."
  (shell-command (concat  "zz fmt " (buffer-file-name)))
  (revert-buffer
    :ignore-auto
    :noconfirm)
  (if (not (executable-find "ctags"))
    (message "Could not locate executable '%s'" "ctags")
    (zetz-build-tags)))

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
  ;;
  (setq-local parse-sexp-ignore-comments t)
  (setq-local comment-start "/*")
  (setq-local comment-start "*/")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  ;;
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
  ;;
  ;; (setq-local syntax-propertize-function zetz-syntax-propertize-function)
  ;;
  (hl-todo-mode)
  (setq-local hl-todo-keyword-faces ;;
    '(("TODO" . "green")
       ("FIXME" . "yellow")
       ("DEBUG" . "DarkCyan")
       ("GOTCHA" . "red")
       ("STUB" . "DarkGreen")))
  (whitespace-mode)
  (setq-local whitespace-style ;;
    '(face spaces tabs newline space-mark tab-mark newline-mark trailing))
  ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and “▷” for tab.
  (setq-local whitespace-display-mappings
    ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
    '((space-mark 32 [183]
        [46]) ;; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
       (newline-mark 10 [182 10]) ;; LINE FEED,
       (tab-mark 9 [9655 9]
         [92 9])))

  ;; (setq-local whitespace-style '(face trailing))
  (setq-local fci-rule-column 80)
  (setq-local fci-handle-truncate-lines nil)
  (setq-local fci-rule-width 1)
  (setq-local fci-rule-color "grey30")
  ;;
  (rainbow-delimiters-mode t)
  ;;
  (defalias 'yafolding-hide-element 'v-folding-hide-element)
  (yafolding-mode t)
  ;;
  (setq-local imenu-generic-expression ;;
    '(("TODO" ".*TODO:[ \t]*\\(.*\\)$" 1)
       ("fn" "[ \t]*fn[ \t]+\\([a-zA-Z0-9_]+\\)[ \t]*(.*)" 1)
       ("symbol" "[ \t]*symbol[ \t]+\\([a-zA-Z0-9_]+\\)" 1)
       ("enum" "[ \t]*enum[ \t]+\\([a-zA-Z0-9_]+\\)" 1)
       ("const" "[ \t]*const[ \t]+\\(.+\\)" 1)
       ("closure" "[ \t]*closure[ \t]+\\([a-zA-Z0-9_]+\\)" 1)
       ("macro" "[ \t]*macro[ \t]+\\([a-zA-Z0-9_]+\\)" 1)
       ("struct" "[ \t]*struct[ \t]+\\([a-zA-Z0-9_]+\\)" 1)
       ("trait" "[ \t]*trait[ \t]+\\([a-zA-Z0-9_]+\\)" 1)
       ("export" "[ \t]*export[ \t]+\\(.*\\)(" 1)
       ("pub" "[ \t]*pub[ \t]+\\(.*\\)[({]" 1)))
  (imenu-add-to-menubar "Index"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zz\\'" . zetz-mode))

;;
(provide 'zetz-mode)

;;; zetz-mode.el ends here

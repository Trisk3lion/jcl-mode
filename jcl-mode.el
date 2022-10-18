;;; jcl-mode.el --- Major mode for editing JCL - Job Control Language - files  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Kristofer Hjelmtorp

;; Author: Kristofer Hjelmtorp <kristofer.hjelmtorp@mailbox.org>
;; Keywords: languages

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


(defgroup jcl-mode nil
  "Major mode for editing JCL code."
  :group 'languages)


(defcustom jcl-outline-regexp
  (rx line-start "//"
      (1+ wordchar)
      (1+ (syntax whitespace))
      (or "EXEC" "JOB") (or "=" (syntax whitespace)))
  "Regexp for outline-minor-mode."
  :group 'jcl-mode
  :type 'regexp)

(defcustom jcl-outline-end-regexp (rx (or (1+ (syntax whitespace)) "\n"))
  "Regexp for end of section or paragraph."
  :group 'jcl-mode
  :type 'string)

(defcustom jcl-line-length 72
  "Length of standard cobol line length."
  :group 'jcl-mode
  :type 'integer)




;;; Highlightning

(defconst jcl-operators-fields
  (regexp-opt '("job" "exec" "dd" "proc" "pend" "include" "set") 'symbols))

(defconst jcl-identifier-fields
  (mapcar (lambda (x) (concat "^" x)) '("//" "//*" "/*")))

(defconst jcl-operands-fields
  (regexp-opt ("class" "msgclass" "msglevel" "user" "password" "region"
               "pgm" "cond"
               "disp" "new" "old" "keep" "catlg" "shared" "shr" "delete" "del"
               "vol" "volume" "ser" "serial"
               "dsname" "dsn"
               "dsorg"
               "notify"
               "ddname"
               "unit"
               "space" "cyl" "trk"
               "dcb" "recfm" "lrecl" "blksize"
               "sysout"
               "data" "dlm") 'paren))

(defconst jcl-jes2-statement
  "^/\\*[[:graph:]]+")

(defconst jcl-if-endif
  (regexp-opt '("if" "endif")))

(defconst jcl-procedure-statement
  "^XX")

(defconst jcl-overwritten-statement
  "^X/")


(defvar jcl-operators
  '("=" "&" "&&" "*" "(" ")" ",")
  "JCL 'operators'.  A really minimal set.")

(defvar jcl-names
  "^//\\([^*][[:graph:]]+\\)"
  "JCL names.

  These are the 'names' of jobs and steps.")


(defvar jcl-comments
  "^//\\*.*$"
  "JCL 'full card' comments.")

(defvar jcl-expanded
  "^XX.*"
  "Lines with expanded INCLUDE's.")


(defvar jcl-card-end-comments-1
  "^//[^* ]+ +[[:graph:]]* +[[:graph:]]+ +\\([[:graph:]].*\\)"
  "JCL 'end of card' comments for 'full' cards.

  Anything after the 'operands' in a card is a comment; this regexp
  selects them.")


(defvar jcl-card-end-comments-2
  "// +[[:graph:]]+ +\\([[:graph:]].*\\)"
  "JCL 'end of card' comments for 'continuation' cards.

  Anything after the 'operands' in a card is a comment; this regexp
  selects them in case of 'continuation' cards that do not have the
  'name' and 'operation'.")

;;; JCL Regexps

(defconst jcl-proc-regexp "^//.*EXEC\\s-+\\([^\\(PGM\\)]\\w+\\)")

(defconst jcl-pgm-regexp "^//.*EXEC\\s-+PGM=\\(\\w+\\)")

(defconst jcl-include-regexp "^//.*INCLUDE\\s-+MEMBER=\\(\\w+\\)")

;;; CC Specific

(defcustom jcl-cc-envs
  '("PROD" "PRODHOLD" "IK" "IKDHOLD" "PROJ")
  "CC Environments.")

(defvar jcl-cc-when
  "^))\\(WHEN\\|ELSE\\|END\\)"
  "JCL names.

These are the 'names' of jobs and steps.")

(defconst jcl-cc-when-envs
  (regexp-opt jcl-cc-envs "^))WHEN\\s-+.*\\("))

;;; JCL faces.

(defcustom jcl-string-face 'font-lock-string-face
  "The face used to fontify strings (single-quoted) in JCL mode."
  :group 'jcl
  :type 'symbol
  )

(defcustom jcl-names-face 'font-lock-function-name-face
  "The face used to fontify 'names' in JCL mode."
  :group 'jcl
  :type 'symbol
  )

(defcustom jcl-operations-face 'font-lock-keyword-face
  "The face used to fontify 'operations' in JCL mode."
  :group 'jcl
  :type 'symbol
  )

(defcustom jcl-operands-face 'font-lock-type-face
  "The face used to fontify 'operands' in JCL mode."
  :group 'jcl
  :type 'symbol
  )

(defcustom jcl-operators-face 'font-lock-builtin-face
  "The face used to fontify 'operators' in JCL mode."
  :group 'jcl
  :type 'symbol
  )

(defcustom jcl-comment-face 'font-lock-comment-face
  "The face used to fontify 'comments' in JCL mode."
  :group 'jcl
  :type 'symbol
  )

(defcustom jcl-expanded-face 'font-lock-warning-face
  "The face used to fontify expanded INCLUDE's in `jcl-mode'."
  :group 'jcl
  :type 'symbol)


(defvar jcl-font-lock-keywords
  `(
    (,jcl-strings . ,jcl-string-face)

    (,jcl-names . (1 ,jcl-names-face))

    (,jcl-cc-when . ,jcl-operands-face)

    (,jcl-cc-when-envs . (1 ,jcl-operations-face))

    (,(regexp-opt jcl-operations 'words) . ,jcl-operations-face)

    (,(regexp-opt jcl-operands 'words) . ,jcl-operands-face)

    (,(regexp-opt jcl-operators nil) . ,jcl-operators-face)

    (,jcl-proc-regexp (1 '(face nil mouse-face link)))

    (,jcl-pgm-regexp (1 '(face nil mouse-face link)))

    (,jcl-include-regexp (1 '(face nil mouse-face link)))

    (,jcl-expanded . (0 ,jcl-expanded-face t))

    ;;These must be last.
    (,jcl-card-end-comments-1 . (1 ,jcl-comment-face))
    (,jcl-card-end-comments-2 . (1 ,jcl-comment-face))
    (,jcl-comments . (0 ,jcl-comment-face t))
    )
  "The JCL mode 'font-lock' 'keyword' specification."
  )


(defvar jcl-font-lock-defaults
  (list 'jcl-font-lock-keywords
	nil ; Do syntax based processing.
	)
  "The JCL mode 'font-lock' defaults specification."
  )

;;; Utility FUNCTION

(defun jcl-locate-proc-mem (name)
  (locate-file name jcl-proc-member-library jcl-proc-mem-suffix))

;;; jcl-mode-syntax-table

(defvar jcl-mode-syntax-table
  (let ((jclst (make-syntax-table)))
    ;; (modify-syntax-entry ?/ ". 1" jclst)
    ;; (modify-syntax-entry ?* ". 2" jclst)
    ;; (modify-syntax-entry ?\n "> " jclst)
    jclst
    )
  "The JCL mode syntax table."
  )


;;; jcl-keymap

(defvar jcl-mode-map
  (let ((km (make-sparse-keymap)))
    (set-keymap-parent km prog-mode-map) ; Inherit from prog-mode-map!
    km)
  "The JCL mode key map.")


;;; jcl-imenu-generic-expression

(defvar jcl-imenu-generic-expression
  '(("Job" "//\\([^* ]*\\) +JOB" 1)		; The JOB is always first.
    ("Steps" "//\\([^* ]+\\) +EXEC" 1)
    ("SYSIN" "//\\([^* ]*SYSIN\\) +DD" 1)
    ("DD" "//\\([^* ]+\\) +DD" 1)
    ("Procedures" "//\\([^* ]+\\) +PROC" 1)
    )
  "The JCL Imenu regular expressions.")


;;; jcl-mode

(define-derived-mode jcl-mode prog-mode "JCL"
  "JCL mode is a major mode to edit IBM MVS or z/OS Job Control Language."

  :syntax-table jcl-mode-syntax-table

  (setq-local font-lock-defaults jcl-font-lock-defaults)

  (face-remap-add-relative jcl-comment-face  :weight 'bold)
  (face-remap-add-relative jcl-operators-face  :weight 'bold
			   :foreground "Forest Green") ; This may be too much.
  (face-remap-add-relative jcl-operations-face  :weight 'bold)

  (setq-local fill-column 72)

  ;; Comments.
  (setq-local comment-start "//*"
              comment-end ""
              comment-start-skip
              "^//\\*\\s-*"
              comment-column 40
              comment-use-syntax nil)

  ;; Outline-minor-mode
  (setq-local outline-level 'jcl-outline-level
              outline-regexp jcl-outline-regexp
              outline-heading-end-regexp jcl-outline-end-regexp)

  ;; Set up the mode keymap.

  (use-local-map jcl-mode-map)

  ;; Set up the menus.

  (easy-menu-define jcl-mainframe-os-menu jcl-mode-map
    "JCL commands"
    '("JCL OS"
      ["Submit" jcl-submit]
      ["Submit JCL File" jcl-submit-file])
    )

  (setq-local imenu-generic-expression
	      (reverse jcl-imenu-generic-expression))

  (imenu-add-to-menubar "JCL Code")

  ;; Start the IRON MAIN minor mode, which sets up the ruler and the
  ;; "card" editing limits, plus the fill-column indicator.

  (iron-main-mode)

  'jcl-mode
  )


;;;; Commands
;;;; ========

(defun jcl-comment-current-line ()
  "Add '*' in the third column."
  (interactive)
  (move-to-column 2)
  (delete-char 1)
  (insert "*")
  )

(defun jcl-submit (&optional port)
  "Submits the buffer's content to the 'card reader' at PORT.

The buffer contains 'JCL cards' (i.e., lines) which are submitted to a
'card reader'  listening on PORT.  PORT is an integer; its default is
3505."

  (interactive
   (let ((p (read-number "JCL: card reader number/port: " 3505))
	 )
     (list p)))

  (unless port
    (setq port 3505))

  (message "JCL: submitting to card reader number/port %s." port)

  (let ((card-reader-stream
	 (open-network-stream "JCL OS CARD READER"
			      nil
			      "127.0.0.1"
			      port
			      :type 'plain
			      ))
	)
    (unwind-protect
	(progn
	  (process-send-region card-reader-stream (point-min) (point-max))
	  (message "JCL: submitted."))
      (delete-process card-reader-stream))
    ))


(defalias 'submit 'jcl-submit)


(defun jcl-submit-file (jcl-file &optional port)
  "Submits the file JCL-FILE to the 'card reader' at PORT.

The file JCL-FILE contains 'JCL cards' (i.e., lines) which are
submitted to a 'card reader' listening on PORT.  PORT is an
integer; its default is 3505."

(interactive
 (let ((f (read-file-name "JCL: card file: " nil nil 'confirm))
       (p (read-number "JCL: card reader number/port: " 3505))
       )
   (list f p)))

(unless port
  (setq port 3505))

(message "JCL: submitting '%s' to card reader number/port %s."
	 jcl-file port)
(let ((card-reader-stream
       (open-network-stream "JCL OS CARD READER"
			    nil
			    "127.0.0.1"
			    port
			    :type 'plain
			    ))
      )
  (unwind-protect
      (with-temp-buffer
	(insert-file-contents jcl-file)
	(process-send-region card-reader-stream (point-min) (point-max))
	(message "JCL: submitted.")
	)
    (delete-process card-reader-stream))
  )
)


;;;; Epilogue
;;;; ========

;; Inspiration:
;;  - https://github.com/lsiksous/jcl-mode.el/blob/master/jcl-mode.el
;;  - https://within-parens.blogspot.com/2020/12/iron-handling-with-emacs-lisp.html

(add-to-list 'auto-mode-alist '("\\.jcl\\'" . jcl-mode))

(provide 'jcl-mode)

;;; jcl-mode.el ends here

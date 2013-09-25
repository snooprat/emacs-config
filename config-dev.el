;; ==============================
;; 括号相关
;; ==============================
;;;###autoload 
(defun skeleton-c-mode-left-brace (arg)
  "输入左大花扩号自动补齐右大花括号"
  (interactive "P")
  (if  (c-in-literal (c-most-enclosing-brace (c-parse-state)))
      (self-insert-command 1)
    ;; auto insert complex things.
    (let* ((current-line (delete-and-extract-region (line-beginning-position) (line-end-position)))
           (lines (and arg (mark t) (delete-and-extract-region (mark t) (point))))
           (after-point (make-marker)))
       ;;; delete extra blank begin and after the LINES
      (setq lines (and lines
                       (with-temp-buffer
                         (insert lines)
                         (beginning-of-buffer)
                         (delete-blank-lines)
                         (delete-blank-lines)
                         (end-of-buffer)
                         (delete-blank-lines)
                         (delete-blank-lines)
                         (buffer-string))))
      (save-excursion
        (let* ((old-point (point)))
          (insert (if current-line current-line "")  "{\n")
          (and lines (insert lines))
          (move-marker after-point (point))
          (insert "\n}")
          (indent-region old-point (point) nil)))
      (goto-char after-point)
      (c-indent-line))))

;;;###autoload
(defun ywb-indent-accoding-to-paren ()
  "按块([]{}())来格式化代码"
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char)))
        (pos (point)))
    (save-excursion
      (cond ((string-match "[[{(<]" next-char)
             (indent-region pos (progn (forward-sexp 1) (point)) nil))
            ((string-match "[\]})>]" prev-char)
             (indent-region (progn (backward-sexp 1) (point)) pos nil))))))

;;;###autoload
(defun goto-paren ()
  "跳到匹配的括号"
  (interactive)
  (cond
   ((looking-at "[ \t]*[[\"({]") (forward-sexp) (backward-char))
   ((or (looking-at "[]\")}]") (looking-back "[]\")}][ \t]*")) (if (< (point) (point-max)) (forward-char)) (backward-sexp))
   (t (message "找不到匹配的括号"))))


;; ==============================
;; flymake
;; ==============================
(defvar flymake-mode-map (make-sparse-keymap))

(autoload 'flymake-find-file-hook "flymake" "" t)

(add-hook 'find-file-hook 'flymake-find-file-hook)

(defun flymake-settings ()
  "Settings for `flymake'."
  (setq flymake-gui-warnings-enabled nil)

  ;; Underline errors instead of highlight
  (custom-set-faces
   '(flymake-errline ((((class color)) (:underline (:style wave :color "red")))))
   '(flymake-warnline ((((class color)) (:underline (:style wave :color"yellow"))))))

  (defvar flymake-makefile-filenames '("Makefile" "makefile" "GNUmakefile") "File names for make.")
  
  (defun flymake-get-make-gcc-cmdline (source base-dir)
    (let (found)
      (dolist (makefile flymake-makefile-filenames)
        (if (file-readable-p (concat base-dir "/" makefile))
            (setq found t)))
      (if found
          (list "make"
                (list "-s"
                      "-C"
                      base-dir
                      (concat "CHK_SOURCES=" source)
                      "SYNTAX_CHECK_MODE=1"
                      "check-syntax"))
        (list (if (string= (file-name-extension source) "c") "gcc" "g++")
              (list "-o"
                    "/dev/null"
                    "-S"
                    source)))))

  (defun flymake-simple-make-gcc-init-impl (create-temp-f use-relative-base-dir use-relative-source build-file-name get-cmdline-f)
    "Create syntax check command line for a directly checked source file.
Use CREATE-TEMP-F for creating temp copy."
    (let* ((args nil)
           (source-file-name buffer-file-name)
           (buildfile-dir (file-name-directory source-file-name)))
      (if buildfile-dir
          (let* ((temp-source-file-name  (flymake-init-create-temp-buffer-copy create-temp-f)))
            (setq args
                  (flymake-get-syntax-check-program-args
                   temp-source-file-name
                   buildfile-dir
                   use-relative-base-dir
                   use-relative-source
                   get-cmdline-f))))
      args))
  
  (defun flymake-simple-make-gcc-init ()
    (flymake-simple-make-gcc-init-impl 'flymake-create-temp-inplace t t "Makefile" 'flymake-get-make-gcc-cmdline))
  
  (setq flymake-allowed-file-name-masks
        '(("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" flymake-simple-make-gcc-init)
	  ;; TODO error
          ;;("\\.xml\\'" flymake-xml-init)
          ;; ("\\.html?\\'" flymake-xml-init)
          ("\\.cs\\'" flymake-simple-make-init)
          ("\\.p[ml]\\'" flymake-perl-init)
          ("\\.php[345]?\\'" flymake-php-init)
          ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup)
          ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup)
          ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
          ("\\.tex\\'" flymake-simple-tex-init)
          ("\\.idl\\'" flymake-simple-make-init)))

  (defun flymake-display-current-warning/error ()
    "Display warning/error under cursor."
    (interactive)
    (let ((ovs (overlays-in (point) (1+ (point)))))
      (dolist (ov ovs)
        (catch 'found
          (when (flymake-overlay-p ov)
            (message (overlay-get ov 'help-echo))
            (throw 'found t))))))

  (defun flymake-goto-next-error-disp ()
    "Go to next error in err ring, and then display warning/error."
    (interactive)
    (flymake-goto-next-error)
    (flymake-display-current-warning/error))

  (defun flymake-goto-prev-error-disp ()
    "Go to previous error in err ring, and then display warning/error."
    (interactive)
    (flymake-goto-prev-error)
    (flymake-display-current-warning/error))

  (defun flymake-settings-4-emaci ()
    "`flymake' settings for `emaci'."
    (emaci-add-key-definition
     "z" 'flymake-display-current-warning/error
     '(memq major-mode dev-modes)))

  (eval-after-load "emaci"
    `(flymake-settings-4-emaci))

;; pyflakes
;; (when (load "flymake" t)
  (defun flymake-pyflakes-init ()
  ;; Make sure it's not a remote buffer or flymake would not work
  ;; (when (not (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  
  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.py\\'" flymake-pyflakes-init))
  )

(provide 'config-dev)

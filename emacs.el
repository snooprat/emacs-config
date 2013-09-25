;; ==============================
;; 加载配置
;; ==============================

(defconst my-emacs-path "~/emacs/" "linux的emacs相关配置文件的路径")
(defconst emacs-lisps-path "/usr/share/emacs/24.3/lisp/" "Linux的emacs lisp包的路径")
(defconst system-head-file-dir (list "/usr/include" "/usr/local/include" "/usr/include/sys") "系统头文件目录")
(defconst user-head-file-dir   (list "." "../include") "用户头文件目录")
;; 使用daemon启动的时候,使用中文字体,光标闪烁
(add-hook 'after-make-frame-functions
	  (lambda (frame)
	    (with-selected-frame frame
	      (blink-cursor-mode t)
	      (set-fontset-font "fontset-default"
				'han '("WenQuanYi Micro Hei Mono" . "unicode-bmp")))))

(defconst my-emacs-lisps-path (concat my-emacs-path "el/") "我的emacs lisp包的路径")
(defconst my-cedet-el (concat my-emacs-lisps-path "cedet-1.1/common/cedet.el") "官方cedet.el路径")
(defconst my-auto-complete-path (concat my-emacs-lisps-path "auto-complete/") "auto complete路径")
(defconst my-yasnippet-path (concat my-emacs-lisps-path "yasnippet-0.6.1c/") "yasnippet路径")

(setq user-emacs-directory my-emacs-path)
(add-to-list 'load-path my-emacs-lisps-path)

;; 利用`eval-after-load'加快启动速度的库
;; 用eval-after-load避免不必要的elisp包的加载
;; http://emacser.com/eval-after-load.htm
(require 'eval-after-load)

;; ==============================
;; 显示设置
;; ==============================

;; 不显示开始画面
(setq inhibit-startup-message t)

;; 显示列号
(setq column-number-mode t)

;; ;; 显示行号
;; (am-add-hooks
;;  `(c-mode-common-hook emacs-lisp-mode-hook java-mode-hook sh-mode-hook python-mode-hook)
;;  (lambda()
;;    (unless (eq major-mode 'image-mode)
;;      (linum-mode t))))

;; 在fringe上显示一个小箭头指示当前buffer的边界
(setq-default indicate-buffer-boundaries 'left)

;; 尽快显示按键序列
(setq echo-keystrokes 0.1)

;; emacs lock
;; (autoload 'toggle-emacs-lock "emacs-lock" "Emacs lock" t)

;; 防止页面滚动时跳动,在靠近屏幕边沿3行时就开始滚动,可以很好的看到上下文
(setq scroll-margin 3
      scroll-conservatively 10000)

;; 缺省使用text-mode
(setq default-major-mode 'text-mode)

;; 标题栏显示完文件整路径
(setq frame-title-format
      '("%* %b ("
	(:eval (if (buffer-file-name)
		    (file-name-directory buffer-file-name)
		  "??"))
	") - "
	(:eval (user-login-name))
	"@"
        (:eval (system-name))
	))

;; 不保存连续的重复的kill
(setq kill-do-not-save-duplicates t)

;; 用y/n代替yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; 显示匹配的括号
;; (show-paren-mode t)

;; 显示图片
(auto-image-file-mode t)

;; 不显示工具栏
(tool-bar-mode -1)

;; 不显示滚动条
;; (scroll-bar-mode -1)

;; 窗口半透明
(set-frame-parameter (selected-frame) 'alpha (list 93 85)) 
(add-to-list 'default-frame-alist (cons 'alpha (list 93 85)))
;; 窗口大小
(add-to-list 'default-frame-alist '(height . 30))

;; ==============================
;; 配色
;; ==============================

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn (color-theme-initialize)
	  (color-theme-blackboard)))

;; ==============================
;; 文件操作
;; ==============================

(require 'config-file)

;; 备份文件
;; (setq backup-by-copying t				  ; 使用copy方式
;;       backup-directory-alist '(("." . "~/emacs/backup/")) ; 自动备份目录
;;       delete-old-versions t				  ; 自动删除旧的备份文件
;;       kept-new-versions 4   ; 保留最近的4个备份文件
;;       kept-old-versions 2   ; 保留最早的2个备份文件
;;       version-control t	    ; 多次备份
;;       vc-follow-symlinks t) ; 打开Link文件时自动编辑原文件
(setq make-backup-files nil	    ; 禁用自动备件
      auto-save-default nil	    ; 禁用自动保存#文件
      vc-follow-symlinks t)	    ; 打开Link文件时自动编辑原文件

;; ffap,打开当前point的文件
(ffap-bindings)
(defun ffap-settings ()
  "Settings for `ffap'."
  (setq ffap-c-path (append ffap-c-path system-head-file-dir user-head-file-dir)))
(eval-after-load "ffap"
  `(ffap-settings))

;; 切换窗口
(global-set-key (kbd "<C-tab>") 'other-window)

;; 不询问,立即关闭文件
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; 启用以下功能
;; (put 'narrow-to-region 'disabled nil)
;; (put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; ibuffe
(require 'ibuffer)
(global-set-key (kbd "C-x C-b ") 'ibuffer)

;; 显示最近打开的文件
(recentf-mode t)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; 查找文件中的关键字
;; (setq grep-find-command "find . -name \\* -type f -print0 | xargs -0 -e grep -nH -e ")
(setq grep-find-command "egrep -rnH -e ")
(global-set-key (kbd "C-M-g") 'grep-find)

;; 快速打开
(global-set-key (kbd "C-x E") 'emacs-conf)
(global-set-key (kbd "C-x W") 'my-notes)
(global-set-key (kbd "C-x J") 'my-journal)
(global-set-key (kbd "C-x T") 'my-todo-list)
(global-set-key (kbd "C-x g l") 'goto-emacs-lisps-dir)
(global-set-key (kbd "C-x g e") 'goto-my-emacs-dir)


;; ==============================
;; 编辑操作
;; ==============================

(require 'config-edit)

;; 增加更丰富的高亮
;; (require 'generic-x)

;; 鼠标中键粘贴在光标处
(setq mouse-yank-at-point t)

;; 支持emacs和外部程序的粘贴
(setq x-select-enable-clipboard t)

;; 先格式化再补全
(setq tab-always-indent 'complete)

;; 简写模式
;; (setq-default abbrev-mode t)
;; (setq save-abbrevs nil)

;; 撤销上一步
(global-set-key (kbd "C-z") 'undo)

;; 取消撤销
(global-set-key (kbd "C-S-z") 'redo)

;; 设置选区
(global-set-key (kbd "C-M-t") 'set-mark-command)

;; 返回到最近去过的地方
(require 'recent-jump)
(require 'recent-jump-small)

(setq rj-mode-line-format nil)
(setq rjs-mode-line-format nil)

(recent-jump-mode)
(recent-jump-small-mode)

(eal-define-keys-commonly
 global-map
 `(("M-[" recent-jump-backward)
   ("M-]" recent-jump-forward)
   ("M-{" recent-jump-small-backward)
   ("M-}" recent-jump-small-forward)))

;; 添加删除注释
(global-set-key (kbd "M-;") 'comment-dwim-line)

;; 复制当前行 
(global-set-key (kbd "M-k") 'copy-line)

;; 拷贝代码自动格式化
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (member major-mode
                   '(emacs-lisp-mode lisp-mode clojure-mode scheme-mode haskell-mode ruby-mode rspec-mode python-mode c-mode c++-mode objc-mode latex-mode js-mode plain-tex-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))

;; 这个功能就是根据光标的所在位置，智能的选择一块区域.
;; 如果当前光标在一个单词上，那么区域就是这个单词的开始和结尾分别。
;; 如果当前光标在一个连字符上，那么就选择包含连字符的一个标识符。
;; 如果当前光标在一个括号上，那么就会选择他们对应的另一个括号之间的区域。
;; 引号中的escape字符也是可以自动识别的。嵌套关系也是可以识别的。
(global-set-key (kbd "C-t") 'wcy-mark-some-thing-at-point)


;; ==============================
;; ORG MODE
;; ==============================

(eal-define-keys
 'org-mode-map
 `(("<C-tab>" nil)
   ("<tab>" nil)))

;; 中文自动换行
(add-hook 'org-mode-hook
	  (lambda () (setq truncate-lines nil)))

;; ;; For MobileOrg
;; ;; Set to the location of your Org files on your local system
;; (setq org-directory "~/Dropbox/testorg")
;; ;; Set to the name of the file where new notes will be stored
;; (setq org-mobile-inbox-for-pull "~/Dropbox/testorg/inbox.org")
;; ;; Set to <your Dropbox root directory>/MobileOrg.
;; (setq org-mobile-directory "~/Dropbox/MobileOrg")
;; (setq org-agenda-files (quote ("~/Dropbox/testorg/test.org")))

;; (setq org-todo-keywords
;;       '((type "TODO(t)" "STARTED(s)" "WAITING(w)" "APPT(a)" "|" "CANCELLED(c)" "DEFERRED(e)" "DONE(d)")
;; 	(sequence "PROJECT(p)" "|" "FINISHED(f)")
;; 	(sequence "INVOICE(i)" "SENT(n)" "|" "RCVD(r)")))

;; ;; 发布为html
;; (setq org-publish-project-alist
;;       '(("note-org"
;;          :base-directory "~/Documents/notes/org"
;;          :publishing-directory "~/Documents/notes/publish"
;;          :base-extension "org"
;;          :recursive t
;;          :publishing-function org-publish-org-to-html
;;          :auto-index t
;;          :index-filename "index.org"
;;          :index-title "Star's Notes"
;;          :link-home "file:///home/star/Documents/notes/publish/index.html"
;;          :section-numbers nil
;; 	 :style-include-default nil
;;          :style "<link rel=\"stylesheet\" href=\"file:///home/star/Documents/notes/publish/star.css\" type=\"text/css\" />")
;;         ("note-static"
;;          :base-directory "~/Documents/notes/org"
;;          :publishing-directory "~/Documents/notes/publish"
;;          :recursive t
;;          :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|swf\\|zip\\|gz\\|txt\\|el"
;;          :publishing-function org-publish-attachment)
;;         ("note" 
;;          :components ("note-org" "note-static")
;;          :author "starpan@gmail.com")))

;; (defun my-notes-org-publish (&optional force)
;;   "Publish my notes."
;;   (interactive)
;;   (color-theme-my-default)
;;   (org-publish (assoc "note" org-publish-project-alist) force)
;;   (color-theme-blackboard))

;; (defun my-notes-org-publish-force ()
;;   "Publish my notes by force."
;;   (interactive)
;;   (my-notes-org-publish t))

;; (global-set-key (kbd "C-c p") 'my-notes-org-publish)
;; (global-set-key (kbd "C-c M-p") 'my-notes-org-publish-force)

;; ==============================
;; 开发相关
;; ==============================

(require 'config-dev)

;; 增加自定义关键字
(dolist (mode '(c-mode c++-mode java-mode lisp-mode emacs-lisp-mode lisp-interaction-mode sh-mode sgml-mode python-mode))
  (font-lock-add-keywords
   mode
   '(("\\<\\(FIXME\\|TODO\\|Todo\\|HACK\\|STAR\\):" 1 font-lock-warning-face prepend))))

;; 输入左大花扩号自动补齐右大花括号
(eal-define-keys
 `(c-mode-base-map awk-mode-map)
 `(("{" skeleton-c-mode-left-brace)))

;; 动态检查语法错误
(eval-after-load "flymake"
  '(progn (flymake-settings)
	  ;; (setq flymake-no-changes-timeout 1)
	  (eal-define-keys
	   'flymake-mode-map
	   `(("C-c N"   flymake-goto-next-error-disp)
	     ("C-c P"   flymake-goto-prev-error-disp)
	     ("C-c W" flymake-display-current-warning/error)))))

;; 高亮匹配的括号
(require 'highlight-parentheses)
;; TODO: 最后一项不知道为啥不起作用
(setq hl-paren-colors '("red" "yellow" "cyan" "magenta" "green" "red"))
(am-add-hooks
 `(c-mode-common-hook lisp-mode-hook emacs-lisp-mode-hook java-mode-hook python-mode-hook)
 'highlight-parentheses-mode)

;; 将括号区域格式化代码
;; 跳到对应括号
(eal-define-keys-commonly
 global-map
 `(("C-M-]" ywb-indent-accoding-to-paren)
   ("C-]" goto-paren)))

;; 用来显示当前光标在哪个函数
(which-function-mode t)

;; CEDET
(load-file "/home/star/emacs/el/cedet-1.1/common/cedet.el")
(global-ede-mode 1)
(semantic-load-enable-code-helpers)
;; (semantic-load-enable-semantic-debugging-helpers)
(global-semantic-tag-folding-mode 1)

;; 代码折叠或显示
(defun my-semantic-tag-fold-or-show ()
  "Change tag state."
  (interactive)
  (let* ((tag (semantic-current-tag))
	 (state
	  (if (semantic-tag-p tag)
	      (semantic-tag-folding-get-fold-state tag nil)
	    nil)))
    (semantic-tag-folding-set-overlay-visibility
     (semantic-tag-folding-get-overlay)
     (if state (eq state 'show) t))))

(defun my-fast-semantic-mrub-swith-tags ()
  "Back to tag fast."
  (interactive)
  (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
      (error "Semantic Bookmark ring is currently empty"))
  (let* ((ring (oref semantic-mru-bookmark-ring ring))
	 (alist (semantic-mrub-ring-to-assoc-list ring))
	 (first (cdr (car alist))))
    (if (semantic-equivalent-tag-p (oref first tag)
				   (semantic-current-tag))
	(setq first (cdr (car (cdr alist)))))
    (semantic-mrub-switch-tags first)))

(eal-define-keys
 `(c-mode-base-map makefile-gmake-mode-map python-mode-map perl-mode-map sh-mode-map emacs-lisp-mode-map)
 `(("<f12>" semantic-ia-fast-jump)
   ("S-<f12>" my-fast-semantic-mrub-swith-tags)
   ("C-c J" semantic-complete-jump)
   ("C-c n" senator-next-tag)
   ("C-c p" senator-previous-tag)
   ("M-n" semantic-ia-complete-symbol-menu)))

;; auto-complete
(add-to-list 'load-path my-auto-complete-path)
(require 'auto-complete-config)
(setq ac-modes (append '(org-mode)
		      ac-modes))
(add-to-list 'ac-dictionary-directories (concat my-auto-complete-path "ac-dict/"))
(ac-config-default)
;; (global-auto-complete-mode t)

;; 不让回车的时候执行`ac-complete'
(eal-define-keys
 'ac-complete-mode-map
 `(("<return>"   nil)
   ("RET"        nil)
   ("M-j"        ac-complete)))

(defun ac-settings-4-cc ()
    (setq ac-sources
        '(ac-source-semantic
	  ac-source-yasnippet
          ac-source-dictionary
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun ac-settings-4-lisp ()
  "Auto complete settings for lisp mode."
  (setq ac-sources
        '(ac-source-features
          ac-source-functions
          ac-source-yasnippet
          ac-source-variables
          ac-source-symbols
          ac-source-dictionary
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-files-in-current-dir
          ac-source-filename
          ac-source-words-in-same-mode-buffers)))

(defun ac-settings-4-org ()
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-dictionary
          ac-source-files-in-current-dir
          ac-source-filename)))

(am-add-hooks
 `(lisp-mode-hook emacs-lisp-mode-hook lisp-interaction-mode-hook
                  svn-log-edit-mode-hook change-log-mode-hook)
 'ac-settings-4-lisp)
(am-add-hooks `(c-mode-common-hook python-mode-hook) 'ac-settings-4-cc)
(am-add-hooks 'org-mode-hook 'ac-settings-4-org)

;; yasnippet
(add-to-list 'load-path my-yasnippet-path)
(require 'yasnippet)
(yas/initialize)
(setq yas/root-directory (concat my-yasnippet-path "snippets"))
(yas/load-directory yas/root-directory)


;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; iPython
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; Display the number and location of flymake errors/warnings on the fringe
(require 'rfringe)

;; Display flymake messages in the minibuffer
(require 'flymake-cursor)

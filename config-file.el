;; 快速打开
(defun emacs-conf ()
  "open .emacs"
  (interactive)
  (find-file "~/.emacs"))

(defun my-notes ()
  "open notes"
  (interactive)
  (find-file "~/Documents/notes/org/personal.org"))

(defun my-journal ()
  "open journal"
  (interactive)
  (find-file "~/Documents/notes/journal.org"))

(defun my-todo-list ()
  "open todo list"
  (interactive)
  (find-file "~/Documents/notes/todo.org"))

(defun goto-my-emacs-dir ()
  "Goto `my-emacs-path'."
  (interactive)
  (dired my-emacs-path))

(defun goto-emacs-lisps-dir ()
  "Goto `emacs-lisps-path'."
  (interactive)
  (dired emacs-lisps-path))

(provide 'config-file)
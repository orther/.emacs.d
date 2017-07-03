;;; lang/borg/+capture.el -*- lexical-binding: t; -*-

;; Sets up two `org-capture' workflows that I like:
;;
;; 1. The traditional way: invoking `org-capture' directly (or through a
;;    command, like :org).
;;
;; 2. Through a org-capture popup frame that is invoked from outside Emacs (the
;;    script is in ~/.emacs.d/bin). This lets me open an org-capture box
;;    anywhere I can call org-capture, like, say, from qutebrowser, vimperator,
;;    dmenu or a global keybinding.

(add-hook '+borg-init-hook #'+borg|init-capture t)

(defun +borg|init-capture ()
  "Set up a sane `org-capture' workflow."

  (setq org-capture-templates
        '(
          ("t" "Task" entry
           (file+headline +borg-organizer "Inbox")
           "* TODO %?\nCAPTURED: %t\n\n%i\n")

          ("T" "Quick Task" entry
           (file+headline +borg-organizer "Inbox")
           "* TODO %?\nSCHEDULED: %t\n\n%i\n")

          ("n" "Notes" entry
           (file+headline (concat +borg-dir "notes.org") "Inbox")
           "* %u %?\n%i" :prepend t)
          ))

  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  ;; Allows the Emacs mini-frame (opened from an external shell script to run
  ;; and clean up properly) if the frame is named "org-capture".
  (require 'org-capture)
  (require 'org-protocol)
  (defun +borg*capture-init (&rest _)
    "Makes sure the org-capture window is the only window in the frame."
    (when (equal "org-capture" (frame-parameter nil 'name))
      (setq mode-line-format nil)
      (delete-other-windows)))
  (advice-add #'org-capture :after #'+borg*capture-init)

  (defun +borg|capture-finalize ()
    "Closes the frame once org-capture is done."
    (when (equal "org-capture" (frame-parameter nil 'name))
      (delete-frame)))
  (add-hook 'org-capture-after-finalize-hook #'+borg|capture-finalize))

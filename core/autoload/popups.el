;;; core/autoload/popups.el -*- lexical-binding: t; -*-

(defvar doom-popup-remember-history)

;;;###autoload
(defun doom-popup-p (&optional target)
  "Return t if TARGET (a window or buffer) is a popup. Uses current window if
omitted."
  (when-let (target (or target (selected-window)))
    (cond ((bufferp target)
           (buffer-local-value 'doom-popup-mode target))
          ((windowp target)
           (window-parameter target 'popup)))))

;;;###autoload
(defun doom-popup-buffer (buffer &optional plist extend-p)
  "Display BUFFER in a shackle popup with PLIST rules. See `shackle-rules' for
possible rules. If EXTEND-P is non-nil, don't overwrite the original rules for
this popup, just the specified properties. Returns the new popup window."
  (declare (indent defun))
  (unless (bufferp buffer)
    (error "%s is not a valid buffer" buffer))
  (shackle-display-buffer
   buffer
   nil (or (if extend-p
               (append plist (shackle-match buffer))
             plist)
           (shackle-match buffer))))

;;;###autoload
(defun doom-popup-switch-to-buffer (buffer)
  "Switch the current (or closest) pop-up window to BUFFER."
  (unless (doom-popup-p)
    (if-let (popups (doom-popup-windows))
        (select-window (car popups))
      (error "No popups to switch to")))
  (set-window-dedicated-p nil nil)
  (switch-to-buffer buffer nil t)
  (prog1 (selected-window)
    (set-window-dedicated-p nil t)))

;;;###autoload
(defun doom-popup-file (file &optional plist extend-p)
  "Display FILE in a shackle popup, with PLIST rules. See `shackle-rules' for
possible rules."
  (unless (file-exists-p file)
    (user-error "Can't display file in popup, it doesn't exist: %s" file))
  (doom-popup-buffer (find-file-noselect file t) plist extend-p))

;;;###autoload
(defun doom-popup-windows (&optional filter-static-p)
  "Get a list of open pop up windows."
  (cl-loop for window in doom-popup-windows
           if (and (doom-popup-p window)
                   (not (and filter-static-p
                             (doom-popup-property :static window))))
           collect window))

;;;###autoload
(defun doom/popup-restore ()
  "Restore the last open popups. If the buffers have been killed, and
represented real files, they will be restored. Dead special buffers or buffers
with non-nil :autokill properties will not be.

Returns t if popups were restored, nil otherwise."
  (interactive)
  (unless doom-popup-history
    (error "No popups to restore"))
  (let (any-p)
    (dolist (spec doom-popup-history)
      (let ((buffer (get-buffer (car spec)))
            (file   (plist-get (cdr spec) :file))
            (rules  (plist-get (cdr spec) :rules))
            (size   (plist-get (cdr spec) :size)))
        (when (and (not buffer) file)
          (setq buffer
                (if-let (buf (get-file-buffer file))
                    (clone-indirect-buffer (buffer-name buf) nil t)
                  (find-file-noselect file t))))
        (when size
          (setq rules (plist-put rules :size size)))
        (when (and buffer (doom-popup-buffer buffer rules) (not any-p))
          (setq any-p t))))
    (when any-p
      (setq doom-popup-history '()))
    any-p))

;;;###autoload
(defun doom/popup-toggle ()
  "Toggle popups."
  (interactive)
  (when (doom-popup-p)
    (if doom-popup-other-window
        (select-window doom-popup-other-window)
      (other-window 1)))
  (if (doom-popup-windows t)
      (let ((doom-popup-inhibit-autokill t))
        (doom/popup-close-all t))
    (doom/popup-restore)))

;;;###autoload
(defun doom/popup-close (&optional window)
  "Find and close WINDOW if it's a popup. If WINDOW is omitted, defaults to
`selected-window'. The contained buffer is buried, unless it has an :autokill
property."
  (interactive)
  (when (doom-popup-p window)
    (delete-window (or window (selected-window)))))

;;;###autoload
(defun doom/popup-close-all (&optional force-p)
  "Closes most open popups.

Does not close popups that are :static or don't have an :autoclose property (see
`shackle-rules').

If FORCE-P is non-nil (or this function is called interactively), ignore popups'
:autoclose property. This command will never close :static popups."
  (interactive
   (list (called-interactively-p 'interactive)))
  (when-let (popups (cl-loop for window in (doom-popup-windows)
                             unless (doom-popup-property :static window)
                             collect window))
    (let (success doom-popup-remember-history)
      (setq doom-popup-history (delq nil (mapcar #'doom--popup-data popups)))
      (dolist (window popups success)
        (when (or force-p (doom-popup-property :autoclose window))
          (delete-window window)
          (setq success t))))))

;;;###autoload
(defun doom/popup-kill-all ()
  "Like `doom/popup-close-all', but kill *all* popups, including :static ones,
without leaving any trace behind (muahaha)."
  (interactive)
  (when-let (popups (doom-popup-windows))
    (let (doom-popup-remember-history)
      (setq doom-popup-history nil)
      (mapc #'delete-window popups))))

;;;###autoload
(defun doom/popup-close-maybe ()
  "Close the current popup *if* its window doesn't have a noesc parameter."
  (interactive)
  (if (doom-popup-property :noesc)
      (call-interactively
       (if (featurep 'evil)
           #'evil-force-normal-state
         #'keyboard-escape-quit))
    (delete-window)))

;;;###autoload
(defun doom/popup-this-buffer ()
  "Display currently selected buffer in a popup window."
  (interactive)
  (doom-popup-buffer (current-buffer) '(:align t :autokill t)))

;;;###autoload
(defun doom/popup-toggle-messages ()
  "Toggle *Messages* buffer."
  (interactive)
  (if-let (win (get-buffer-window "*Messages*"))
      (doom/popup-close win)
    (doom-popup-buffer (get-buffer "*Messages*"))))

;;;###autoload
(defun doom-popup-properties (window-or-buffer)
  "Returns a window's popup property list, if possible. The buffer-local
`doom-popup-rules' always takes priority, but this will fall back to the popup
window parameter."
  (cond ((windowp window-or-buffer)
         (or (window-parameter window-or-buffer 'popup)
             (doom-popup-properties (window-buffer window-or-buffer))))
        ((bufferp window-or-buffer)
         (buffer-local-value 'doom-popup-rules window-or-buffer))))

;;;###autoload
(defun doom-popup-property (prop &optional window)
  "Returns a `doom-popup-rules' PROPerty from WINDOW."
  (or (plist-get (doom-popup-properties (or window (selected-window)))
                 prop)
      (pcase prop
        (:size  shackle-default-size)
        (:align shackle-default-alignment))))

;;;###autoload
(defun doom-popup-side (&optional window)
  "Return what side a popup WINDOW came from ('left 'right 'above or 'below)."
  (let ((align (doom-popup-property :align window)))
    (when (eq align t)
      (setq align shackle-default-alignment))
    (when (functionp align)
      (setq align (funcall align)))
    align))

;;;###autoload
(defun doom-popup-size (&optional window)
  "Return the size of a popup WINDOW."
  (pcase (doom-popup-side window)
    ((or 'left 'right)  (window-width window))
    ((or 'above 'below) (window-height window))))

;;;###autoload
(defmacro with-popup-rules! (rules &rest body)
  "TODO"
  (declare (indent defun))
  `(let ((old-shackle-rules shackle-rules))
     ,@(cl-loop for rule in rules
                collect `(set! :popup ,@rule))
     ,@body
     (setq shackle-rules old-shackle-rules)))

;;;###autoload
(defmacro save-popups! (&rest body)
  "Sets aside all popups before executing the original function, usually to
prevent the popup(s) from messing up the UI (or vice versa)."
  `(let ((in-popup-p (doom-popup-p))
         (popups (doom-popup-windows))
         (doom-popup-remember-history t)
         (doom-popup-inhibit-autokill t))
     (when popups
       (mapc #'doom/popup-close popups))
     (unwind-protect
         (progn ,@body)
       (when popups
         (let ((origin (selected-window)))
           (doom/popup-restore)
           (unless in-popup-p
             (select-window origin)))))))

;;;###autoload
(defun doom/other-popup (count)
  "Cycle through popup windows. Like `other-window', but for popups."
  (interactive "p")
  (if-let (popups (if (doom-popup-p)
                      (cdr (memq (selected-window) doom-popup-windows))
                    (setq doom-popup-other-window (selected-window))
                    doom-popup-windows))
      (select-window (nth (mod (1- count) (length popups)) popups))
    (unless (eq (selected-window) doom-popup-other-window)
      (when doom-popup-other-window
        (select-window doom-popup-other-window t)
        (cl-decf count))
      (when (/= count 0)
        (other-window count)))))

;;;###autoload
(defun doom/popup-raise (&optional window)
  "Turn a popup window into a normal window."
  (interactive)
  (let ((window (or window (selected-window))))
    (unless (doom-popup-p window)
      (user-error "Not a valid popup to raise"))
    (with-selected-window window
      (doom-popup-mode -1))))

;;;###autoload
(defun doom-popup-move (direction)
  "Move a popup window to another side of the frame, in DIRECTION, which can be
one of the following: 'left 'right 'above 'below"
  (when (doom-popup-p)
    (let ((buffer (current-buffer))
          (doom-popup-inhibit-autokill t))
      (doom/popup-close)
      (doom-popup-buffer buffer `(:align ,direction) 'extend))))

;;;###autoload
(defun doom/popup-move-top () "See `doom-popup-move'." (interactive) (doom-popup-move 'above))
;;;###autoload
(defun doom/popup-move-bottom () "See `doom-popup-move'." (interactive) (doom-popup-move 'below))
;;;###autoload
(defun doom/popup-move-left () "See `doom-popup-move'." (interactive) (doom-popup-move 'left))
;;;###autoload
(defun doom/popup-move-right () "See `doom-popup-move'." (interactive) (doom-popup-move 'right))

(defun doom--popup-data (window)
  (when-let (buffer (window-buffer window))
    `(,(buffer-name buffer)
      :file  ,(buffer-file-name buffer)
      :rules ,(window-parameter window 'popup)
      :size  ,(doom-popup-size window))))

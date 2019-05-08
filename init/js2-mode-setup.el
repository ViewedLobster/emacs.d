;;; js2-mode-setup --- sets up js2-mode

;;; Commentary:

;;; code:

(require 'js2-mode)
(require 'evil)

(defun js2-whitespace-setup ()
  "Set linefeed to add a comment ampersand and indentation in evil mode insert mode."
  ;; (evil-define-key 'insert js2-mode-map (kbd "RET") 'js2-newline)
  (evil-define-key 'normal js2-mode-map (kbd "o") 'js2-evil-open-below))
  ;;(evil-define-key 'insert js2-mode-map (kbd "<escape>") 'js2-evil-normal-state))

(defun js2-newline ()
  "If in block comment, insert new line and ampersand..."
  (interactive)
  (cond ((js2-block-comment-p (js2-comment-at-point))
          (when (string-match "^\\s-*/?\\*\\s-*$" (thing-at-point 'line))
            (delete-trailing-whitespace (line-beginning-position) (line-end-position)))
          (newline)
          (insert "* ")
          (indent-for-tab-command))
        (:else
          (newline))))

(defun js2-evil-open-below (count)
  "Open new line below, with same behaviour as js2-newline."
  (interactive "p")
  (cond ((and (js2-block-comment-p (js2-comment-at-point)) (not (save-excursion
                                                                  (ignore-errors
                                                                    (evil-backward-char))
                                                                  (looking-at ".*\\*/.*$"))))
          (when (string-match "^\\s-*/?\\*\\s-*$" (thing-at-point 'line))
            (delete-trailing-whitespace (line-beginning-position) (line-end-position)))
          (evil-open-below count)
          (insert "* ")
          (indent-according-to-mode))
        (:else
         (evil-open-below count))))

(defun js2-evil-normal-state (&optional arg)
  "Remove any whitespace of the current line when exiting insert state."
  (interactive "p")
  (when (js2-comment-at-point)
    (delete-trailing-whitespace (line-beginning-position) (line-end-position)))
  (evil-normal-state arg))

;; Use js-mode indentation in js2-mode, I don't like js2-mode's indentation
;;
;; thanks http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
;; with my own modifications
;;
(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      ;; (save-excursion

      ;;   (back-to-indentation)
      ;;   ;; consecutive declarations in a var statement are nice if
      ;;   ;; properly aligned, i.e:
      ;;   ;;
      ;;   ;; var foo = "bar",
      ;;   ;;     bar = "foo";
      ;;   (setq node (js2-node-at-point))
      ;;   (when (and node
      ;;              (= js2-NAME (js2-node-type node))
      ;;              (= js2-VAR (js2-node-type (js2-node-parent node))))
      ;;     (setq indentation ( 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)))))

(defun modify-js2-indentation ()
  (require 'espresso)
  (setq espresso-indent-level 2
        indent-tabs-mode nil
        c-basic-offset 2)
  (c-toggle-auto-newline 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map [(control meta q)] 'my-indent-sexp)
  ;; (if (featurep 'js2-highlight-vars)
  ;;  (js2-highlight-vars-mode))
  (message "My JS2 hook"))

(provide 'js2-mode-setup)
;;; js2-mode-setup ends here

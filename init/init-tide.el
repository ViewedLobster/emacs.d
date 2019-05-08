;;; init-tide --- functions to initialise tide

;;; Commentary:

;;; code:

(require 'evil-jumps)
(require 'tide)

(defun evilify-tide-jumps ()
  "Function to initialize tide."
  (advice-add 'tide-jump-to-definition :around
              (lambda (orig-fun &rest x)
                "Adds positions of jump to evil jump list."
                (evil-set-jump)
                (apply orig-fun x)
                (evil-set-jump))))


(provide 'init-tide)

;;; init-tide.el ends here

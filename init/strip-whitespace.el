(require 'evil-states)

(advice-add 'evil-normal-state :around
            (lambda (orig-function &rest args)
              "removes all whitespace when on an all whitespace line"
              (when (string-match "^\\s-+$" (thing-at-point 'line))
                (delete-trailing-whitespace (line-beginning-position) (line-end-position)))
              (apply orig-function args)))

(provide 'strip-whitespace)

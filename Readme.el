;;; The first tree of the local buffer is the document we want to
;;; format to text. (Another tree is the local variable code that
;;; sources this file and that we don't want to see in the output.)

(let ((map (make-sparse-keymap)))
  (set-keymap-parent map org-mode-map)
  (use-local-map map)
  (define-key map (kbd "C-x C-s")
    (lambda ()
      "Automatically export the first tree of this document when you
  save the buffer."
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^\\*\\s-+Generate sesshin documents")
        (org-ascii-export-to-ascii nil 'subtree nil 'body-only
                                   '(:ascii-charset utf-8)))
      (save-buffer)
      )))

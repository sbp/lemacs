;; Waiting for discalaimer from lars@myab.se

(setq compilation-parse-errors-hook 'compilation-parse-lint-errors)

(defun compilation-parse-lint-errors ()
  (compilation-convert-lint)
  (compilation-parse-errors))

(defun compilation-convert-lint ()
  "Modify *compilation* buffer from lint format."
  (save-excursion
    (set-buffer "*compilation*")
    (goto-char 1)
    (while (re-search-forward "^=" nil t)
      (let (filename)
	(forward-line -1)
	(beginning-of-line)
	(setq filename
	      (buffer-substring (point)
				(save-excursion (end-of-line) (point))))
	(forward-line 2)
	(while (looking-at "(")
	  (insert (concat "\"" filename "\", line "))
	  (delete-char 1)
	  (search-forward ")")
	  (delete-char -1)
	  (insert ":")
	  (forward-line 1))))))

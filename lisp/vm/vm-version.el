(provide 'vm)

(defconst vm-version "5.35.L (beta)"
  "Version number of VM.")

(defun vm-version ()
  "Returns the version numbers of VM and Emacs."
  (interactive)
  (if (interactive-p)
      (message "%s" (vm-version))
    (format "VM %s, %s" vm-version (emacs-version))))

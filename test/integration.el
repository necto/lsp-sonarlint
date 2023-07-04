;;; integration.el --- Integration tests for Sonarlint LSP client   -*- lexical-binding: t; -*-

(require 'lsp-mode)
(require 'lsp-sonarlint)
(require 'lsp-sonarlint-python)

(defun lsp-sonarlint--get-issues (file invoke-lsp-mode)
  (let ((dir (file-name-directory file)))
    (lsp-workspace-folders-add dir)
    (unwind-protect
        (let ((buf (find-file-noselect file))
              (result nil)
              (lsp-sonarlint-plugin-autodownload t))
          (with-current-buffer buf
            (funcall invoke-lsp-mode)
            (let ((diagnostics-updated nil))
              (cl-flet ((check-diags-for-file ()
                                              (when (lsp-diagnostics-stats-for file)
                                                (setq diagnostics-updated t))))
                (add-hook 'lsp-diagnostics-updated-hook #'check-diags-for-file)
                (unwind-protect
                    (with-timeout (5) (while (not diagnostics-updated) (sit-for 0.1 t)))
                  (remove-hook 'lsp-diagnostics-updated-hook #'check-diags-for-file))))
            (setq result (gethash file (lsp-diagnostics t))))
          (kill-buffer buf)
          result)
      ; TODO: lsp-workspace-shutdown?
      (lsp-workspace-folders-remove dir))))


(defun lsp-sonarlint--invoke-python-lsp-mode ()
  (let ((lsp-sonarlint-python-enabled t))
    (python-mode)
    (lsp)))

(ert-deftest lsp-sonarlint-python-reports-issues ()
  (let ((issues (lsp-sonarlint--get-issues "/home/arseniy/proj/lsp-sonarlint/fixtures/sample.py"
                                        #'lsp-sonarlint--invoke-python-lsp-mode)))
    (should (equal (length issues) 2))
    (should (equal (gethash "code" (nth 0 issues)) "python:S1135"))
    (should (equal (gethash "code" (nth 1 issues)) "python:S1716"))))


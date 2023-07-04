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
                                              (when-let ((stats (lsp-diagnostics-stats-for file)))
                                                (when (< 0 (seq-reduce '+ stats 0))
                                                  (setq diagnostics-updated t)))))
                (add-hook 'lsp-diagnostics-updated-hook #'check-diags-for-file)
                (unwind-protect
                    (with-timeout (5) (while (not diagnostics-updated) (sit-for 0.1 t)))
                  (remove-hook 'lsp-diagnostics-updated-hook #'check-diags-for-file)))
              )
            (setq result (gethash file (lsp-diagnostics t))))
          (kill-buffer buf)
          result)
      ; TODO: lsp-workspace-shutdown?
      (lsp-workspace-folders-remove dir))))

(defun lsp-sonarlint--get-issue-codes (issues)
  (sort (mapcar (lambda (issue) (gethash "code" issue)) issues) #'string-lessp))

(defun lsp-sonarlint--invoke-python-lsp-mode ()
  (let ((lsp-sonarlint-python-enabled t))
    (python-mode)
    (lsp)))

(ert-deftest lsp-sonarlint-python-reports-issues ()
  (let ((issues (lsp-sonarlint--get-issues "/home/arseniy/proj/lsp-sonarlint/fixtures/sample.py"
                                        #'lsp-sonarlint--invoke-python-lsp-mode)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("python:S1135" "python:S1716")))))


(defun lsp-sonarlint--invoke-java-lsp-mode ()
  (let ((lsp-sonarlint-java-enabled t))
    (python-mode)
    (lsp)))

(ert-deftest lsp-sonarlint-java-reports-issues ()
  (let ((issues (lsp-sonarlint--get-issues "/home/arseniy/proj/lsp-sonarlint/fixtures/sample.java"
                                        #'lsp-sonarlint--invoke-java-lsp-mode)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("java:S106" "java:S1135" "java:S1220")))))

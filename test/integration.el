;;; integration.el --- Integration tests for Sonarlint LSP client   -*- lexical-binding: t; -*-

(require 'lsp-mode)
(require 'lsp-sonarlint)
(require 'lsp-sonarlint-python)

;; TODO: check out lsp-in-sample-cpp-project for more ideas

(defun lsp-sonarlint--analyze ()
  (let* ((file "/home/arseniy/proj/lsp-sonarlint/fixture/sample.py")
         (dir (file-name-directory file)))
    (lsp-workspace-folders-add dir)
    (unwind-protect
        (let ((buf (find-file-noselect file))
              (result nil))
          (with-current-buffer buf
            (python-mode)
            (setq lsp-sonarlint-python-enabled t) ; TODO: make local
            (lsp-mode)
            (let ((diagnostics-updated nil))
              (cl-flet ((check-diags-for-file ()
                                              (when (lsp-diagnostics-stats-for file)
                                                (setq diagnostics-updated t))))
                (add-hook 'lsp-diagnostics-updated-hook #'check-diags-for-file)
                (unwind-protect
                    (with-timeout (5)
                      (while (not diagnostics-updated)
                        (sit-for 0.1 t)))
                  (remove-hook 'lsp-diagnostics-updated-hook #'check-diags-for-file)))
              )
            (setq result (gethash file (lsp-diagnostics t))))
          (kill-buffer buf)
          result)
      (lsp-workspace-folders-remove dir)
      )))

(ert-deftest lsp-sonarlint-python-reports-issues ()
  (let ((issues (lsp-sonarlint--analyze)))
    (should (equal (length issues) 2))
    (should (equal (gethash "code" (nth 0 issues)) "python:S1135"))
    (should (equal (gethash "code" (nth 1 issues)) "python:S1716"))))


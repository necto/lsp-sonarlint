;;; integration.el --- Integration tests for Sonarlint LSP client   -*- lexical-binding: t; -*-

(require 'lsp-mode)
(require 'lsp-sonarlint)
(require 'lsp-sonarlint-python)

(defun lsp-sonarlint--get-issues (file knob-symbol &optional mode)
  (setq mode (or mode #'python-mode)) ;; Any of lsp-sonarlint-modes-enabled enable all lsp-sonarlint languages
  (let ((lsp-enabled-clients '(sonarlint))
        (dir (file-name-directory file)))
    (lsp-workspace-folders-add dir)
    (unwind-protect
        (let ((buf (find-file-noselect file))
              (result nil)
              (lsp-sonarlint-plugin-autodownload t))
          (with-current-buffer buf
            (cl-letf (((symbol-value knob-symbol) t))
              (funcall mode)
              (lsp))
            (let ((diagnostics-updated nil))
              (cl-flet ((check-diags-for-file ()
                                              (when-let ((stats (lsp-diagnostics-stats-for file)))
                                                (when (< 0 (seq-reduce '+ stats 0))
                                                  (setq diagnostics-updated t)))))
                (add-hook 'lsp-diagnostics-updated-hook #'check-diags-for-file)
                (unwind-protect
                    (with-timeout (10) (while (not diagnostics-updated) (sit-for 0.1 t)))
                  (remove-hook 'lsp-diagnostics-updated-hook #'check-diags-for-file))))
            (setq result (gethash file (lsp-diagnostics t))))
          (kill-buffer buf)
          result)
      (lsp-workspace-folders-remove dir))))

(defun lsp-sonarlint--get-issue-codes (issues)
  (sort (mapcar (lambda (issue) (gethash "code" issue)) issues) #'string-lessp))

(ert-deftest lsp-sonarlint-python-reports-issues ()
  (let ((issues (lsp-sonarlint--get-issues "/home/arseniy/proj/lsp-sonarlint/fixtures/sample.py"
                                           'lsp-sonarlint-python-enabled)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("python:S1135" "python:S1716")))))

(ert-deftest lsp-sonarlint-java-reports-issues ()
  (let ((issues (lsp-sonarlint--get-issues "/home/arseniy/proj/lsp-sonarlint/fixtures/sample.java"
                                        'lsp-sonarlint-java-enabled)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("java:S106" "java:S1135" "java:S1220")))))

(ert-deftest lsp-sonarlint-html-reports-issues ()
  (let ((issues (lsp-sonarlint--get-issues "/home/arseniy/proj/lsp-sonarlint/fixtures/sample.html"
                                           'lsp-sonarlint-html-enabled)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("Web:S1135")))))

;; javascript-sample.js must have a distinct name from sample.ts,
;; otherwise the javascript/typescript plugin gets confused.
(ert-deftest lsp-sonarlint-js-reports-issues ()
  (let ((issues (lsp-sonarlint--get-issues "/home/arseniy/proj/lsp-sonarlint/fixtures/javascript-sample.js"
                                           'lsp-sonarlint-javascript-enabled)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("javascript:S1134" "javascript:S1135")))))

(ert-deftest lsp-sonarlint-ts-reports-issues ()
  (let ((issues (lsp-sonarlint--get-issues "/home/arseniy/proj/lsp-sonarlint/fixtures/sample.ts"
                                           'lsp-sonarlint-typescript-enabled)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("typescript:S1134" "typescript:S1135")))))

(ert-deftest lsp-sonarlint-php-reports-issues ()
  (let ((issues (lsp-sonarlint--get-issues "/home/arseniy/proj/lsp-sonarlint/fixtures/sample.php"
                                           'lsp-sonarlint-php-enabled)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("php:S1135" "php:S1780")))))

(ert-deftest lsp-sonarlint-xml-reports-issues ()
  (let ((issues (lsp-sonarlint--get-issues "/home/arseniy/proj/lsp-sonarlint/fixtures/sample.xml"
                                           'lsp-sonarlint-xml-enabled)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("xml:S1135")))))

;; "text" plugin detects secrets and bidirectional unicode characters
(ert-deftest lsp-sonarlint-text-reports-issues ()
  (let ((issues (lsp-sonarlint--get-issues "/home/arseniy/proj/lsp-sonarlint/fixtures/secrets.java"
                                           'lsp-sonarlint-text-enabled
                                           #'java-mode)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("java:S1118" "java:S1220" "secrets:S6290" "secrets:S6290" "secrets:S6290")))))

(ert-deftest lsp-sonarlint-go-reports-issues ()
  (let ((issues (lsp-sonarlint--get-issues "/home/arseniy/proj/lsp-sonarlint/fixtures/sample.go"
                                           'lsp-sonarlint-go-enabled)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("go:S1135")))))

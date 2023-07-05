;;; integration.el --- Integration tests for Sonarlint LSP client   -*- lexical-binding: t; -*-

(require 'lsp-mode)
(require 'lsp-sonarlint)
(require 'lsp-sonarlint-python)
(require 'lsp-sonarlint-java)
(require 'lsp-sonarlint-html)
(require 'lsp-sonarlint-javascript)
(require 'lsp-sonarlint-typescript)
(require 'lsp-sonarlint-php)
(require 'lsp-sonarlint-xml)
(require 'lsp-sonarlint-text)
(require 'lsp-sonarlint-go)

(defun lsp-sonarlint--wait-for (predicate hook timeout)
  (let ((done nil))
    (cl-flet ((setter (lambda () (when (funcall predicate)
                              (setq done t)))))
      (unwind-protect
          (progn
            (add-hook hook #'setter)
            (with-timeout (timeout) (while (not done) (sit-for 0.1 t))))
        (remove-hook hook #'setter)))))

(defun lsp-sonarlint--get-issues (file knob-symbol)
  ;; Any of lsp-sonarlint-modes-enabled enable all lsp-sonarlint languages
  (let ((lsp-enabled-clients '(sonarlint))
        (dir (file-name-directory file))
        ;; Disable all plugins to focus only on the issues from the knob-symbol
        (lsp-sonarlint-go-enabled nil)
        (lsp-sonarlint-html-enabled nil)
        (lsp-sonarlint-java-enabled nil)
        (lsp-sonarlint-javascript-enabled nil)
        (lsp-sonarlint-typescript-enabled nil)
        (lsp-sonarlint-php-enabled nil)
        (lsp-sonarlint-text-enabled nil)
        (lsp-sonarlint-typescript-enabled nil)
        (lsp-sonarlint-xml-enabled nil)
        )
    (lsp-workspace-folders-add dir)
    (unwind-protect
        (let ((buf (find-file-noselect file))
              (result nil)
              (lsp-sonarlint-plugin-autodownload t))
          (with-current-buffer buf
            (cl-letf (((symbol-value knob-symbol) t))
              ;; (print knob-symbol)
              ;; (print lsp-sonarlint-java-enabled)
              (python-mode)
              (lsp)) ;; TODO: wait for finishing the init? lsp-after-initialize-hook
            (lsp-sonarlint--wait-for
             (lambda ()
               (when-let ((stats (lsp-diagnostics-stats-for file)))
                 (when (< 0 (seq-reduce '+ stats 0))
                   (setq diagnostics-updated t))))
             'lsp-diagnostics-updated-hook
             10)
            (setq result (gethash file (lsp-diagnostics t))))
          (kill-buffer buf) ;; TODO: wait for finishing the teardown? lsp-after-uninitialized-functions lsp-unconfigure-hook
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
                                           'lsp-sonarlint-text-enabled)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("secrets:S6290" "secrets:S6290" "secrets:S6290")))))

(ert-deftest lsp-sonarlint-go-reports-issues ()
  (let ((issues (lsp-sonarlint--get-issues "/home/arseniy/proj/lsp-sonarlint/fixtures/sample.go"
                                           'lsp-sonarlint-go-enabled)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("go:S1135")))))

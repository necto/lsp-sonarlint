;;; integration.el --- Integration tests for Sonarlint LSP client   -*- lexical-binding: t; -*-

(require 'lsp-mode)
(require 'lsp-sonarlint)

(defun lsp-sonarlint--wait-for (predicate hook timeout)
  (let ((done nil))
    (cl-flet ((setter (lambda (&rest args) (when (apply predicate args)
                                        (setq done t)))))
      (unwind-protect
          (progn
            (add-hook hook #'setter)
            (with-timeout (timeout (error "Timed out waiting for %s" hook))
              (while (not done) (accept-process-output nil 0.1))))
        (remove-hook hook #'setter)))))

(defun lsp-sonarlint--any-alive-workspaces ()
  (< 0 (hash-table-count (lsp-session-folder->servers (lsp-session)))))

(defun wait-for-workspaces-to-die (timeout)
  (with-timeout (timeout (error "Timed out waiting for the workspace to shutdown"))
    (while (lsp-sonarlint--any-alive-workspaces)
      (accept-process-output nil 0.1))))

(defun lsp-sonarlint--get-issues (file knob-symbol)
  ;; It is important to start from a clean slate.
  ;; If lsp-mode runs any servers already, the test might fall into a race condition,
  ;; when a server was requested to stop, but did not quite shut down yet,
  ;; lsp-mode might reopen the connection with the new FILE, thus communicating with the
  ;; end-of-life server. This puts lsp-mode into a buggy state - it is a race condition in lsp-mode
  (should (null (lsp-sonarlint--any-alive-workspaces)))
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
        (lsp-sonarlint-xml-enabled nil))
        (let ((buf (find-file-noselect file))
              (lsp-sonarlint-plugin-autodownload t))
          (unwind-protect
              (progn
                (lsp-workspace-folders-add dir)
                (with-current-buffer buf
                  (cl-letf (((symbol-value knob-symbol) t))
                    (python-mode) ;; Any prog mode that triggers lsp-sonarlint triggers all its analyzers
                    (lsp))
                  (lsp-sonarlint--wait-for
                   (lambda ()
                     (when-let ((stats (lsp-diagnostics-stats-for file)))
                       (when (< 0 (seq-reduce '+ stats 0))
                         (setq diagnostics-updated t))))
                   'lsp-diagnostics-updated-hook
                   30)
                  (gethash file (lsp-diagnostics t))
                  ))
            (kill-buffer buf)
            (lsp-workspace-folders-remove dir)
            (wait-for-workspaces-to-die 10)))))

(defun lsp-sonarlint--get-issue-codes (issues)
  (sort (mapcar (lambda (issue) (gethash "code" issue)) issues) #'string-lessp))

(defun lsp-sonarlint--fixtures-dir ()
  (concat
   (file-name-directory
    (directory-file-name (file-name-directory (symbol-file #'lsp-sonarlint--fixtures-dir))))
   "fixtures/"))

(defun lsp-sonarlint--sample-file (fname)
  (concat (lsp-sonarlint--fixtures-dir) fname))

(ert-deftest lsp-sonarlint-python-reports-issues ()
  (require 'lsp-sonarlint-python)
  (let ((issues (lsp-sonarlint--get-issues (lsp-sonarlint--sample-file "sample.py")
                                           'lsp-sonarlint-python-enabled)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("python:S1135" "python:S1716")))))

(ert-deftest lsp-sonarlint-java-reports-issues ()
  (require 'lsp-sonarlint-java)
  (let ((issues (lsp-sonarlint--get-issues (lsp-sonarlint--sample-file "sample.java")
                                        'lsp-sonarlint-java-enabled)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("java:S106" "java:S1135" "java:S1220")))))

(ert-deftest lsp-sonarlint-html-reports-issues ()
  (require 'lsp-sonarlint-html)
  (let ((issues (lsp-sonarlint--get-issues (lsp-sonarlint--sample-file "sample.html")
                                           'lsp-sonarlint-html-enabled)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("Web:S1135")))))

;; javascript-sample.js must have a distinct name from sample.ts,
;; otherwise the javascript/typescript plugin gets confused.
(ert-deftest lsp-sonarlint-js-reports-issues ()
  (require 'lsp-sonarlint-javascript)
  (let ((issues (lsp-sonarlint--get-issues (lsp-sonarlint--sample-file "javascript-sample.js")
                                           'lsp-sonarlint-javascript-enabled)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("javascript:S1134" "javascript:S1135")))))

(ert-deftest lsp-sonarlint-ts-reports-issues ()
  (require 'lsp-sonarlint-typescript)
  (let ((issues (lsp-sonarlint--get-issues (lsp-sonarlint--sample-file "sample.ts")
                                           'lsp-sonarlint-typescript-enabled)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("typescript:S1134" "typescript:S1135")))))

(ert-deftest lsp-sonarlint-php-reports-issues ()
  (require 'lsp-sonarlint-php)
  (let ((issues (lsp-sonarlint--get-issues (lsp-sonarlint--sample-file "sample.php")
                                           'lsp-sonarlint-php-enabled)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("php:S1135" "php:S1780")))))

(ert-deftest lsp-sonarlint-xml-reports-issues ()
  (require 'lsp-sonarlint-xml)
  (let ((issues (lsp-sonarlint--get-issues (lsp-sonarlint--sample-file "sample.xml")
                                           'lsp-sonarlint-xml-enabled)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("xml:S1135")))))

;; "text" plugin detects secrets and bidirectional unicode characters
(ert-deftest lsp-sonarlint-text-reports-issues ()
  (require 'lsp-sonarlint-text)
  (let ((issues (lsp-sonarlint--get-issues (lsp-sonarlint--sample-file "secrets.java")
                                           'lsp-sonarlint-text-enabled)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("secrets:S6290" "secrets:S6290" "secrets:S6290")))))

(ert-deftest lsp-sonarlint-go-reports-issues ()
  (require 'lsp-sonarlint-go)
  (let ((issues (lsp-sonarlint--get-issues (lsp-sonarlint--sample-file "sample.go")
                                           'lsp-sonarlint-go-enabled)))
    (should (equal (lsp-sonarlint--get-issue-codes issues)
                   '("go:S1135")))))

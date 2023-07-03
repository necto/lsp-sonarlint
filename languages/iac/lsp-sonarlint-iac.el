;;; lsp-sonarlint-iac.el --- lsp-sonarlint infrastructure-as-code module   -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Fermin Munoz
;; URL: https://gitlab.com/sasanidas/lsp-sonarlint
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Especific configuration for the sonarlint IaC plugin.

;; This is NOT and official Sonarlint extension.


;;; Code:

(defgroup lsp-sonarlint-iac nil
  "lsp-sonarlint IaC analyzer group"
  :group 'lsp-sonarlint
  :version '(lsp-sonarlint-iac . "1.16.0"))

(defcustom lsp-sonarlint-iac-enabled nil
  "Enable lsp-sonarlint-iac plugin."
  :group 'lsp-sonarlint-iac
  :type 'boolean)

(defcustom lsp-sonarlint-iac-download-url
  "https://repo.maven.apache.org/maven2/org/sonarsource/iac/sonar-iac-plugin/1.16.0.3845/sonar-iac-plugin-1.16.0.3845.jar"
  "IaC plugin download URL."
  :group 'lsp-sonarlint-iac
  :type 'string)

(defcustom lsp-sonarlint-iac-analyzer-path
(concat
   (file-name-directory load-file-name)
   "sonar-iac.jar")
  "Lsp-sonarlint IaC analyzer location."
  :group 'lsp-sonarlint-iac
  :type 'file)

(defvar lsp-sonarlint-iac-doc-url "https://www.sonarsource.com/knowledge/languages/terraform/"
  "Documentation sonarsource URL.")

(defvar lsp-sonarlint-iac-repository-url "https://github.com/SonarSource/sonar-iac/"
  "Official sonarlint code extension repository.")





(provide 'lsp-sonarlint-iac)
;;; lsp-sonarlint-iac.el ends here

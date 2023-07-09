;;; lsp-sonarlint-cpp.el --- lsp-sonarlint cpp module             -*- lexical-binding: t; -*-

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
;; Specific configuration for the SonarLint cpp analyzer.
;; Not that the cpp analyzer is not available for download.
;; You can use this config only if you have the analyzer jar.
;; If you have the cfamily.jar, place it next to this file.

;; This is NOT and official Sonarlint extension.

;;; Code:

(require 'lsp-sonarlint)

(defgroup lsp-sonarlint-cpp nil
  "lsp-sonarlint cpp analyzer group"
  :group 'lsp-sonarlint
  :version '(lsp-sonarlint-cpp . "6.33.0"))

(defcustom lsp-sonarlint-cpp-enabled nil
  "Enable lsp-sonarlint-cpp plugin."
  :group 'lsp-sonarlint-cpp
  :type 'boolean)

(defcustom lsp-sonarlint-cpp-download-url
  ""
  "Cpp analyzer has no public url for download."
  :group 'lsp-sonarlint-cpp
  :type 'string)

(defcustom lsp-sonarlint-cpp-analyzer-path
  (concat (file-name-directory load-file-name) "cfamily.jar")
  "Lsp-sonarlint cpp analyzer location."
  :group 'lsp-sonarlint-cpp
  :type 'file)

(defvar lsp-sonarlint-cpp-doc-url "https://www.sonarsource.com/cpp/"
  "Documentation sonarsource URL.")

(defvar lsp-sonarlint-cpp-repository-url "https://github.com/SonarSource/sonar-cpp"
  "Official sonarlint code extension repository.")

(provide 'lsp-sonarlint-cpp)
;;; lsp-sonarlint-cpp.el ends here

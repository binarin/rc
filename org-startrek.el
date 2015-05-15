;;; org-startrek.el --- Provide ticket data from StarTrek to org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Alexey Lebedeff

;; Author: Alexey Lebedeff <binarin@binarin.ru>
;; Version: 0.1.0
;; Package-Requires: ((url "0.0") (json "0.0") (anaphora "0.0")

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(require 'request)
(require 'json)
(require 'anaphora)

(defgroup org-startrek ()
  "Fetch ticket data from startrek and use them for updating org-mode headings"
  :prefix "org-startrek-"
  :group 'external)

(defcustom org-startrek-base-url "https://st.yandex-team.ru"
  "StarTrek base URL - used for generation of URI's linking to ticket"
  :type '(string))
        
(defcustom org-startrek-api-issues-url-format
  "https://st-api.yandex-team.ru/v2/issues/%s"
  "URL format to get the issue metadata."
  :type '(string))

(defun org-startrek-oauth-token ()
  "Searches for oauth-token used for accessing startrek api. If
it is not found, queries the user about it and stores it to
passwords file. Go to
https://oauth.yandex-team.ru/authorize?response_type=token&client_id=de16c030409a41928465d52c43c88865
to get token."
  (let* ((host (url-host (url-generic-parse-url org-startrek-base-url)))
         (auth-source-creation-prompts
          '((secret . "%u for %h: ")))
         (found (nth 0 (auth-source-search :max 1
                                           :host host
                                           :port 443
                                           :user "oauth-token"
                                           :require '(:secret)
                                           :create t))))
         (if found
             (let ((secret (plist-get found :secret))
                   (save-function (plist-get found :save-function)))
               (if (functionp save-function)
                   (funcall save-function))
               (if (functionp secret)
                   (funcall secret)
                 secret))
           nil)))

(defun org-startrek-get-ticket-data (ticket-id)
  (request (format org-startrek-api-issues-url-format ticket-id)
           :params `(("oauth_token" .  ,(org-startrek-oauth-token)))
           ;; :parser (lambda ()
           ;;           (let ((json-object-type 'hash))
           ;;             (json-read)))
           :success (lambda (&key data &allow-other-keys)
                      (message "Result: %s" data))))

(provide 'org-startrek)
;;; org-startrek.el ends here

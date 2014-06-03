;;; marmalade-upload.el --- upload client for marmalade from emacs -*- lexical-binding: t -*-

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: lisp
;; Version: 0.0.1
;; Package-requires: ((web "0.4.1"))

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

;; This is a simple API V1 upload client for Emacs and marmalade-repo,
;; based on Nic Ferrier's lexical scope `web' client.

;;; Code:

(require 'web)

(defvar marmalade/tokens (make-hash-table :test 'equal)
  "The tokens used to send requests to marmalade keyed by username.

This is the result of authentication.  If you have the token you
don't need to re-authenticate.")

(defun marmalade/token-acquired (username token next)
  "Called by `marmalade/token-acquire'."
  (puthash username token marmalade/tokens)
  (funcall next))

(defconst marmalade-auth-url "http://marmalade-repo.org/v1/users/login/"
  "The URL to get the token.")

;;(defconst marmalade-auth-url "http://localhost:8000/v1/users/login/"
;;  "The URL to get the token.")

(defun marmalade/token-acquire (username password next)
  "Get the token, cache it and call the thunk NEXT."
  (let ((url marmalade-auth-url))
    (web-json-post
     (lambda (data con hdr)
       (let ((token (kva 'token data)))
         (if (not token)
             (error "marmalade-uploader: login failed - %s" data)
             ;; Else we have the token
             (marmalade/token-acquired username token next))))
     :url url
     :data `(("name" . ,username) ("password" . ,password))
     :headers '(("Accept" . "application/json")))))

(defconst marmalade-url "http://marmalade-repo.org/v1/packages"
  "The URL where we send packages.")

;;(defconst marmalade-url "http://localhost:8000/v1/packages"
;;  "The URL where we send packages.")

(defun marmalade-upload (package-buffer username &optional password)
  "Upload a package to marmalade using `web'."
  (interactive
   (append
    (list
     (if (save-excursion
           (save-match-data
             (goto-char (point-min))
             (re-search-forward "^;; Version:" nil t)))
         (current-buffer)
         ;; Else ask for a file?
         ;;;  (get-buffer (read-buffer "package file buffer: " nil t))
         (find-file-noselect (read-file-name "package file: ") t t)))
    (let ((username (read-from-minibuffer "marmalade username: ")))
      ;; Only need password if we don't have the token cached
      (if (gethash username marmalade/tokens)
          (list username)
          (list username (read-passwd "marmalade password: "))))))
  (let ((uploader
         (lambda ()
           (web-json-post
            (lambda (data con hdr)
              (let ((msg (kva "message" data)))
                (cond 
                  ((equal msg "Username or token invalid")
                   (remhash username marmalade/tokens)
                   (error "marmalade-upload: bad username or token, try again?"))
                  (t
                   (message "package uploaded: %s" data)))))
            :url marmalade-url
            :headers '(("Accept" . "application/json"))
            :data `(("name" . ,username)
                    ("token" .  ,(gethash username marmalade/tokens))
                    ("package" . ,package-buffer))
            :mime-type web-multipart-mimetype))))
    (if (equal password nil)
        (funcall uploader)
        (marmalade/token-acquire username password uploader))))


(provide 'marmalade-upload)

;;; marmalade-upload.el ends here

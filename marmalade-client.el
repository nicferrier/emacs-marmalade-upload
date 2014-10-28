;;; marmalade-client.el --- client for marmalade API from emacs -*- lexical-binding: t -*-

;; Copyright (C) 2014  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: lisp
;; Version: 0.0.11
;; Package-requires: ((web "0.4.2")(kv "0.0.19")(gh "0.8.0"))
;; Url: https://github.com/nicferrier/emacs-marmalade-upload

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

;; This is a simple API client for Emacs and marmalade-repo,
;; based on Nic Ferrier's lexical scope `web' client.

;; Mainly this is useful because it lets you upload a package to
;; marmalade-repo.  If you open a buffer with a package file in it you
;; just have to:

;;   M-x marmalade-upload [RET]

;; and enter your username and password and you're away.

;; If you're not in a package buffer then marmalade-client will ask
;; you what your package file is.  This means you can also upload
;; multi-file packages.

;; It also allows removal of packages.  Again you need to have a
;; login, you also need to be assigned an owner of a package:

;;   M-x marmalade-remove-package [RET]

;; marmalade does completion of package names based on what your Emacs
;; has downloaded from marmalade.  If a package is missing perhaps you
;; need to:

;;   M-x package-refresh-packages [RET]

;; and try again.

;; You can also add owners to your package:

;;  M-x marmalade-client-add-owner [RET]

;; This makes it easier to distribute the management of releases and
;; also to hand off a package when you get tired of maintaining it.

;;; Code:

(require 'web)
(require 'kv)
(require 'time-stamp)
(require 'gh)
(require 'gh-issues)

(defvar marmalade/tokens (make-hash-table :test 'equal)
  "The tokens used to send requests to marmalade keyed by username.

This is the result of authentication.  If you have the token you
don't need to re-authenticate.")

(defvar marmalade/default-token-folder "~/.marmalade"
  "Default folder to look for a token on disk.")

(defvar marmalade/default-token-name   nil
  "Default token name to search on disk.
If set to nil, the token filename is the user's login.")

(defun marmalade/compute-token-filepath (login)
  "Compute the token's filepath from disk.
If the default token name is not set, the LOGIN is used as the token filename."
  (let ((token-filename (if marmalade/default-token-name marmalade/default-token-name login)))
    (expand-file-name (format "%s/%s" marmalade/default-token-folder token-filename))))

(defun marmalade/token-acquired (username token next)
  "Called by `marmalade/token-acquire'."
  (puthash username token marmalade/tokens)
  (funcall next))

(defun marmalade/token-acquire (username password next)
  "Get the token, cache it and call the thunk NEXT."
  (let ((url (marmalade/get-url "/v1/users/login/")))
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

(defun marmalade/get-token-from-cache (username)
  "Try and retrieve the USERNAME's token.
First in the marmalade-upload's cache.
If not found, this will try to locate a file ~/.marmalade/<username>.
This file contains the marmalade authentication token.
If found returns it, otherwise, returns nil."
  (let ((token (gethash username marmalade/tokens)))
    (if token
        token
      (let ((token-file (marmalade/compute-token-filepath username)))
        (when (file-exists-p token-file)
          (with-temp-buffer ;; return the token file's content
            (insert-file-contents token-file)
            (buffer-string)))))))

(defun marmalade-client/log (message)
  "Log the specified MESSAGE and pop the marmalade buffer."
  (with-current-buffer (get-buffer-create "*Marmalade-client-log*")
    (setq buffer-read-only t)
    (save-excursion
      (goto-char (point-min))
      (let ((buffer-read-only nil))
        (insert message "\n\n"))))
  (display-buffer (get-buffer-create "*Marmalade-client-log*")))


(defvar marmalade/test-mode-socket nil
  "If set then marmalade will use test mode urls.")

(defun marmalade/get-url (part)
  "Get the url for talking to Marmalade URL PART."
  (if marmalade/test-mode-socket
      (format "https://%s:%d%s" "localhost" (car marmalade/test-mode-socket) part)
      ;; Else
      (format "https://%s%s" "marmalade-repo.org" part)))

(defun marmalade-test-configure (port &optional name)
  "Configure marmalade-upload urls to point to a test server.

You only need this if you're testing marmalade.

If you call with PORT < 0 it will turn test mode off."
  (interactive
   (if current-prefix-arg
       (list -1)
       ;; Return the port of the server
       (let* ((completions
               (--keep
                (cons
                 (format
                  "%d %s"
                  (car it)
                  (let ((handler (elnode/con-lookup (cdr it) :elnode-http-handler)))
                    (when (functionp handler) (documentation handler))))
                 (car it)) 
                elnode-server-socket))
              (chosen
               (kva
                (completing-read "Elnode server: " completions)
                completions)))
         (list chosen
               (format "%s"
                       (let ((handler 
                              (elnode/con-lookup
                               (kva chosen elnode-server-socket)
                               :elnode-http-handler)))
                         (when (functionp handler) (documentation handler))))))))
  (setq marmalade/test-mode-socket
        (unless (< port 0)
          (list port name)))
  (message "marmalade test mode: %s" marmalade/test-mode-socket))

;;;###autoload
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
    (let ((username (completing-read "marmalade username: " marmalade/tokens)))
      ;; Only need password if we don't have the token cached (or stored)
      (if (marmalade/get-token-from-cache username)
          (list username)
          (list username (read-passwd "marmalade password: "))))))
  (let ((uploader
         (lambda ()
           (message "marmalade-upload: starting upload now...")
           (web-json-post
            (lambda (data con hdr)
              (let ((msg (kva "message" data)))
                (cond 
                  ((equal msg "Username or token invalid")
                   (remhash username marmalade/tokens)
                   (error "marmalade-upload: bad username or token, try again?"))
                  (t
                   (marmalade-client/log
                    (format "package uploaded: %s" data))))))
            :url (marmalade/get-url "/v1/packages")
            :headers '(("Accept" . "application/json"))
            :data `(("name" . ,username)
                    ("token" .  ,(marmalade/get-token-from-cache username))
                    ("package" . ,package-buffer))
            :mime-type web-multipart-mimetype))))
    (if (equal password nil)
        (funcall uploader)
        (marmalade/token-acquire username password uploader))))

;;;###autoload
(defun marmalade-remove-package (package-name username &optional password)
  "Ask marmalade to remove a package.

You must be authorized to do so and the package must exist and
all those things.  Marmalade will give an error if it doesn't
like you."
  (interactive
   (append
    (list
     (completing-read
      "Marmalade package: "
      (with-current-buffer
          (find-file-noselect
           (format "%s/archives/marmalade/archive-contents" package-user-dir))
        (save-excursion
          (goto-char (point-min))
          (cdr (read (current-buffer)))))
      nil t))
    (let ((username (completing-read "marmalade username: " marmalade/tokens)))
      ;; Only need password if we don't have the token cached (or stored)
      (if (marmalade/get-token-from-cache username)
          (list username)
          (list username (read-passwd "marmalade password: "))))))
  (let* ((marmalade-package-url
          (marmalade/get-url (format "/v1/package/%s" package-name))))
    (let ((remover
           (lambda () ; capture the package-name
             (web-json-post
              (lambda (data con hdr)
                (let ((msg (kva "message" data)))
                  (cond 
                    ((equal msg "Username or token invalid")
                     (remhash username marmalade/tokens)
                     (error "marmalade-upload: bad username or token, try again?"))
                    (t
                     (marmalade-client/log 
                      (format "package '%s' removed: %s" package-name data))))))
              :url marmalade-package-url
              :headers '(("Accept" . "application/json"))
              :data `(("name" . ,username)
                      ("delete" . "delete")
                      ("token" .  ,(marmalade/get-token-from-cache username)))))))
      (if (equal password nil)
          (funcall remover)
          (marmalade/token-acquire username password remover)))))

;;;###autoload
(defun marmalade-client-add-owner (package-name new-username username &optional password)
  "Ask marmalade to add NEW-USERNAME as an owner of PACKAGE-NAME.

You must be authorized to do so and the package must exist and
all those things.  Marmalade will give an error if it doesn't
like you."
  (interactive
   (append
    (list
     (completing-read
      "Marmalade package: "
      (with-current-buffer
          (find-file-noselect
           (format "%s/archives/marmalade/archive-contents" package-user-dir))
        (save-excursion
          (goto-char (point-min))
          (cdr (read (current-buffer)))))
      nil t)
     (read-from-minibuffer "new owner username: "))
    (let ((username (completing-read "marmalade username: " marmalade/tokens)))
      ;; Only need password if we don't have the token cached (or stored)
      (if (marmalade/get-token-from-cache username)
          (list username)
          (list username (read-passwd "marmalade password: "))))))
  (let* ((marmalade-package-url
          (marmalade/get-url (format "/v1/package/%s" package-name))))
    (let ((owner-adder
           (lambda () ; capture the package-name and new-username
             (web-json-post
              (lambda (data con hdr)
                (let ((msg (kva "message" data)))
                  (cond 
                    ((equal msg "Username or token invalid")
                     (remhash username marmalade/tokens)
                     (error "marmalade-upload: bad username or token, try again?"))
                    (t
                     (marmalade-client/log 
                      (format "package '%s' updated: %s" package-name data))))))
              :url marmalade-package-url
              :headers '(("Accept" . "application/json"))
              :data `(("name" . ,username)
                      ("token" .  ,(marmalade/get-token-from-cache username))
                      ("addowner" . "addowner")
                      ("new-owner" . ,new-username))))))
      (if (equal password nil)
          (funcall owner-adder)
          (marmalade/token-acquire username password owner-adder)))))

(defun fill-string (str)
  (with-temp-buffer
    (insert str)
    (fill-paragraph)
    (buffer-string)))

;;;###autoload
(defun marmalade-client-list-issues ()
  (interactive)
  (let ((ghcon (gh-issues-api "api")))
    (with-current-buffer (get-buffer-create "*marmalade-issues*")
      (let ((buffer-read-only nil))
        (erase-buffer)
        (--map
         (insert
          (format
           "#%s %s -- %s\n%s\n\n-------------------\n"
           (oref it number)
           (oref it created_at)
           (fill-string (oref it title))
           (fill-string (replace-regexp-in-string "\r" "\n" (oref it body)))))
         (oref
          (gh-issues-issue-list ghcon "nicferrier" "elmarmalade")
          data)))
      (setq buffer-read-only t)
      (pop-to-buffer (current-buffer)))))

(provide 'marmalade-client)

;;; marmalade-client.el ends here

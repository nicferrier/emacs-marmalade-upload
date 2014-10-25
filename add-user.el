;;; -*- lexical-binding: t -*-

(require 'marmalade-client)

(defun marmalade-make-user (new-username new-email username &optional password)
  "Ask marmalade to add NEW-USERNAME with NEW-EMAIL.

You must be authorized to do so and the package must exist and
all those things.  Marmalade will give an error if it doesn't
like you."
  (interactive
   (append
    (list
     (read-from-minibuffer "New username for marmalade: ")
     (read-from-minibuffer "User's email address: "))
    (let ((username (completing-read "marmalade username: " marmalade/tokens)))
      ;; Only need password if we don't have the token cached (or stored)
      (if (marmalade/get-token-from-cache username)
          (list username)
          (list username (read-passwd "marmalade password: "))))))
  (let* ((marmalade-user-url (marmalade/get-url "/v1/users/add/")))
    (let ((user-adder
           (lambda () ; capture the package-name
             (web-json-post
              (lambda (data con hdr)
                (message "make-user data: %S" data)
                (let ((msg (kva "message" data)))
                  (if (equal msg "Username or token invalid")
                      (progn
                        (remhash username marmalade/tokens)
                        (error "marmalade-upload: bad username or token, try again?"))
                      ;; Else display a buffer with the data in it
                      (with-current-buffer
                          (get-buffer-create
                           (format "*marmalade-add-user-%s*" new-username))
                        (erase-buffer)
                        (insert
                         (format
                          "To: %s
From: nic@ferrier.me.uk
Subject: marmalade-repo.org user registration
--text follows this line--
Your user has been created on marmalade with username: %s

Please follow this:

  %s

To create a password. You will then be redirected to login.

Please do NOT LOSE YOUR PASSWORD as we don't have any password
recovery right now.

Nic Ferrier"
                          new-email
                          new-username
                          (marmalade/get-url
                           (concat
                            "/verify/"
                            (or
                             (kva 'verified-code data)
                             (kva "verified-code" data))))))
                        (message-mode)
                        (pop-to-buffer (current-buffer))))))
              :headers '(("Accept" . "application/json"))
              :url marmalade-user-url
              :data `(("name" . ,username)
                      ("new-username" . ,new-username)
                      ("new-email" . ,new-email)
                      ("token" . ,(marmalade/get-token-from-cache username)))))))
      (if (equal password nil)
          (funcall user-adder)
          (marmalade/token-acquire username password user-adder)))))

;;; end

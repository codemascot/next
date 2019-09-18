(uiop:define-package :next/git
    (:use :common-lisp :trivia :next)
  (:export :*git-projects-roots*)
  (:documentation "Interact with Git repositories.

- clone a git repository
  - [X] to disk
  - on Github
  - on Gitlab
- be notified if you have unpushed changes
"))

(in-package :next/git)
(annot:enable-annot-syntax)


(defparameter *git-projects-roots* '("~/projects" "~/work")
  "A list of directories to look for Git repositories to.")

(defparameter *git-projects* '()
  "Currently registered git projects.")

(defun search-git-directories (dir)
  "Search all directories that contain a .git/ subdirectory, one level deep inside DIR."
  (when (uiop:directory-pathname-p (uiop:ensure-directory-pathname dir))
    (loop for dir in (uiop:subdirectories dir)
       for git-dir = (merge-pathnames dir ".git/")
       when (uiop:directory-exists-p git-dir)
       collect dir)))

(defun parse-projects ()
  "Scan `*git-projects-roots*' and register git repositories."
  (setf *git-projects*
        (mapcan #'search-git-directories *git-projects-roots*)))

(defun find-project-directory (name &key exit)
  "Return the directory pathname of the project named NAME."
  (unless *git-projects*
    (parse-projects))
  (let ((result (find name *git-projects*
                      :key (lambda (dir)
                             (car (last (str:split "/" (namestring dir) :omit-nulls t))))
                      :test #'string=)))
    (unless (or result
                exit)
      (log:info "parsing again")
      (parse-projects)
      (setf result (find-project-directory name :exit t)))
    result))

(defun concat-filenames (base dir)
  "Concat filenames. Handle a tilde in BASE.
  Create BASE if it doesn't exist."
  ;; ensure a trailing slash.
  (setf base
        (str:concat (string-right-trim (list #\/) base)
                    "/"))
  (ensure-directories-exist base)
  ;; truename fails if the directory doesn't actually exist.
  (let* ((base-truename-path (truename base))
         (base-truename-string (namestring base-truename-path))
         (dir-string (string-trim (list #\/) (namestring dir)))
         (joined (str:concat base-truename-string dir-string)))
    joined))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :next)

(define-command git-clone ()
  "Clone the repository of the current url (if any), both in the user's Github account and locally."
  (with-result (url (buffer-get-url))
    (let* ((uri (quri:uri url))
           ;TODO: cleanup
           (root-name (first (str:split "/" (quri:uri-path uri) :omit-nulls t)))
           (project-name (second (str:split "/" (quri:uri-path uri) :omit-nulls t)))
           (clone-uri (quri:copy-uri uri))
           (existing-git (next/git::find-project-directory project-name)))
      (if project-name
          (if existing-git
              (echo "This repository exists in ~a" existing-git)
              (progn
                ;TODO: ask for destination.
                (echo "Cloning ~a into ~a" project-name (first next/git::*git-projects-roots*))
                (setf (quri:uri-path clone-uri)
                      (str:concat "/" root-name "/" project-name))
                (handler-case (progn
                                (uiop:launch-program
                                 (list "git" "clone"
                                       (quri:render-uri clone-uri)
                                       (next/git::concat-filenames (first next/git::*git-projects-roots*) project-name))))
                  (error (c)
                    (log:warn "Error cloning ~a: ~a" project-name c)
                    (echo "There was an error cloning ~a." project-name)))))
          (echo "Could not find the git project name.")))))

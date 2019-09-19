(uiop:define-package :next/git
    (:use :common-lisp :trivia :next)
  (:export :*git-projects-roots*)
  (:documentation "Interact with Git repositories.

New command: git-clone, to clone a Git repository on disk.

Change the `*git-projects-roots*' list to define where to look for existing Git repositories on disk.

The clone command is run asynchronously.

Much can be added! We could clone on Github/Gitlab, be notified if we have unpushed changes, browse files in a text editor...
"))

(in-package :next/git)

(defparameter *git-projects-roots* '("~/projects" "~/src" "~/work" "~/common-lisp" "~/quicklisp/local-projects")
  "A list of directories to look for Git repositories to.")
;; Possible improvement: specify the depth to look for projects alongside the directory.
;; See magit-list-repositories.

(defparameter *git-projects* '()
  "Currently registered Git projects.")

(defun search-git-directories (dir)
  "Search all directories that contain a .git/ subdirectory, one level deep inside DIR."
  (when (uiop:directory-pathname-p (uiop:ensure-directory-pathname dir))
    (loop for dir in (uiop:subdirectories dir)
       for git-dir = (merge-pathnames dir ".git/")
       when (uiop:directory-exists-p git-dir)
       collect dir)))

(defun parse-projects ()
  "Scan `*git-projects-roots*'."
  (mapcan #'search-git-directories *git-projects-roots*))

(defun find-project-directory (name &key exit)
  "Return the directory pathname of the project named NAME.
If EXIT is true and the project was not found, don't parse the projects roots again."
  (unless *git-projects*
    (setf *git-projects* (parse-projects)))
  (let ((result (find name *git-projects*
                      :key (lambda (dir)
                              (alexandria:last-elt (str:split "/" (namestring dir) :omit-nulls t)))
                      :test #'string=)))
    (unless (or result
                exit)
      (setf *git-projects* (parse-projects))
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
  ;; truename expands the tilde, but fails if the directory doesn't actually exist.
  (let* ((base-truename-path (truename base))
         (base-truename-string (namestring base-truename-path))
         (dir-string (string-trim (list #\/) (namestring dir)))
         (joined (str:concat base-truename-string dir-string)))
    joined))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :next)

(define-command git-clone ()
  "Clone the repository of the current url to disk (if any)."
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
          (echo "Could not find the Git project name.")))))

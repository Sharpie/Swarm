;; Copyright � 1996-2000 Swarm Development Group

(require 'cl)
(provide 'common)

(defconst *swarm-modules* '(activity
                            analysis
                            collections
                            defobj
                            gui
                            objectbase
                            random
                            (random . "generators.h")
                            (random . "distributions.h")
                            simtools
                            simtoolsgui
                            space
                            swarm))

(defun swarm-modules ()
  (loop for module-sym in *swarm-modules*
        unless (consp module-sym)
        collect module-sym))

(defun get-swarmsrcdir ()
  (concat
   (let ((swarmsrcdir-env (getenv "SWARMSRCDIR")))
     (if swarmsrcdir-env
         swarmsrcdir-env
         (if (> (length command-line-args) 1)
             (car (last command-line-args))
             (error "Can't find SWARMSRCDIR"))))
   "/"))

(defun get-swarmdocs ()
  (let ((swarmdocs-env (getenv "SWARMDOCS")))
    (if swarmdocs-env
        swarmdocs-env
        (concat (get-swarmsrcdir) "../swarmdocs"))))

(defun header-filename-for-module-sym (module-sym)
  (concat (symbol-name module-sym) ".h"))

(defun pathname-for-module-sym (module-sym &optional filename)
  (let ((module-name (symbol-name module-sym))
        (file 
         (if filename
             filename
           (header-filename-for-module-sym module-sym))))
    (if (eq module-sym 'swarm)
        (concat (get-swarmsrcdir) "src/" file)
      (concat (get-swarmsrcdir) "src/" module-name "/" file))))

(defun pathname-for-swarmdocs (module-sym filename)
  (let ((module-name (symbol-name module-sym)))
    (concat (get-swarmdocs) 
            (case module-sym
              ((refbook set installbook overbook) (concat module-name "/" filename))
              (otherwise (concat "refbook/" module-name "/" filename))))))

(defun ensure-ending-slash (str)
  (if (string= (substring str -1) "/")
      str
      (concat str "/")))

(defun get-builddir ()
  (ensure-ending-slash (getenv "BUILDDIR")))

(defun get-top-builddir ()
  (ensure-ending-slash (getenv "TOP_BUILDDIR")))

(defun insert-text (text)
  (let ((beg (point)))
    (insert text)
    (let ((end (point)))
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (save-excursion
            (while (search-forward "<" nil t)
              (replace-match "&lt;")))
          (goto-char (point-min))
          (save-excursion
            (while (search-forward ">" nil t)
              (replace-match "&gt;")))
          (goto-char (point-min))
          (save-excursion
            (while (search-forward "\t" nil t)
              (replace-match ""))))))))

(defun strip-regexp (str strip-str)
  (with-output-to-string
    (with-temp-buffer
      (let ((beg (point)))
        (insert str)
        (let ((end (point)))
          (save-excursion
            (save-restriction
              (narrow-to-region beg end)
              (goto-char (point-min))
              (while (re-search-forward strip-str nil t)
                (replace-match ""))))))
      (princ (buffer-string)))))

(defun cook-id (id)
  (strip-regexp id "_"))

(defvar *old-push-mark* (symbol-function 'push-mark))

(defun set-verbosity (verbose)
  (if verbose
      (setf (symbol-function 'push-mark) *old-push-mark*)
    (progn
      (setq *old-push-mark* (symbol-function 'push-mark))
      
      (setf (symbol-function 'push-mark)
            #'(lambda () 
                (funcall *old-push-mark* nil t))))))

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
                            space))

(defun swarm-modules ()
  (loop for module in *swarm-modules*
        unless (consp module)
        collect module))

(defun get-swarmhome ()
  (concat
   (let ((swarmhome-env (getenv "SWARMHOME")))
     (if swarmhome-env
         swarmhome-env
         (if (> (length command-line-args 1))
             (car (last command-line-args))
             (error "Can't find SWARMHOME"))))
   "/"))

(defun get-swarmdocs ()
  (let ((swarmdocs-env (getenv "SWARMDOCS")))
    (if swarmdocs-env
        swarmdocs-env
        (concat (get-swarmhome) "../swarmdocs"))))

(defun header-filename-for-module (module)
  (concat (symbol-name module) ".h"))

(defun pathname-for-module (module &optional filename)
  (let ((module-name (symbol-name module)))
    (concat (get-swarmhome) "src/" module-name "/" 
            (if filename
                filename
                (header-filename-for-module module)))))

(defun pathname-for-swarmdocs (module filename)
  (let ((module-name (symbol-name module)))
    (concat (get-swarmdocs) "src/" module-name "/" filename)))

(defun get-swarmdocs-build-area ()
    (getenv "SWARMDOCS_BUILD_AREA"))

(defun pathname-for-swarmdocs-pages-output (module)
  (let ((module-name (symbol-name module)))
    (concat (get-swarmdocs-build-area)
            "src/"
            module-name
            "/"
            module-name
            "pages.sgml")))

(defun pathname-for-swarmdocs-revision-output (module)
  (let ((module-name (symbol-name module)))
    (concat (get-swarmdocs-build-area)
            "src/"
            module-name
            "/"
            module-name
            "revhistory.sgml")))
  
(defun insert-text (text)
  (when text
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
            (save-excursion
              (while (search-forward ">" nil t)
                (replace-match "&gt;")))))))))


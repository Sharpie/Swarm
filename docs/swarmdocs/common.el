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
  (loop for module-sym in *swarm-modules*
        unless (consp module-sym)
        collect module-sym))

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

(defun header-filename-for-module-sym (module-sym)
  (concat (symbol-name module-sym) ".h"))

(defun pathname-for-module-sym (module-sym &optional filename)
  (let ((module-name (symbol-name module-sym)))
    (concat (get-swarmhome) "src/" module-name "/" 
            (if filename
                filename
                (header-filename-for-module-sym module-sym)))))

(defun pathname-for-swarmdocs (module-sym filename)
  (let ((module-name (symbol-name module-sym)))
    (cond ((eq module-sym 'src)
           (concat (get-swarmdocs) "src/" filename))
          ((eq module-sym 'set)
           (concat (get-swarmdocs) "set/" filename)) 
          ((eq module-sym 'install)
           (concat (get-swarmdocs) "install/" filename))
          ((eq module-sym 'over)
           (concat (get-swarmdocs) "over/" filename))
          (t
           (concat (get-swarmdocs) "src/" module-name "/" filename)))))

(defun get-swarmdocs-build-area ()
    (getenv "SWARMDOCS_BUILD_AREA"))

(defun pathname-for-swarmdocs-pages-output (module-sym)
  (let ((module-name (symbol-name module-sym)))
    (concat (get-swarmdocs-build-area)
            "src/"
            module-name
            "/"
            module-name
            "pages.sgml")))

(defun pathname-for-swarmdocs-revision-output (module-sym)
  (let ((module-name (symbol-name module-sym)))
    (cond ((eq module-sym 'src)
           (concat (get-swarmdocs-build-area)
                   "src/srcrevhistory.sgml"))
          ((eq module-sym 'set)
           (concat (get-swarmdocs-build-area)
                   "set/setrevhistory.sgml"))
          ((eq module-sym 'install)
           (concat (get-swarmdocs-build-area)
                   "install/installrevhistory.sgml")) 
          ((eq module-sym 'over)
           (concat (get-swarmdocs-build-area)
                   "over/overrevhistory.sgml"))
          (t
           (concat (get-swarmdocs-build-area)
                   "src/"
                   module-name
                   "/"
                   module-name
                   "revhistory.sgml")))))
  
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


;; Copyright © 1996-2000 Swarm Development Group
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA
;; 
;; The Swarm Development Group can be reached via our website at:
;; http://www.swarm.org/

(require 'cl)
(provide 'common)

(defvar *disable-gui* nil)

(defconst *swarm-modules* 
    (if (not *disable-gui*)
       '(activity
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
         SwarmTop)
       '(activity
         analysis
         collections
         defobj
         objectbase
         random
         (random . "generators.h")
         (random . "distributions.h")
         simtools
         space
         SwarmTop)))
        

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
    (if (eq module-sym 'SwarmTop)
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

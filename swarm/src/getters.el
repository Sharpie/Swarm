;; Copyright © 2000 Swarm Development Group
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
(eval-and-compile
 (push (getenv "TOP_BUILDDIR") load-path))
(require 'protocol)
(require 'interface) ; get-variable-name-for-getter-method

(defun print-arg (argument)
  (let ((type (argument-type argument)))
    (insert ":")
    (when type (insert type))
    (insert (argument-name argument))))

(defun print-method-declaration (method)
  (insert (if (method-factory-flag method) "+" "-"))
  (insert " ")
  (let ((ret (method-return-type method)))
    (when ret
      (insert "(")
      (insert ret)
      (insert ")")))
  (let* ((arguments (method-arguments method))
         (first-argument (first arguments)))
    (insert (argument-key first-argument))
    (when (has-arguments-p method)
      (print-arg first-argument)
      (loop for argument in (cdr arguments)
            do
            (insert (argument-key argument))
            (print-arg argument)))))

(defun generate-getters-header (protocol)
  (with-temp-file (concat (get-swarmsrcdir) "src/"
                          (protocol-name protocol) "_getters.h")
    (loop for method in (protocol-method-list protocol)
          when (eq (method-phase method) :getters)
          do
          (print-method-declaration method)
          (insert ";\n"))))

(defun generate-getters-implementation (protocol)
  (with-temp-file (concat (get-swarmsrcdir) "src/"
                          (protocol-name protocol) "_getters.m")
    (loop for method in (protocol-method-list protocol)
          when (eq (method-phase method) :getters)
          do
          (print-method-declaration method)
          (insert "\n{\n")
          (insert "  return ")
          (insert
           (let ((name (get-variable-name-for-getter-method method)))
             (if (string= name "guiFlag")
                 "swarmGUIMode"
               name)))
          (insert ";\n")
          (insert "}\n"))))

(defun generate-SwarmEnvironment-getters ()
  (load-and-process-modules :uniquify-method-lists t)
  (let ((env (lookup-protocol "SwarmEnvironment")))
    (generate-getters-header env)
    (generate-getters-implementation env)))
  

;; Copyright © 2000 Swarm Development Group

(require 'cl)
(eval-and-compile
 (push (getenv "TOP_BUILDDIR") load-path))
(require 'protocol)

(defun print-method-declaration (method)
  (insert (if (method-factory-flag method) "+" "-"))
  (insert " ")
  (let ((ret (method-return-type method)))
    (when ret
      (insert "(")
      (insert ret)
      (insert ")")))
  (loop for argument in (method-arguments method)
        do
        (insert (first argument))
        (unless (argument-empty-p argument)
          (insert ": ")
          (when (second argument)
            (insert (second argument)))
          (insert (third argument)))))
  
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
          (let* ((first-argument (first (method-arguments method)))
                 (name (strip-regexp (first first-argument) "^get"))
                 (ret-type (method-return-type method)))
            (insert
             (cond ((string= "id <Symbol>" ret-type) name)
                   ((string= "GuiFlag" name) "swarmGUIMode")
                   (t (concat (downcase (substring name 0 1))
                              (substring name 1))))))
          (insert ";\n")
          (insert "}\n"))))

(defun generate-SwarmEnvironment-getters ()
  (load-and-process-modules :uniquify-method-lists t)
  (let ((env (lookup-protocol "SwarmEnvironment")))
    (generate-getters-header env)
    (generate-getters-implementation env)))
  

;; Copyright © 2000 Swarm Development Group

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
  

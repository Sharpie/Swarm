(require 'cl)
(eval-and-compile
 (push (getenv "BUILDDIR") load-path))
(require 'protocol)

(defun get-module-list ()
  (car (read-from-string (concat "(" (getenv "MODULES") ")"))))

(defun print-module-list (module-list)
  (insert "static const char *module_list[] = {")
  (let ((buf (current-buffer)))
    (terpri buf)
    (prin1 (symbol-name (first module-list)) buf)
    (loop for module-sym in (cdr module-list)
          do
          (insert ",")
          (terpri buf)
          (prin1 (symbol-name module-sym) buf))
    (terpri buf)
    (insert "};")
    (terpri buf)))

(defun print-module-lookup-function ()
  (let ((buf (current-buffer)))
    (insert "const char *")
    (terpri buf)
    (insert "swarm_lookup_module (const char *protocol_name)")
    (terpri buf)
    (insert "{")
    (terpri buf)
    (insert "  ")
    (insert "return module_list[in_word_set (protocol_name, strlen (protocol_name))->module_index];")
    (terpri buf)
    (insert "}")
    (terpri buf)))

(defun print-keywords (module-list)
  (loop for protocol in
        (sort
         (loop for protocol being each hash-value of *protocol-hash-table*
               collect protocol)
         #'(lambda (a b) (string< (protocol-name a)
                                  (protocol-name b))))
        do
        (insert (protocol-name protocol))
        (insert ",")
        (insert (format "%d" (position (module-sym
                                        (protocol-module protocol))
                                       module-list)))
        (terpri (current-buffer))))

(defun generate-module-map ()
  (load-and-process-modules :uniquify-method-lists t)
  (with-temp-file (concat (get-builddir) "modulemap")
    (let ((module-list (get-module-list))
          (buf (current-buffer)))
      (insert "%{")
      (terpri buf)
      (print-module-list module-list)
      (insert "%}")
      (terpri buf)
      (insert "struct protocol { const char *name; unsigned module_index; };")
      (terpri buf)
      (insert "%%")
      (terpri buf)
      (print-keywords module-list)
      (insert "%%")
      (terpri buf)
      (print-module-lookup-function)
      )))

                                         
                                         
    

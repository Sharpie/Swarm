;; Copyright © 1999, 2000 Swarm Development Group

(require 'cl)
(eval-and-compile
 (push (getenv "TOP_BUILDDIR") load-path))
(require 'protocol)

(defun get-module-list ()
  (car (read-from-string (concat "(" (getenv "MODULES") ")"))))

(defun print-module-list (module-list)
  (insert "static const char *module_list[] = {\n")
  (let ((buf (current-buffer)))
    (prin1 (symbol-name (first module-list)) buf)
    (loop for module-sym in (cdr module-list)
          do
          (insert ",\n")
          (prin1 (symbol-name module-sym) buf))
    (insert "\n};\n")))

(defun print-module-lookup-function ()
  (let ((buf (current-buffer)))
    (insert "const char *\n")
    (insert "swarm_lookup_module (const char *protocol_name)\n")
    (insert "{\n")
    (insert "  const struct protocol *protocol =\n")
    (insert "    in_word_set (protocol_name, strlen (protocol_name));\n")
    (insert "  int index = protocol ? protocol->module_index : -4;\n")
    (insert "  return index < -3 ? NULL : index < 0 ? \"\" : module_list[index];\n")
    (insert "}\n")))

(defun print-keywords (module-list)
  (loop for protocol in
        (sort
         (loop for protocol being each hash-value of *protocol-hash-table*
               collect protocol)
         #'(lambda (a b) (string< (protocol-name a)
                                  (protocol-name b))))
        for pos = (position (module-sym (protocol-module protocol))
                            module-list)
        when pos
        do
        (insert (protocol-name protocol))
        (insert ",")
        (insert (format "%d\n" pos))))

(defun generate-module-map ()
  (load-and-process-modules :uniquify-method-lists t)
  (with-temp-file (concat (get-builddir) "modulemap")
    (let ((module-list (get-module-list))
          (buf (current-buffer)))
      (insert "%{\n")
      (insert "#include <stddef.h>\n")
      (insert "#include <string.h>\n")
      (print-module-list module-list)
      (insert "%}\n")
      (insert "struct protocol { const char *name; int module_index; };\n")
      (insert "%%\n")
      (insert "Selector,-1\n")
      (insert "SwarmEnvironment,-2\n")
      (insert "Phase,-3\n")
      (print-keywords module-list)
      (insert "%%\n")
      (print-module-lookup-function)
      )))

(require 'cl)

(eval-and-compile
 (push (getenv "TOP_BUILDDIR") load-path))

(require 'protocol)

(defun module-package-string (module)
  (concat "SWARM." (module-name module) "."))

(defun module-path-string (module)
  (if (eq (module-sym module) 'swarm)
      ""
    (concat (module-name module) "/")))

(defun sgml-pathname-for-swarmdocs-pages-output (module-sym)
  (let ((module-name (symbol-name module-sym)))
    (concat (get-top-builddir)
            "refbook/"
            (module-path-string (lookup-module module-sym))
            module-name
            "pages.xml")))

(defun sgml-protocol-indexentry (protocol)
  (insert "<indexentry>\n")
  (insert "<primaryie linkends=\"")
  (insert (sgml-protocol-id protocol))
  (insert "\">")
  (insert (external-protocol-name protocol))
  (insert "</primaryie>\n")
  (insert "</indexentry>\n"))

(defun sgml-generate-protocol-index ()
  (insert "<index id=\"PROTOCOL.INDEX\">\n")
  (insert "<title>Protocol Index</title>\n")
  (loop for protocol in *protocol-list*
        unless (internal-protocol-p protocol)
        do (sgml-protocol-indexentry protocol))
  (insert "</index>\n"))

(defun sgml-method-signature-indexentry (method-signature)
  (insert "<indexentry>\n")
  (insert "<primaryie linkends=\"")
  (let ((protocol.method-list (gethash method-signature
                                       *method-signature-hash-table*))
        (space ""))
    (loop for protocol.method in protocol.method-list
          do
          (insert space)
          (insert (sgml-method-signature-id
                   (car protocol.method)
                   (method-phase (cdr protocol.method))
                   method-signature))
          (setq space " ")))
  (insert "\">")
  (insert method-signature)
  (insert "</primaryie>\n")
  (insert "</indexentry>\n"))

(defun sgml-generate-method-signature-index ()
  (insert "<index id=\"METHOD.INDEX\">\n")
  (insert "<title>Method Index</title>\n")
  (loop for method-signature in *method-signature-list*
        do (sgml-method-signature-indexentry method-signature))
  (insert "</index>\n"))

(defun sgml-indexentry (object)
  (insert "<indexentry>\n")
  (insert "<primaryie linkends=\"")
  (insert (sgml-id object))
  (insert "\">")
  (insert (generic-name object))
  (insert "</primaryie>\n")
  (insert "</indexentry>\n"))

(defun sgml-generate-index-of-type (type)
  (insert "<index id=\"")
  (insert (upcase (symbol-name type)))
  (insert ".INDEX\">\n")
  (insert "<title>")
  (insert (capitalize (symbol-name type)))
  (insert " Index")
  (insert "</title>\n")
  (loop for object in (sort (collect-objects-of-type type) #'name<)
        do (sgml-indexentry object))
  (insert "</index>\n"))

(defun sgml-generate-indices ()
  (with-temp-file (concat (get-top-builddir) "refbook/refindex.xml")
    (sgml-generate-protocol-index)
    (sgml-generate-method-signature-index)
    (loop for type in '(function global macro typedef)
          do (sgml-generate-index-of-type type))))

(defun sgml-object-id (type module protocol &optional name)
  (cook-id
   (let* ((type-str (upcase (symbol-name type)))
          (base-id
           (if protocol
               (let* ((cooked-protocol-name (external-protocol-name protocol))
                      (module (protocol-module protocol)))
                 (concat (insert (module-package-string module))
                         (upcase cooked-protocol-name)
                         "."
                         type-str))
               (concat (insert (module-package-string module))
                       "GENERIC."
                       type-str))))
     (if name
         (concat base-id "." (upcase name))
         base-id))))

(defun sgml-protocol-id (protocol)
  (sgml-object-id 'protocol
                  (protocol-module protocol)
                  protocol))

(defun sgml-method-signature-id (protocol phase method-signature)
  (sgml-object-id 'method
                  (protocol-module protocol)
                  protocol
                  (format "P%s.M%d" 
                          (case phase
                            (:creating "C")
                            (:setting "S")
                            ((:getters :using) "U")
                            (otherwise (error "bad phase")))
                          (method-signature-index method-signature))))

(defun sgml-module-id (module)
  (sgml-object-id 'module
                  module
                  nil))

(defun sgml-id (object)
  (sgml-object-id (object-type object)
                  (generic-module object)
                  (generic-protocol object)
                  (generic-name object)))

(defun sgml-refentry-start (obj)
  (insert "<refentry id=\"")
  (insert
   (cond ((module-p obj) (sgml-module-id obj))
         ((protocol-p obj) (sgml-protocol-id obj))
         (t (error "unknown object type"))))
  (insert "\">\n"))

(defun sgml-refmeta (object)
  (let (title module-name)
    (cond ((protocol-p object)
           (setq title (protocol-name object))
           (setq module-name (module-name (protocol-module object))))
          ((module-p object)
           (setq title "General")
           (setq module-name (module-name object)))
          (t (error "unknown object")))
    
    (insert "<refmeta>\n")
    (insert "<refentrytitle>")
    (insert title)
    (if (deprecated-p object)
        (insert " [Deprecated]\n")
      )
    (insert "</refentrytitle>\n")
    (insert "<refmiscinfo>")
    (insert module-name)
    (insert "</refmiscinfo>\n")
    (insert "</refmeta>\n")))


(defun sgml-namediv (object)
  (insert "<refnamediv>\n")
  (insert "<refname>")
  (insert (generic-name object))
  (insert "</refname>\n")
  (insert "<refpurpose>\n")
  (insert (generic-summary object))
  (insert "\n</refpurpose>\n")
  (insert "</refnamediv>\n"))

(defun sgml-refsect1-text-list (title text-list object)
  (when text-list
    (insert "<refsect1>\n")
    (insert "<title>")
    (insert-text title)
    (insert "</title>\n")
    (if (deprecated-p object)
        (progn
          (insert "<para><emphasis>Deprecated: \n")
          (loop for item in (generic-deprecated-list object)
                do
                (insert item))
          (insert "\n</emphasis></para>\n")
          ))
    (loop for text in text-list
          do 
          (insert "<para>\n")
          (insert-text text)
          (insert "\n</para>\n"))
    (insert "</refsect1>\n")))

(defun sgml-refsect1-description (object)
  (sgml-refsect1-text-list "Description" (generic-description-list object)
                           object))

(defun sgml-funcsynopsisinfo (class-name description-list)
  (insert "<funcsynopsisinfo>\n")
  (insert "<classname>")
  (insert class-name)
  (insert "</classname>\n")
  (loop for description in description-list
          do
          (insert-text description)
          (insert "\n"))
    (insert "</funcsynopsisinfo>\n"))

(defun sgml-method-funcsynopsis (owner-protocol method)
  (insert "<funcsynopsis id=\"")
  (insert (sgml-method-signature-id owner-protocol
                                    (method-phase method)
                                    (get-method-signature method)))
  (insert "\">\n")
  (insert "<funcprototype>\n")
  (insert "<funcdef>")
  (let ((return-type (method-return-type method)))
    (when return-type
      (insert-text return-type)))
  (insert "<function>")
  (print-method-signature method (current-buffer))
  (insert "</function>")
  (insert "</funcdef>\n")
  (let ((arguments (method-arguments method)))
    (if (and (eql (length arguments) 1)
             (null (third (first arguments))))
        (insert "<void/>\n")
        (loop for arg in arguments
              for type = (second arg)
              do
              (insert "<paramdef>")
              (when type
                (insert-text type))
              (insert "<parameter>")
              (insert-text (third arg))
              (insert "</parameter>")
              (insert "</paramdef>\n"))))
  (insert "</funcprototype>\n")
  (sgml-funcsynopsisinfo (protocol-name owner-protocol)
                         (method-description-list method))
  (insert "</funcsynopsis>\n"))

(defun sgml-link-to-protocol (protocol)
  (insert "<link linkend=\"")
  (insert (sgml-protocol-id protocol))
  (insert "\">")
  (insert (external-protocol-name protocol))
  (insert "</link>"))

(defun sgml-method-definitions (protocol
                                phase
                                &optional protocol-listitem-flag)
  (unless (zerop (count-included-methodinfo-entries protocol phase))
    (let ((methodinfo-list (methodinfo-list-for-phase protocol phase))
          have-list have-item)
      (when protocol-listitem-flag
        (insert "<itemizedlist>\n"))
      (loop with last-protocol = nil
            for methodinfo in methodinfo-list
            for level = (methodinfo-level methodinfo)
            for owner-protocol = (methodinfo-protocol methodinfo)
            for method = (methodinfo-method methodinfo)
            for new-group-flag = (not (eq owner-protocol last-protocol))
            
            when new-group-flag do
            (when have-list
              (insert "</itemizedlist>\n")
              (setq have-list nil))
            (when have-item
                (insert "</listitem>\n")
                (setq have-item nil))
            (when protocol-listitem-flag
              (when (include-p level protocol owner-protocol)
                (insert "<listitem>\n")
                (setq have-item t)
                (insert "<para>")
                (insert (external-protocol-name owner-protocol))
                (insert "</para>\n")))
            (when (include-p level protocol owner-protocol)
              (setq have-list t)
              (insert "<itemizedlist>\n"))
              
            do
            (when (include-p level protocol owner-protocol)
              (insert "<listitem>\n")
              (sgml-method-funcsynopsis owner-protocol method)
              (sgml-method-examples owner-protocol method)
              (insert "</listitem>\n"))
            
            for last-protocol = owner-protocol)
      (when have-list
        (insert "</itemizedlist>\n"))
      (when protocol-listitem-flag
        (when have-item
          (insert "</listitem>\n"))
        (insert "</itemizedlist>\n")))))

(defun sgml-macro (macro)
  (if (eq :no-arguments (macro-arguments macro))
      (progn
        (insert "<para id=\"")
        (insert (sgml-id macro))
        (insert "\">\n")
        (insert-text (macro-name macro))
        (insert "\n</para>\n")
        (loop for text in (macro-description-list macro)
              do 
              (insert "<para>\n")
              (insert-text text)
              (insert "</para>\n")))
      (progn
        (insert "<funcsynopsis id=\"")
        (insert (sgml-id macro))
        (insert "\">\n")
        (insert "<funcprototype>\n")
        (insert "<funcdef>")
        (insert "<function>")
        (insert-text (macro-name macro))
        (insert "</function>")
        (insert "</funcdef>\n")
        (loop for arg in (macro-arguments macro)
              do
              (when arg
                (insert "<paramdef>")
                (insert "<parameter>")
                (insert arg)
                (insert "</parameter>")
                (insert "</paramdef>\n")))
        (insert "</funcprototype>\n")
        (sgml-funcsynopsisinfo "(MACRO)"
                               (macro-description-list macro))
        (insert "</funcsynopsis>\n"))))
      
(defun sgml-function (function)
  (insert "<funcsynopsis id=\"")
  (insert (sgml-id function))
  (insert "\">\n")
  (insert "<funcprototype>\n")
  (insert "<funcdef>")
  (insert-text (function-return-type function))
  (insert "<function>")
  (insert-text (function-name function))
  (insert "</function>")
  (insert "</funcdef>\n")
  (loop for type.name in (function-arguments function)
        do
        (insert "<paramdef>")
        (insert-text (car type.name))
        (insert "<parameter>")
        (let ((name (cdr type.name)))
          (when name
          (insert-text name)))
        (insert "</parameter>")
        (insert "</paramdef>\n"))
  (insert "</funcprototype>\n")
  (sgml-funcsynopsisinfo "(FUNCTION)"
                         (function-description-list function))
  (insert "</funcsynopsis>\n"))

(defun sgml-typedef (typedef)
  (insert "<para id=\"")
  (insert (sgml-id typedef))
  (insert "\">\n")
  (insert (typedef-name typedef))
  (insert "\n<type>\n")
  (insert (typedef-type typedef))
  (insert "</type>\n")
  (insert "</para>\n"))

(defun sgml-refsect1-object-list (title
                                  object-list
                                  print-object-func)
  (when object-list
    (insert "<refsect1>\n")
    (insert "<title>")
    (insert-text title)
    (insert "</title>\n")
    (insert "<itemizedlist>\n")
    (loop for object in (sort object-list #'name<)
          do
          (insert "<listitem>\n")
          (funcall print-object-func object)
          (insert "</listitem>\n"))
    (insert "</itemizedlist>\n")
    (insert "</refsect1>\n")))

(defun sgml-refsect1-macro-list (object)
  (sgml-refsect1-object-list "Macros"
                             (generic-macro-list object)
                             #'sgml-macro))

(defun sgml-refsect1-typedef-list (object)
  (sgml-refsect1-object-list "Typedefs"
                             (generic-typedef-list object)
                             #'sgml-typedef))

(defun sgml-refsect1-function-list (object)
  (sgml-refsect1-object-list "Functions"
                             (generic-function-list object)
                             #'sgml-function))

(defun sgml-refsect1-global-list (object)
  (let ((global-list (generic-global-list object)))
    (when global-list
      (insert "<refsect1>\n")
      (insert "<title>")
      (insert "Globals")
      (insert "</title>\n")
      (insert "<variablelist>\n")
      (loop for global in global-list
            do
            (insert "<varlistentry id=\"")
            (insert (sgml-id global))
            (insert "\">\n")
            (insert "<term>")
            (insert-text (global-type global))
            (insert "</term>\n")
            (insert "<term>")
            (insert-text (global-name global))
            (insert "</term>\n")
            (let ((description-list (global-description-list global)))
              (if description-list
                  (progn
                    (insert "<listitem>\n")
                    (loop for text in description-list
                          do 
                          (insert "<para>\n")
                          (insert-text text)
                          (insert "</para>\n"))
                    (insert "</listitem>\n"))
                  (insert "<listitem><para>No description available.</para></listitem>\n")))
            (insert "</varlistentry>\n"))
      (insert "</variablelist>\n")
      (insert "</refsect1>\n"))))

(defun sgml-examples (object)
  (let ((example-list (protocol-example-list object)))
    (when example-list
      (insert "<example label=\"")
      (insert (module-path-string (protocol-module protocol)))
      (insert (external-protocol-name protocol))
      (insert (format "/%d" (general-example-counter protocol)))

      ; start ID tag attribute                                        
      (insert "\" id=\"")
      (insert (module-package-string (protocol-module protocol)))
      (insert (external-protocol-name protocol))
      (insert ".GENERIC")
      (insert (format ".%d" (general-example-counter protocol)))
      (insert ".EXAMPLE")
      (insert "\">")

      (insert "<title>\n")
      (insert "</title>\n")
      (loop for example in example-list
            do
            (insert "<programlisting>\n<![ CDATA [\n")
            (insert example)
            (insert "]]>\n</programlisting>\n"))
      (insert "</example>\n"))))

(defun sgml-method-examples (protocol method)
  (when (method-example-list method)
    (insert "<example label=\"")
    (insert (module-path-string (protocol-module protocol)))
    (insert (external-protocol-name protocol))
    (insert "/")
    (print-method-signature method (current-buffer))
    (insert (format "/%d" (method-example-counter protocol method)))

    ; start ID tag attribute                                        
    (insert "\" id=\"")
    (insert (module-package-string (protocol-module protocol)))
    (insert (external-protocol-name protocol))
    (insert ".")
    (print-method-signature method (current-buffer) t)
    (insert (format ".%d" (method-example-counter protocol method)))
    (insert ".EXAMPLE")

    (insert "\">")
    (insert "<title>")
    (insert "</title>\n")

    (loop for example in (method-example-list method)
          do
          (insert "<programlisting>\n<![ CDATA [\n")
          (insert example)
          (insert "]]>\n</programlisting>\n"))
    (insert "</example>\n")))

(defun sgml-methods-for-phase (protocol phase)
  (unless (zerop (count-included-methodinfo-entries protocol phase))
    (insert "<refsect2>\n")
    (insert "<title>Phase: ")
    (insert (capitalize (substring (prin1-to-string phase) 1)))
    (insert "</title>\n")
    (sgml-method-definitions protocol phase)
    (insert "</refsect2>\n")))

(defun sgml-refsect1-protocol-list (protocol &optional expand-flag)
  (insert "<refsect1>\n")
  (insert "<title>Protocols adopted by ")
  (insert (protocol-name protocol))
  (insert "</title>\n")
  (if (zerop (count-noninternal-protocols protocol))
      (insert "<para>None</para>\n")
      (flet ((print-expanded-protocol-list (protocol)
               (insert "<itemizedlist>\n")
               (loop for included-protocol in
                     (protocol-included-protocol-list protocol)
                     do
                     (unless (internal-protocol-p protocol)
                       (insert "<listitem>\n")
                       (insert "<para>")
                       (sgml-link-to-protocol included-protocol)
                       (insert "</para>\n")
                       (print-expanded-protocol-list included-protocol)
                       (insert "</listitem>\n")))
               (insert "</itemizedlist>\n"))
             (print-unexpanded-protocol-list (protocol)
               (insert "<para>")
               (loop for included-protocol in
                     (protocol-included-protocol-list protocol)
                     do
                     (unless (internal-protocol-p protocol)
                       (insert " ")
                       (sgml-link-to-protocol included-protocol)))
               (insert "</para>\n")))
        (if expand-flag
            (print-expanded-protocol-list protocol)
            (print-unexpanded-protocol-list protocol))))
  (insert "</refsect1>\n"))

(defun sgml-refsect1-method-list (protocol)
  (insert "<refsect1><title>Methods</title>\n")
  (if (zerop (count-included-methodinfo-entries-for-all-phases protocol))
      (insert "<para>None</para>\n")
      (loop for phase in *phases*
            do (sgml-methods-for-phase protocol phase)))
  (insert "</refsect1>\n"))

(defun sgml-refsect1-examples (protocol)
  (when (protocol-example-list protocol)
    (insert "<refsect1><title>Examples</title>\n")
    (sgml-examples protocol)
    (insert "</refsect1>\n")))

(defun generate-refentry (object)
  (unless (and (protocol-p object) (internal-protocol-p object))
    (sgml-refentry-start object)
    (sgml-refmeta object)
    (sgml-namediv object)
    (sgml-refsect1-description object)

    (when (protocol-p object)
      (sgml-refsect1-protocol-list object)
      (sgml-refsect1-method-list object))
    
    (sgml-refsect1-macro-list object)
    (sgml-refsect1-function-list object)
    (sgml-refsect1-typedef-list object)
    (sgml-refsect1-global-list object)
    
    (when (protocol-p object)
      (sgml-refsect1-examples object))
    
    (insert "</refentry>\n")))

(defun sgml-generate-refentries-for-module (module-sym)
  (loop for object in (sort (gethash module-sym *module-hash-table*)
                            #'name<)
        do (generate-refentry object)))
  
(defun sgml-create-refentries-for-module (module-sym)
  (with-temp-file (sgml-pathname-for-swarmdocs-pages-output module-sym)
    (sgml-generate-refentries-for-module module-sym)))

(defun sgml-create-refentries-for-all-modules ()
  (interactive)
  (loop for module-sym being each hash-key of *module-hash-table*
        do
        (sgml-create-refentries-for-module module-sym)
        ))

(defun run-all ()
  (interactive)
  (load-and-process-modules :uniquify-method-lists nil)
  (sgml-create-refentries-for-all-modules) 
  (sgml-generate-indices)
  nil)


(require 'cl)

(eval-and-compile
 (push (getenv "TOP_BUILDDIR") load-path))

(require 'protocol)

(defun sgml-protocol-indexentry (protocol)
  (insert "<INDEXENTRY>\n")
  (insert "<PRIMARYIE LINKENDS=\"")
  (insert (sgml-protocol-id protocol))
  (insert "\">")
  (insert (external-protocol-name protocol))
  (insert "</PRIMARYIE>\n")
  (insert "</INDEXENTRY>\n"))

(defun sgml-generate-protocol-index ()
  (insert "<INDEX ID=\"PROTOCOL.INDEX\">\n")
  (insert "<TITLE>Protocol Index</TITLE>\n")
  (loop for protocol in *protocol-list*
        unless (internal-protocol-p protocol)
        do (sgml-protocol-indexentry protocol))
  (insert "</INDEX>\n"))

(defun sgml-method-signature-indexentry (method-signature)
  (insert "<INDEXENTRY>\n")
  (insert "<PRIMARYIE LINKENDS=\"")
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
  (insert "</PRIMARYIE>\n")
  (insert "</INDEXENTRY>\n"))

(defun sgml-generate-method-signature-index ()
  (insert "<INDEX ID=\"METHOD.INDEX\">\n")
  (insert "<TITLE>Method Index</TITLE>\n")
  (loop for method-signature in *method-signature-list*
        do (sgml-method-signature-indexentry method-signature))
  (insert "</INDEX>\n"))

(defun sgml-indexentry (object)
  (insert "<INDEXENTRY>\n")
  (insert "<PRIMARYIE LINKENDS=\"")
  (insert (sgml-id object))
  (insert "\">")
  (insert (generic-name object))
  (insert "</PRIMARYIE>\n")
  (insert "</INDEXENTRY>\n"))

(defun sgml-generate-index-of-type (type)
  (insert "<INDEX ID=\"")
  (insert (upcase (symbol-name type)))
  (insert ".INDEX\">\n")
  (insert "<TITLE>")
  (insert (capitalize (symbol-name type)))
  (insert " Index")
  (insert "</TITLE>\n")
  (loop for object in (sort (collect-objects-of-type type) #'name<)
        do (sgml-indexentry object))
  (insert "</INDEX>\n"))

(defun sgml-generate-indices ()
  (with-temp-file (concat (get-top-builddir) "refbook/refindex.sgml")
    (sgml-generate-protocol-index)
    (sgml-generate-method-signature-index)
    (loop for type in '(function global macro typedef)
          do (sgml-generate-index-of-type type))))

(defun sgml-object-id (type module protocol &optional name)
  (cook-id
   (let* ((type-str (upcase (symbol-name type)))
          (base-id
           (if protocol
               (let* ((cooked-protocol-name (external-protocol-name protocol)))
                 (concat "SWARM."
                         (upcase (module-name (protocol-module protocol)))
                         "."
                         (upcase cooked-protocol-name)
                         "."
                         type-str))
               (concat "SWARM."
                       (upcase (module-name module))
                       ".GENERIC."
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
                            (:using "U")
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
  (insert "<REFENTRY ID=\"")
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
    
    (insert "<REFMETA>\n")
    (insert "<REFENTRYTITLE>")
    (insert title)
    (if (deprecated-p object)
        (insert " [Deprecated]\n")
      )
    (insert "</REFENTRYTITLE>\n")
    (insert "<REFMISCINFO>")
    (insert module-name)
    (insert "</REFMISCINFO>\n")
    (insert "</REFMETA>\n")))


(defun sgml-namediv (object)
  (insert "<REFNAMEDIV>\n")
  (insert "<REFNAME>")
  (insert (generic-name object))
  (insert "</REFNAME>\n")
  (insert "<REFPURPOSE>\n")
  (insert (generic-summary object))
  (insert "\n</REFPURPOSE>\n")
  (insert "</REFNAMEDIV>\n"))

(defun sgml-refsect1-text-list (title text-list object)
  (when text-list
    (insert "<REFSECT1>\n")
    (insert "<TITLE>")
    (insert-text title)
    (insert "</TITLE>\n")
    (if (deprecated-p object)
        (progn
          (insert "<PARA><EMPHASIS>Deprecated: \n")
          (loop for item in (generic-deprecated-list object)
                do
                (insert item))
          (insert "\n</EMPHASIS></PARA>\n")
          ))
    (loop for text in text-list
          do 
          (insert "<PARA>\n")
          (insert-text text)
          (insert "\n</PARA>\n"))
    (insert "</REFSECT1>\n")))

(defun sgml-refsect1-description (object)
  (sgml-refsect1-text-list "Description" (generic-description-list object)
                           object))

(defun sgml-funcsynopsisinfo (class-name description-list)
  (insert "<FUNCSYNOPSISINFO>\n")
  (insert "<CLASSNAME>")
  (insert class-name)
  (insert "</CLASSNAME>\n")
  (loop for description in description-list
          do
          (insert-text description)
          (insert "\n"))
    (insert "</FUNCSYNOPSISINFO>\n"))

(defun sgml-method-funcsynopsis (owner-protocol method)
  (insert "<FUNCSYNOPSIS ID=\"")
  (insert (sgml-method-signature-id owner-protocol
                                    (method-phase method)
                                    (get-method-signature method)))
  (insert "\">\n")
  (insert "<FUNCPROTOTYPE>\n")
  (insert "<FUNCDEF>")
  (let ((return-type (method-return-type method)))
    (when return-type
      (insert-text return-type)))
  (insert "<FUNCTION>")
  (print-method-signature method (current-buffer))
  (insert "</FUNCTION>")
  (insert "</FUNCDEF>\n")
  (let ((arguments (method-arguments method)))
    (if (and (eql (length arguments) 1)
             (null (third (first arguments))))
        (insert "<VOID>\n")
        (loop for arg in arguments
              for type = (second arg)
              do
              (insert "<PARAMDEF>")
              (when type
                (insert-text type))
              (insert "<PARAMETER>")
              (insert-text (third arg))
              (insert "</PARAMETER>")
              (insert "</PARAMDEF>\n"))))
  (insert "</FUNCPROTOTYPE>\n")
  (sgml-funcsynopsisinfo (protocol-name owner-protocol)
                         (method-description-list method))
  (insert "</FUNCSYNOPSIS>\n"))

(defun sgml-link-to-protocol (protocol)
  (insert "<LINK LINKEND=\"")
  (insert (sgml-protocol-id protocol))
  (insert "\">")
  (insert (external-protocol-name protocol))
  (insert "</LINK>"))

(defun sgml-method-definitions (protocol
                                phase
                                &optional protocol-listitem-flag)
  (unless (zerop (count-included-methodinfo-entries protocol phase))
    (let ((methodinfo-list (methodinfo-list-for-phase protocol phase))
          have-list have-item)
      (when protocol-listitem-flag
        (insert "<ITEMIZEDLIST>\n"))
      (loop with last-protocol = nil
            for methodinfo in methodinfo-list
            for level = (first methodinfo)
            for owner-protocol = (second methodinfo)
            for method = (third methodinfo)
            for new-group-flag = (not (eq owner-protocol last-protocol))
            
            when new-group-flag do
            (when have-list
              (insert "</ITEMIZEDLIST>\n")
              (setq have-list nil))
            (when have-item
                (insert "</LISTITEM>\n")
                (setq have-item nil))
            (when protocol-listitem-flag
              (when (include-p level protocol owner-protocol)
                (insert "<LISTITEM>\n")
                (setq have-item t)
                (insert "<PARA>")
                (insert (external-protocol-name owner-protocol))
                (insert "</PARA>\n")))
            (when (include-p level protocol owner-protocol)
              (setq have-list t)
              (insert "<ITEMIZEDLIST>\n"))
              
            do
            (when (include-p level protocol owner-protocol)
              (insert "<LISTITEM>\n")
              (sgml-method-funcsynopsis owner-protocol method)
              (sgml-method-examples owner-protocol method)
              (insert "</LISTITEM>\n"))
            
            for last-protocol = owner-protocol)
      (when have-list
        (insert "</ITEMIZEDLIST>\n"))
      (when protocol-listitem-flag
        (when have-item
          (insert "</LISTITEM>\n"))
        (insert "</ITEMIZEDLIST>\n")))))

(defun sgml-macro (macro)
  (if (eq :no-arguments (macro-arguments macro))
      (progn
        (insert "<PARA ID=\"")
        (insert (sgml-id macro))
        (insert "\">\n")
        (insert-text (macro-name macro))
        (insert "\n</PARA>\n")
        (loop for text in (macro-description-list macro)
              do 
              (insert "<PARA>\n")
              (insert-text text)
              (insert "</PARA>\n")))
      (progn
        (insert "<FUNCSYNOPSIS ID=\"")
        (insert (sgml-id macro))
        (insert "\">\n")
        (insert "<FUNCPROTOTYPE>\n")
        (insert "<FUNCDEF>")
        (insert "<FUNCTION>")
        (insert-text (macro-name macro))
        (insert "</FUNCTION>")
        (insert "</FUNCDEF>\n")
        (loop for arg in (macro-arguments macro)
              do
              (when arg
                (insert "<PARAMDEF>")
                (insert "<PARAMETER>")
                (insert arg)
                (insert "</PARAMETER>")
                (insert "</PARAMDEF>\n")))
        (insert "</FUNCPROTOTYPE>\n")
        (sgml-funcsynopsisinfo "(MACRO)"
                               (macro-description-list macro))
        (insert "</FUNCSYNOPSIS>\n"))))
      
(defun sgml-function (function)
  (insert "<FUNCSYNOPSIS ID=\"")
  (insert (sgml-id function))
  (insert "\">\n")
  (insert "<FUNCPROTOTYPE>\n")
  (insert "<FUNCDEF>")
  (insert-text (function-return-type function))
  (insert "<FUNCTION>")
  (insert-text (function-name function))
  (insert "</FUNCTION>")
  (insert "</FUNCDEF>\n")
  (loop for type.name in (function-arguments function)
        do
        (insert "<PARAMDEF>")
        (insert-text (car type.name))
        (insert "<PARAMETER>")
        (let ((name (cdr type.name)))
          (when name
          (insert-text name)))
        (insert "</PARAMETER>")
        (insert "</PARAMDEF>\n"))
  (insert "</FUNCPROTOTYPE>\n")
  (sgml-funcsynopsisinfo "(FUNCTION)"
                         (function-description-list function))
  (insert "</FUNCSYNOPSIS>\n"))

(defun sgml-typedef (typedef)
  (insert "<PARA ID=\"")
  (insert (sgml-id typedef))
  (insert "\">\n")
  (insert (typedef-name typedef))
  (insert "\n<TYPE>\n")
  (insert (typedef-type typedef))
  (insert "</TYPE>\n")
  (insert "</PARA>\n"))

(defun sgml-refsect1-object-list (title
                                  object-list
                                  print-object-func)
  (when object-list
    (insert "<REFSECT1>\n")
    (insert "<TITLE>")
    (insert-text title)
    (insert "</TITLE>\n")
    (insert "<ITEMIZEDLIST>\n")
    (loop for object in (sort object-list #'name<)
          do
          (insert "<LISTITEM>\n")
          (funcall print-object-func object)
          (insert "</LISTITEM>\n"))
    (insert "</ITEMIZEDLIST>\n")
    (insert "</REFSECT1>\n")))

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
      (insert "<REFSECT1>\n")
      (insert "<TITLE>")
      (insert "Globals")
      (insert "</TITLE>\n")
      (insert "<VARIABLELIST>\n")
      (loop for global in global-list
            do
            (insert "<VARLISTENTRY ID=\"")
            (insert (sgml-id global))
            (insert "\">\n")
            (insert "<TERM>")
            (insert-text (global-type global))
            (insert "</TERM>\n")
            (insert "<TERM>")
            (insert-text (global-name global))
            (insert "</TERM>\n")
            (let ((description-list (global-description-list global)))
              (if description-list
                  (progn
                    (insert "<LISTITEM>\n")
                    (loop for text in description-list
                          do 
                          (insert "<PARA>\n")
                          (insert-text text)
                          (insert "</PARA>\n"))
                    (insert "</LISTITEM>\n"))
                  (insert "<LISTITEM><PARA>No description available.</PARA></LISTITEM>\n")))
            (insert "</VARLISTENTRY>\n"))
      (insert "</VARIABLELIST>\n")
      (insert "</REFSECT1>\n"))))

(defun sgml-examples (object)
  (let ((example-list (protocol-example-list object)))
    (when example-list
      (insert "<EXAMPLE LABEL=\"")
      (insert (module-name (protocol-module protocol)))
      (insert "/")
      (insert (external-protocol-name protocol))
      (insert (format "/%d" (general-example-counter protocol)))

      ; start ID tag attribute                                        
      (insert "\" ID=\"SWARM.")
      (insert (module-name (protocol-module protocol)))
      (insert ".")
      (insert (external-protocol-name protocol))
      (insert ".GENERIC")
      (insert (format ".%d" (general-example-counter protocol)))
      (insert ".EXAMPLE")
      (insert "\">")

      (insert "<TITLE>\n")
      (insert "</TITLE>\n")
      (loop for example in example-list
            do
            (insert "<PROGRAMLISTING>\n<![ CDATA [\n")
            (insert example)
            (insert "]]>\n</PROGRAMLISTING>\n"))
      (insert "</EXAMPLE>\n"))))

(defun sgml-method-examples (protocol method)
  (when (method-example-list method)
    (insert "<EXAMPLE LABEL=\"")
    (insert (module-name (protocol-module protocol)))
    (insert "/")
    (insert (external-protocol-name protocol))
    (insert "/")
    (print-method-signature method (current-buffer))
    (insert (format "/%d" (method-example-counter protocol method)))

    ; start ID tag attribute                                        
    (insert "\" ID=\"SWARM.")
    (insert (module-name (protocol-module protocol)))
    (insert ".")
    (insert (external-protocol-name protocol))
    (insert ".")
    (print-method-signature method (current-buffer) t)
    (insert (format ".%d" (method-example-counter protocol method)))
    (insert ".EXAMPLE")

    (insert "\">")
    (insert "<TITLE>")
    (insert "</TITLE>\n")

    (loop for example in (method-example-list method)
          do
          (insert "<PROGRAMLISTING>\n<![ CDATA [\n")
          (insert example)
          (insert "]]>\n</PROGRAMLISTING>\n"))
    (insert "</EXAMPLE>\n")))

(defun sgml-methods-for-phase (protocol phase)
  (unless (zerop (count-included-methodinfo-entries protocol phase))
    (insert "<REFSECT2>\n")
    (insert "<TITLE>Phase: ")
    (insert (capitalize (substring (prin1-to-string phase) 1)))
    (insert "</TITLE>\n")
    (sgml-method-definitions protocol phase)
    (insert "</REFSECT2>\n")))

(defun sgml-refsect1-protocol-list (protocol &optional expand-flag)
  (insert "<REFSECT1>\n")
  (insert "<TITLE>Protocols adopted by ")
  (insert (protocol-name protocol))
  (insert "</TITLE>\n")
  (if (zerop (count-noninternal-protocols protocol))
      (insert "<PARA>None</PARA>\n")
      (flet ((print-expanded-protocol-list (protocol)
               (insert "<ITEMIZEDLIST>\n")
               (loop for included-protocol in
                     (protocol-included-protocol-list protocol)
                     do
                     (unless (internal-protocol-p protocol)
                       (insert "<LISTITEM>\n")
                       (insert "<PARA>")
                       (sgml-link-to-protocol included-protocol)
                       (insert "</PARA>\n")
                       (print-expanded-protocol-list included-protocol)
                       (insert "</LISTITEM>\n")))
               (insert "</ITEMIZEDLIST>\n"))
             (print-unexpanded-protocol-list (protocol)
               (insert "<PARA>")
               (loop for included-protocol in
                     (protocol-included-protocol-list protocol)
                     do
                     (unless (internal-protocol-p protocol)
                       (insert " ")
                       (sgml-link-to-protocol included-protocol)))
               (insert "</PARA>\n")))
        (if expand-flag
            (print-expanded-protocol-list protocol)
            (print-unexpanded-protocol-list protocol))))
  (insert "</REFSECT1>\n"))

(defun sgml-refsect1-method-list (protocol)
  (insert "<REFSECT1><TITLE>Methods</TITLE>\n")
  (if (zerop (count-included-methodinfo-entries-for-all-phases protocol))
      (insert "<PARA>None</PARA>\n")
      (loop for phase in *phases*
            do (sgml-methods-for-phase protocol phase)))
  (insert "</REFSECT1>\n"))

(defun sgml-refsect1-examples (protocol)
  (when (protocol-example-list protocol)
    (insert "<REFSECT1><TITLE>Examples</TITLE>\n")
    (sgml-examples protocol)
    (insert "</REFSECT1>\n")))

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
    
    (insert "</REFENTRY>\n")))

(defun sgml-generate-refentries-for-module (module-sym)
  (loop for object in (sort (gethash module-sym *module-hash-table*)
                            #'name<)
        do (generate-refentry object)))
  
(defun sgml-create-refentries-for-module (module-sym)
  (let ((module-name (symbol-name module-sym)))
    (with-temp-file (pathname-for-swarmdocs-pages-output module-sym)
      (sgml-generate-refentries-for-module module-sym))))

(defun sgml-create-refentries-for-all-modules ()
  (interactive)
  (loop for module-sym being each hash-key of *module-hash-table*
        do
        (sgml-create-refentries-for-module module-sym)))

(defun run-all ()
  (interactive)
  (load-and-process-modules :uniquify-method-lists nil)
  (sgml-create-refentries-for-all-modules)  (sgml-generate-indices)
  nil)


(require 'cl)
(eval-and-compile
  (push (getenv "TOP_BUILDDIR") load-path))
(require 'protocol)
(require 'interface)

(defconst *com-interface-prefix* "swarmI")
(defconst *com-impl-prefix* "swarm")

(defconst *com-objc-to-idl-type-alist*
    '(("id .*" . "nsISupports")
      ;("SEL" . "swarmISelector")
      ("SEL" . "nsISupports")
      ("void" . "void")
      ("const char \\*" . "string")
      
      ("char \\*" . "string")
      ("char" . "char")
      ("unsigned char" . "octet")
      ("int" . "long")
      ("short" . "short")
      ("unsigned short" . "short")
      ("double" . "double")
      ("float" . "float")
      ("unsigned" . "unsigned long")
      ("BOOL" . "boolean")

      ("long" . "long")
      ("unsigned long" . "unsigned long")
      ("timeval_t" . "long")
      ("size_t" . "long")

      ("Class" . "nsISupports"); XXX

      ("unsigned long long int" . "long")
      ("unsigned long long" . "long")
      ("long long" . "long")

      ("JOBJECT" . freaky)

      ("Color" . "octet")

      ("compare_t" . freaky)

      ("void \\*" . freaky)
      ("ref_t" . freaky)
      ("val_t" . freaky)
      ("Protocol \\*" . freaky)

      ("notify_t" . freaky)
      
      ("const char \\* const \\*" . freaky)
      ("int \\*" . freaky)
      ("double \\*" . freaky)
      ("BOOL \\*" . freaky)
      ("Class" . freaky)
      ("FILE \\*" . freaky)
      ("func_t" . freaky)
      ("unsigned \\*" . freaky)
      ("IMP" . freaky)
      ("const char \\*\\*" . freaky)
      ("int (\\*) (int, const char \\*)" . freaky)
      ("struct argp_option \\*" . freaky)
      ("PixelValue" . freaky)
      ("PixelValue \\*" . freaky)
      ("long \\*" . freaky)
      ("const void \\*" . freaky)
      ("int(\\*)(const void\\*,const void\\*)" . freaky)
      ("int (\\*) (id hdf5Obj)" . freaky)
      ("int (\\*) (const char \\*key, const char \\*value)" . freaky)
      ("long double" . freaky)
      ("unsigned long \\*" . freaky)
      ("float \\*" . freaky)
      ("void (\\*) (unsigned rank, unsigned \\*vec, double val)" . freaky)
      ("void (\\*) (unsigned rank, unsigned \\*vec, int val)" . freaky)
      ("ProbeMap \\*" . freaky) 
      ))

(defconst *com-idl-to-c++-type-alist*
  '(("void" . "void")
    ("boolean" . "PRBool")
    ("octet" . "PRUint8")
    ("short" . "PRInt16")
    ("long" . "PRInt32")
    ("long long" . "PRInt64")
    ("unsigned short" . "PRUint16")
    ("unsigned long" . "PRUint32")
    ("unsigned long long" . "PRUint64")
    ("float" . "float")
    ("double" . "double")
    ("char" . "char")
    ("wchar" . "PRUnichar")
    ("string" . "const char*")
    ("wstring" . "PRUnichar*")))

(defvar *com-uuid-hash-table* (make-hash-table :test #'equal))

(defun com-phase-name (protocol phase)
  (concat (protocol-name protocol) (suffix-for-phase phase)))

(defun com-interface-name (protocol phase)
  (concat *com-interface-prefix* (com-phase-name protocol phase)))

(defun com-uuid-table-pathname ()
  (concat (get-swarmsrcdir) "COM/uuids.el"))

(defun com-new-uuid-table-pathname ()
  (concat (get-builddir) "new-uuids.el"))

(defun com-load-uuid-table (list)
  (loop for pair in list
        do
        (setf (gethash (car pair) *com-uuid-hash-table*) (cdr pair))))

(defun com-save-uuid-table ()
  (with-temp-file (com-new-uuid-table-pathname)
    (insert "(com-load-uuid-table '(\n");
    (loop for name in
          (sort 
           (loop for name being each hash-key of
                 *com-uuid-hash-table*
                 collect name)
           #'string<)
          for uuid = (gethash name *com-uuid-hash-table*)
          do
          (insert "  ")
          (insert (prin1-to-string (cons name uuid)))
          (insert "\n"))
    (insert "))")))
                        
  
(defun com-ensure-uuid (name)
  (let ((uuid (gethash name *com-uuid-hash-table*)))
    (unless uuid
      (message (concat "Generating uuid for `" name "'"))
      (setq uuid
            (with-temp-buffer
              (call-process "uuidgen" nil t nil)
              (goto-char (point-min))
              (end-of-line)
              (buffer-substring (point-min) (point))))
      (setf (gethash name *com-uuid-hash-table*) uuid))
    uuid))

(defun com-type-category-to-com-type (objc-type com-type-category)
  (cond ((eq com-type-category 'freaky)
         (freaky-message objc-type)
         com-type-category)
        ((let ((protocol (objc-protocol-for-type objc-type)))
           (when protocol
             (com-interface-name protocol :using))))
        (t com-type-category)))

(defun com-objc-to-idl-type-category (objc-type)
  (if objc-type
      (if (objc-protocol-for-type objc-type)
          'protocol
        (cdr (find objc-type *com-objc-to-idl-type-alist*
                   :key #'car
                   :test #'regexp-match-p)))
    "nsISupports"))

(defun com-objc-to-idl-type (objc-type)
  (com-type-category-to-com-type objc-type
                                 (com-objc-to-idl-type-category objc-type)))

(defun com-idl-method-name (arguments)
  (with-output-to-string
    (princ (car (first arguments)))
    (loop for argument in (cdr arguments)
          for nameKey = (car argument)
          do
          (if nameKey
              (progn
                (princ "_")
                (princ nameKey))
            (princ "X")))))

(defun com-c++-method-name (arguments)
  (let ((idl-name (com-idl-method-name arguments)))
    (concat (upcase (substring idl-name 0 1))
            (substring idl-name 1))))

(defun com-print-argument (argument convert-type-func &optional prefix)
  (let* ((type-and-varname (cdr argument))
         (varname (cadr type-and-varname)))
    ;; the case of method with no arguments
    (when varname
      (when prefix (insert prefix))
      (insert (funcall convert-type-func (car type-and-varname)))
      (insert " ")
      (insert (cond ((string= varname "context") "context_")
                    ((string= varname "class") "class_")
                    (t varname)))
      t)))

(defun com-swarm-type-p (type)
  (when (stringp type)
    (string-match (concat "^" *com-interface-prefix*) type)))

(defun com-idl-type (objc-type)
  (let ((idl-type (com-objc-to-idl-type objc-type)))
    (unless idl-type
      (error "No IDL type for `%s'" objc-type))
    (if (eq idl-type 'freaky)
        (progn
          (freaky-message objc-type)
          "nsISupports")
        idl-type)))

(defun com-idl-print-method-declaration (protocol method)
  (let* ((arguments (method-arguments method))
         (first-argument (car arguments)))
    (insert "  ")
    (insert (com-idl-type (method-return-type method)))
    (insert " ")
    (insert (com-idl-method-name arguments))
    (insert " (")
    (com-print-argument first-argument #'com-idl-type "in ")
    (loop for argument in (cdr arguments)
          do
          (insert ", ")
          (com-print-argument argument #'com-idl-type "in "))
    (insert ");\n")))

(defun com-idl-print-include (idl-type)
  (insert "#include \"")
  (insert idl-type)
  (insert ".idl\"\n"))

(defun com-idl-print-includes (protocol phase)
  (let ((ht (create-type-hash-table protocol phase)))
    (insert "\n")
    (loop for objc-type being each hash-key of ht
          do
          (insert "interface ")
          (insert (com-objc-to-idl-type objc-type))
          (insert ";\n"))
    (insert "\n")
    (loop for objc-type being each hash-key of ht
          do (com-idl-print-include (com-objc-to-idl-type objc-type)))))

(defun com-idl-print-methods-in-phase (protocol phase)
  (loop for method in (protocol-method-list protocol)
        when (included-method-p protocol method phase)
	do
        (com-idl-print-method-declaration protocol method)))

(defun com-start-idl (protocol phase)
  (let* ((interface-name (com-interface-name protocol phase))
         (uuid (gethash interface-name *com-uuid-hash-table*)))
    (insert "#include \"nsISupports.idl\"\n")
    (com-idl-print-includes protocol phase)
    (insert "\n")
    (insert "[scriptable, uuid(")
    (insert uuid)
    (insert ")]\n")
    (insert "interface ")
    (insert interface-name)
    (insert " : nsISupports")
    (insert "\n{\n")))

(defun com-end-idl (interface-name)
  (insert "};\n"))

(defun com-idl-pathname (protocol phase)
  (c-path (concat (com-interface-name protocol phase) ".idl")))

(defun com-idl-generate ()
  (when (file-exists-p (com-uuid-table-pathname))
    (load (com-uuid-table-pathname)))

  (ensure-directory (c-path))
  (loop for protocol being each hash-value of *protocol-hash-table*
        unless (removed-protocol-p protocol)
        do
        (loop for phase in '(:creating :setting :using)
              for interface-name = (com-interface-name protocol phase)
              for uuid = (com-ensure-uuid interface-name)
              do
              (with-temp-file (com-idl-pathname protocol phase)
                (setq *last-protocol* protocol)
                (com-start-idl protocol phase)
                (com-idl-print-methods-in-phase protocol phase)
                (com-end-idl interface-name)
                ))))

(defun com-impl-name (protocol phase)
  (concat *com-impl-prefix*
          (com-phase-name protocol phase)
          "Impl"))

(defun com-impl-pathname (protocol phase suffix)
  (c-path (concat (com-impl-name protocol phase) suffix)))

(defun com-impl-ns-decl (protocol phase)
  (concat "NS_DECL_" (upcase (com-interface-name protocol phase))))

(defun com-impl-type-from-idl-type (idl-type)
  (cdr (find idl-type *com-idl-to-c++-type-alist*
             :key #'car
             :test #'(lambda (a b)
                       (let ((expr (concat (concat "^" b) "$")))
                         (string-match expr a))))))

(defun com-impl-type (objc-type)
  (let* ((idl-type (com-objc-to-idl-type objc-type))
         (c++-type (com-impl-type-from-idl-type idl-type)))
    (if c++-type
        c++-type
      (concat idl-type "*"))))

(defun com-impl-print-interface-include (iprotocol phase)
  (insert "#include <")
  (insert (com-interface-name iprotocol phase))
  (insert ".h>\n"))

(defun com-impl-map-protocols (protocol-func)
  (loop for protocol in
        (sort
         (loop for protocol being each hash-value of *protocol-hash-table*
               when (and (real-class-p protocol)
                         (not (removed-protocol-p protocol)))
               collect protocol)
         #'(lambda (a b)
             (string< (protocol-name a)
                      (protocol-name b))))
        do
        (loop for phase in '(:creating :using)
              do (funcall protocol-func protocol phase))))

(defun com-impl-generate-headers (protocol phase)
  (let ((iprotocols (cons protocol
                          (included-protocol-list protocol))))
    (with-temp-file
        (com-impl-pathname protocol phase ".h")
      (let ((cppsym 
             (concat "__gen_" (com-impl-name protocol phase) "_h__\n")))
        (insert "#ifndef ")
        (insert cppsym)
        (insert "\n")
        (insert "#define ")
        (insert cppsym)
        (insert "\n"))
      (loop for iprotocol in iprotocols
            do
            (com-impl-print-interface-include iprotocol phase)
            (com-impl-print-interface-include iprotocol :setting))
      (insert "\n")
      (when (eq phase :using)
        (loop for argument-protocol being each hash-value of
              (create-type-hash-table-for-convenience-create-methods protocol)
              do
              (com-impl-print-interface-include argument-protocol :using)))
      (insert "\n")
      (insert "class ")
      (insert (com-impl-name protocol phase))
      (insert ": public ")
      (print-implemented-interfaces-list
       protocol phase
       #'(lambda (imodule iprotocol iphase)
           (com-interface-name iprotocol iphase))
       t)
      (insert "\n")
      (insert "{\n")
      (insert "public:\n")
      (insert "  ")
      (insert (com-impl-name protocol phase))
      (insert " ();\n")
      (when (eq phase :using)
        (loop for method in (collect-convenience-create-methods protocol)
              do
              (insert "  ")
              (com-impl-print-class-constructor-method-declaration protocol
                                                                   method)
              (insert ";\n")))
      (insert "  virtual ~")
      (insert (com-impl-name protocol phase))
      (insert " ();\n")
      (insert "\n")
      (insert "  NS_DECL_ISUPPORTS\n\n")
      (loop for iprotocol in iprotocols
            do
            (insert "  ")
            (insert (com-impl-ns-decl iprotocol :setting))
            (insert "\n")
            (insert "  ")
            (insert (com-impl-ns-decl iprotocol phase))
            (insert "\n"))
      (insert "\n")
      (insert "};\n")
      (insert "#endif\n"))))

(defun com-impl-print-basic-constructor (protocol)
  (let ((name (com-impl-name protocol :creating)))
    (insert name)
    (insert "::")
    (insert name)
    (insert " ()\n")
    (insert "{\n")
    (insert "  NS_INIT_REFCNT ();\n")
    (insert "}\n")
    (insert "\n")))

(defun com-impl-print-class-constructor-method-declaration (protocol method)
  (let ((name (com-impl-name protocol :using))
        (name.arguments
         (collect-convenience-constructor-name.arguments method)))
    (insert name)
    (insert " (")
    (insert (com-interface-name (lookup-protocol "Zone") :using))
    (insert "* aZone")
    (loop for name.argument in name.arguments
          do
          (insert ", ")
          (com-print-argument (cdr name.argument) #'com-impl-type))
    (insert ")")))

(defun com-impl-print-class-constructor-method (protocol method)
  (let ((name (com-impl-name protocol :using))
        (name.arguments
         (collect-convenience-constructor-name.arguments method)))
    (insert name)
    (insert "::")
    (com-impl-print-class-constructor-method-declaration protocol method)
    (insert "\n{\n")
    (insert "  new ")
    (insert (com-impl-name protocol :creating))
    (insert " (this).Create")
    (loop for name.argument in name.arguments
          do
          (insert "_")
          (insert (car (cdr name.argument))))
    (insert " (")
    (insert "aZone")
    (loop for name.argument in name.arguments
          do
          (insert ", ")
          (insert (caddr (cdr name.argument))))
    (insert ");\n")
    (insert "  NS_INIT_REFCNT ();\n")
    (insert "}\n")
    (insert "\n")))

(defun com-impl-print-convenience-constructors (protocol)
  (loop for method in (collect-convenience-create-methods protocol)
        do
        (com-impl-print-class-constructor-method protocol method)))

(defun com-impl-print-destructor (protocol phase)
  (let ((name (com-impl-name protocol phase)))
    (insert name)
    (insert "::~")
    (insert name)
    (insert " ()\n")
    (insert "{\n")
    (insert "}\n")
    (insert "\n")))

(defun com-impl-generate-supports (protocol phase)
  (let* ((iprotocols (cons protocol (included-protocol-list protocol))))
    (insert "NS_IMPL_ISUPPORTS")
    (insert (prin1-to-string (* (length iprotocols) 2)))
    (insert " (")
    (insert (com-impl-name protocol phase))
    (loop for iprotocol in iprotocols
          do
          (insert ", ")
          (insert (com-interface-name iprotocol :setting))
          (insert ", ")
          (insert (com-interface-name iprotocol phase)))
    (insert ");")
    (insert "\n")))

(defun com-impl-print-method-definition (protocol phase method)
  (let* ((arguments (method-arguments method))
         (first-argument (car arguments)))
    (insert "NS_IMETHODIMP\n")
    (insert (com-impl-name protocol phase))
    (insert "::")
    (insert (com-c++-method-name arguments))
    (insert " (")
    (let ((ret (com-impl-type (method-return-type method))))
      (flet ((print-return-argument (preceding-arguments-flag)
                                    (unless (string= ret "void")
                                      (when preceding-arguments-flag
                                        (insert ", "))
                                      (let ((start (point)))
                                        (insert ret)
                                        (save-excursion
                                          (let ((end (point)))
                                            (goto-char start)
                                            (when (search-forward "const " end t)
                                              (replace-match "")))))
                                      (insert "* ret"))))
        (if (com-print-argument first-argument #'com-impl-type)
            (progn
              (loop for argument in (cdr arguments)
                    do
                    (insert ", ")
                    (com-print-argument argument #'com-impl-type))
              (print-return-argument t))
          (print-return-argument nil))))
    (insert ")\n")
    (insert "{\n")
    (insert "  return NS_OK;\n")
    (insert "}\n")
    (insert "\n")))

(defun com-impl-print-method-definitions (protocol phase)
  (loop for method in (protocol-method-list protocol)
        when (included-method-p protocol method phase)
	do
        (com-impl-print-method-definition protocol phase method)
        (insert "\n")))

(defun com-impl-print-impl-include (protocol phase)
  (insert "#include \"")
  (insert (com-impl-name protocol phase))
  (insert ".h\"\n"))

(defun com-impl-print-namestring (protocol phase)
  (insert "\"")
  (insert (com-impl-name protocol phase))
  (insert "\""))

        
(defun com-impl-generate-c++ (protocol phase)
  (with-temp-file 
      (com-impl-pathname protocol phase ".cpp")
    (when (eq phase :using)
      (com-impl-print-impl-include protocol :using))
    (com-impl-print-impl-include protocol :creating)
    (loop for argument-protocol being each hash-value of
          (create-type-hash-table protocol phase)
          do
          (com-impl-print-interface-include argument-protocol :using))
    (insert "\n")
    (when (eq phase :using)
      (loop for argument-protocol being each hash-value of
            (create-type-hash-table-for-convenience-create-methods protocol)
            do
            (com-impl-print-interface-include argument-protocol :using)))
    (com-impl-print-basic-constructor protocol)
    (when (eq phase :using)
      (com-impl-print-convenience-constructors protocol))
    (com-impl-print-destructor protocol phase)
    (com-impl-print-method-definitions protocol phase)))

(defun com-protocol-sym (protocol phase suffix)
  (concat
   "SWARM_"
   (upcase (symbol-name (module-sym (protocol-module protocol))))
   "_"
   (upcase (com-phase-name protocol phase))
   "_"
   suffix))

(defun com-print-cid-define (protocol phase)
  (flet ((print-hex (str)
                    (insert "0x")
                    (insert str)))
    (insert "#define ")
    (insert (com-protocol-sym protocol phase "CID \\\n"))
    (insert "{")
    (let ((l (split-string
              (com-ensure-uuid (com-impl-name protocol phase))
              "-")))
      (print-hex (first l))
      (insert ", ")
      (print-hex (second l))
      (insert ", ")
      (print-hex (third l))
      (insert ", {")
      (let ((l4 (fourth l)))
        (print-hex (substring l4 0 2))
        (insert ", ")
        (print-hex (substring l4 2 4)))
      (let ((l5 (fifth l)))
        (loop for i from 0 below 6
              for start = (* i 2)
              do
              (insert ", ")
              (print-hex (substring l5 start (+ start 2)))))))
  (insert "}}\n"))

(defun com-print-progid-define (protocol phase)
  (insert "#define ")
  (insert (com-protocol-sym protocol phase "PROGID"))
  (insert " ")
  (insert "\"component://swarm/")
  (insert (symbol-name (module-sym (protocol-module protocol))))
  (insert "/")
  (insert (downcase (com-phase-name protocol phase)))
  (insert "\"\n"))


(defun com-impl-generate-module ()
  (with-temp-file (c-path "module.cpp")
    (insert "#include \"nsIModule.h\"\n")
    (insert "#include \"nsIGenericFactory.h\"\n")
    (com-impl-map-protocols #'com-impl-print-impl-include)
    (insert "\n")
    (com-impl-map-protocols
     #'(lambda (protocol phase)
         (insert "NS_GENERIC_FACTORY_CONSTRUCTOR (")
         (insert (com-impl-name protocol phase))
         (insert ");\n")))
    (insert "\n")
    (com-impl-map-protocols #'com-print-cid-define)
    (insert "\n")
    (com-impl-map-protocols #'com-print-progid-define)
    (insert "\n")
    (insert "static nsModuleComponentInfo components[] = {\n")
    (let ((first t))
      (com-impl-map-protocols
       #'(lambda (protocol phase)
           (if first
               (progn
                 (insert "  ")
                 (setq first nil))
             (insert ",\n  "))
           (insert "{")
           (com-impl-print-namestring protocol phase)
           (insert ", ")
           (insert (com-protocol-sym protocol phase "CID"))
           (insert ", ")
           (insert (com-protocol-sym protocol phase "PROGID"))
           (insert ", ")
           (insert (com-impl-name protocol phase))
           (insert "Constructor")
           (insert ", NULL, NULL }"))))
    (insert "};\n")
    (insert "\n")
    (insert "NS_IMPL_NSGETMODULE(\"swarmModule\", components)\n")))

(defun com-init ()
  (setq *extra-removed-methods* '("-addJavaObject:"
                                  "-setJavaMethod:inObject:"))
  
  (setq *extra-unwanted-create-method-signatures*
        '("+create:setAutoDrop:")))

(defun run ()
  (com-init)
  (let ((*idl-flag* t))
    (load-and-process-modules :uniquify-method-lists t))
  (print-makefile.common)
  (com-idl-generate)
  (com-impl-map-protocols #'com-impl-generate-headers)
  (com-impl-map-protocols #'com-impl-generate-c++)
  (com-impl-generate-module)
  (com-save-uuid-table))


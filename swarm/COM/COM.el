(require 'cl)
(eval-and-compile
  (push (getenv "TOP_BUILDDIR") load-path))
(require 'protocol)
(require 'interface)

(defconst *com-interface-prefix* "swarmI")
(defconst *com-impl-prefix* "swarm")

(defconst *com-objc-to-idl-type-alist*
    '(("id.*" . "nsISupports")

      ;; can't use nsCIDRef because that would result in pointer to a reference
      ;; on return
      ("Class" . "nsCIDPtr") 

      ("SEL" . "swarmISelector")
      
      ("void" . "void")
      ("const char \\*" . "string")
      ("char \\*" . "string")
      ("const char \\*\\*" . "string-array")

      ("char" . "char")
      
      ("unsigned char" . "octet")
      ("int" . "long")
      ("short" . "short")
      ("unsigned short" . "unsigned short")
      ("double" . "double")
      ("float" . "float")
      ("unsigned" . "unsigned long")
      ("BOOL" . "boolean")

      ("long" . "long")
      ("unsigned long" . "unsigned long")
      ("timeval_t" . "unsigned long")
      ("size_t" . "unsigned long")

      ("long long" . "long long")
      ("unsigned long long" . "unsigned long long")
      ("unsigned long long int" . "unsigned long long")

      ("COMOBJECT" . "nsISupports")

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
  '(("string-array" . "const char **")
    ("void" . "void")
    ("boolean" . "PRBool")
    ("octet" . "PRUint8")
    ("short" . "PRInt16")
    ("long" . "PRInt32")
    ("long long" . "PRInt64")
    ("unsigned short" . "PRUint16")
    ("unsigned long" . "PRUint32")
    ("long long" . "PRInt64")
    ("unsigned long long" . "PRUint64")
    ("float" . "float")
    ("double" . "double")
    ("char" . "char")
    ("wchar" . "PRUnichar")
    ("string" . "const char*")
    ("wstring" . "PRUnichar*")
    ("nsCIDPtr" . "const nsCID*")
    ))

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
    (let ((firstKey (car (first arguments))))
      (princ firstKey)
      (loop for argument in (cdr arguments)
            for nameKey = (car argument)
            do
            (if nameKey
                (progn
                  (princ "_")
                  (princ nameKey))
              ;; the idea here is that constructors won't have non-keyed
              ;; variant names, but features like forEach may
              (unless (string= firstKey "create")
                (princ "X")))))))
    
(defun com-c++-method-name (arguments)
  (let ((idl-name (com-idl-method-name arguments)))
    (concat (upcase (substring idl-name 0 1))
            (substring idl-name 1))))

(defun com-idl-type (objc-type)
  (let ((idl-type (com-objc-to-idl-type objc-type)))
    (unless idl-type
      (error "No IDL type for `%s'" objc-type))
    (if (eq idl-type 'freaky)
        (progn
          (freaky-message objc-type)
          "nsISupports")
      idl-type)))

(defun com-arg-idl-type (objc-type)
  (let ((type (com-idl-type objc-type)))
    (if (string= "string-array" type)
        "[array, size_is (count)] in string"
      (concat "in " type))))

(defun com-idl-print-argument (argument)
  (print-argument argument
                  #'com-arg-idl-type
                  #'(lambda (name)
                      (cond ((string= name "context") "context_")
                            ((string= name "class") "class_")
                            (t name)))))

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

(defun com-remove-const (type)
  (with-temp-buffer
    (let ((start (point)))
      (insert type)
      (save-excursion
        (let ((end (point)))
          (goto-char start)
          (when (search-forward "const " end t)
            (replace-match "")))))
    (buffer-string)))

(defun com-idl-return-type (protocol phase method)
  (if (method-factory-flag method)
      (com-interface-name protocol phase)
    (com-idl-type (method-return-type method))))

(defun com-impl-defining-interface (protocol phase method)
  (let* ((methodinfo-list (protocol-expanded-methodinfo-list protocol))
         (methodinfo (find method methodinfo-list :key #'methodinfo-method)))
      (com-interface-name (methodinfo-protocol methodinfo) phase)))

(defun com-impl-return-type (protocol phase method)
  (if (method-factory-flag method)
      (concat (com-impl-defining-interface protocol phase method) "*")
    (com-remove-const (com-impl-type (method-return-type method)))))

(defun com-impl-argument-name (name)
  (concat "_" name))

(defun com-impl-print-argument (argument)
  (print-argument argument #'com-impl-type #'com-impl-argument-name))

(defun com-impl-print-objc-argument (argument)
  (print-argument argument #'com-simplify-type #'com-impl-argument-name))

(defun com-swarm-type-p (type)
  (when (stringp type)
    (string-match (concat "^" *com-interface-prefix*) type)))

(defun com-idl-print-method-declaration (protocol phase method)
  (let* ((arguments (method-arguments method))
         (first-argument (car arguments)))
    (insert "  ")
    (insert (com-idl-return-type protocol phase method))
    (insert " ")
    (insert (com-idl-method-name arguments))
    (insert " (")
    (com-idl-print-argument first-argument)
    (loop for argument in (cdr arguments)
          do
          (insert ", ")
          (com-idl-print-argument argument))
    (insert ");\n")))

(defun com-idl-print-getter-as-attribute (protocol phase method)
  (insert "  readonly attribute ")
  (insert (com-idl-return-type protocol phase method))
  (insert " ")
  (insert (get-variable-name-for-getter-method method))
  (insert ";\n"))

(defun com-idl-print-include (idl-type)
  (insert "#include \"")
  (insert idl-type)
  (insert ".idl\"\n"))

(defun com-idl-print-includes (protocol phase)
  (let ((ht (create-type-hash-table protocol phase)))
    (insert "\n")
    
    ;; this won't get picked up by collecting protocols, as SEL isn't one.
    (insert "#include \"swarmISelector.idl\"\n") 
    
    (loop for objc-type being each hash-key of ht
          do
          (insert "interface ")
          (insert (com-objc-to-idl-type objc-type))
          (insert ";\n"))
    (insert "\n")
    (when (inclusive-phase-p phase :using)
      (let ((cht (create-type-hash-table-for-immediate-convenience-create-methods protocol)))
        (loop for objc-type being each hash-key of cht
              unless (gethash objc-type ht)
              do
              (insert "interface ")
              (insert (com-objc-to-idl-type objc-type))
              (insert ";\n"))
        (insert "\n")
        (loop for objc-type being each hash-key of cht
              unless (gethash objc-type ht)
              do (com-idl-print-include (com-objc-to-idl-type objc-type)))
        (insert "\n")))
    (loop for objc-type being each hash-key of ht
          do (com-idl-print-include (com-objc-to-idl-type objc-type)))))

(defun com-idl-print-attributes (protocol)
  (loop for method in (method-list-for-phase protocol :getters)
        do
        (com-idl-print-getter-as-attribute protocol :getters method)))

(defun com-idl-print-methods-in-phase (protocol phase)
  (loop for method in (method-list-for-phase protocol phase)
        for sig = (get-method-signature method)
        when (and (not (eq (method-phase method) :getters))
                  (creating-phase-method-p method))
	do
        (com-idl-print-method-declaration protocol phase method)))

(defun com-idl-print-create-methods (protocol)
  (loop for method in (method-list-for-phase protocol :creating)
        when (convenience-create-method-p protocol method)
	do
        (com-idl-print-method-declaration protocol :using method)))

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

(defun interface-phases-for-protocol (protocol)
  (if (string= (protocol-name protocol) "Selector")
    '(:using)
    '(:creating :setting :using)))

(defun impl-phases-for-protocol (protocol)
  (if (string= (protocol-name protocol) "Selector")
      '(:using)
    '(:creating :using)))

(defun com-wrapped-protocols ()
  (loop for protocol being each hash-value of *protocol-hash-table*
        unless (removed-protocol-p protocol)
        collect protocol))

(defun com-idl-generate ()
  (when (file-exists-p (com-uuid-table-pathname))
    (load (com-uuid-table-pathname)))

  (ensure-directory (c-path))
  (loop for protocol in (com-complete-protocols)
        do
        (loop for phase in (interface-phases-for-protocol protocol)
              for interface-name = (com-interface-name protocol phase)
              for uuid = (com-ensure-uuid interface-name)
              do
              (with-temp-file (com-idl-pathname protocol phase)
                (setq *last-protocol* protocol)
                (com-start-idl protocol phase)
                (when (inclusive-phase-p phase :using)
                  (com-idl-print-attributes protocol)
                  (com-idl-print-create-methods protocol))
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

(defun com-impl-print-interface-include (iprotocol phase)
  (insert "#include <")
  (insert (com-interface-name iprotocol phase))
  (insert ".h>\n"))

(defun com-impl-map-protocol-list (protocol-list protocol-func)
  (loop for protocol in
        (sort
         protocol-list
         #'(lambda (a b)
             (string< (protocol-name a)
                      (protocol-name b))))
        do
        (loop for phase in (impl-phases-for-protocol protocol)
              do (funcall protocol-func protocol phase))))

(defun selector-protocol ()
  (make-protocol
   :name "Selector"
   :module (lookup-module 'swarm)
   :included-protocol-list (list (lookup-protocol "CREATABLE"))
   :method-list (list
                 (make-method
                  :phase :creating
                  :factory-flag t
                  :arguments (list (list "create" "id" "obj")
                                   (list nil "const char *" "methodName")
                                   (list nil "BOOL" "objcFlag"))
                  :return-type "id <Selector>"))))
                 
(defun com-complete-protocols ()
  (cons (selector-protocol) (com-wrapped-protocols)))

(defun com-impl-wrapped-protocols ()
  (remove-if-not #'real-class-p (com-wrapped-protocols)))

(defun com-impl-complete-protocols ()
  (remove-if-not #'real-class-p (com-complete-protocols)))

(defun com-impl-map-wrapped-protocols (protocol-func)
  (com-impl-map-protocol-list (com-impl-wrapped-protocols) protocol-func))

(defun com-impl-map-complete-protocols (protocol-func)
  (com-impl-map-protocol-list (com-impl-complete-protocols) protocol-func))

(defun com-impl-generate-headers (protocol phase)
  (let ((iprotocols (generate-complete-protocol-list protocol)))
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
      (insert "#include <swarmITyping.h>\n")
      (loop for iprotocol in iprotocols
            do
            (com-impl-print-interface-include iprotocol phase)
            (com-impl-print-interface-include iprotocol :setting))
      (insert "\n")
      (when (inclusive-phase-p phase :using)
        (loop for argument-protocol being each hash-value of
              (create-type-hash-table-for-convenience-create-methods protocol)
              do
              (com-impl-print-interface-include argument-protocol :using)))
      (insert "\n")
      (insert "class ")
      (insert (com-impl-name protocol phase))
      (insert ": ")
      (print-implemented-interfaces-list
       protocol phase
       #'(lambda (imodule iprotocol iphase)
           (insert "public ")
           (com-interface-name iprotocol iphase))
       t)
      (insert ", public swarmITyping")
      (insert "\n")
      (insert "{\n")
      (insert "public:\n")
      (insert "  ")
      (insert (com-impl-name protocol phase))
      (insert " ();\n")
      (insert "  virtual ~")
      (insert (com-impl-name protocol phase))
      (insert " ();\n")
      (insert "\n")
      (insert "  NS_DECL_ISUPPORTS\n")
      (insert "  NS_DECL_SWARMITYPING\n")
      (insert "\n") 
      (loop for iprotocol in iprotocols
            do
            (insert "  ")
            (insert (com-impl-ns-decl iprotocol phase))
            (insert "\n")
            (insert "  ")
            (insert (com-impl-ns-decl iprotocol :setting))
            (insert "\n"))
      (insert "\n")
      (when (and (string= (protocol-name protocol) "SwarmEnvironment")
                 (inclusive-phase-p phase :using))
        (insert "  NS_IMETHOD Init ();\n"))
      (insert "};\n")
      (insert "#endif\n"))))

(defun com-impl-print-basic-constructor (protocol phase)
  (let ((name (com-impl-name protocol phase)))
    (insert name)
    (insert "::")
    (insert name)
    (insert " ()\n")
    (insert "{\n")
    (insert "  NS_INIT_REFCNT ();\n")
    (insert "}\n")
    (insert "\n")))

(defun com-cid (protocol phase)
  (concat (com-impl-name protocol phase) "CID"))

(defun com-impl-print-startup-init-method ()
  (insert "NS_IMETHODIMP\n")
  (insert "swarmSwarmEnvironmentImpl::Init ()\n")
  (insert "{\n")
  
  (insert "  static COMEnv env = { createComponent, findComponent, copyString, getName, addRef };\n")
  (insert "  initCOM (&env);\n")
  (insert "  return NS_OK;\n")
  (insert "}\n\n"))
  
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
  (let* ((iprotocols (generate-complete-protocol-list protocol))
         (class (com-impl-name protocol phase)))
    (insert "NS_IMPL_ADDREF(")
    (insert class)
    (insert ")\n")
    (insert "NS_IMPL_RELEASE(")
    (insert class)
    (insert ")\n")
    (insert "NS_INTERFACE_MAP_BEGIN(")
    (insert class)
    (insert ")\n")
    (insert "NS_INTERFACE_MAP_ENTRY(swarmITyping)\n")
    (loop for iprotocol in iprotocols
          do
          (insert "NS_INTERFACE_MAP_ENTRY(")
          (insert (com-interface-name iprotocol phase))
          (insert ")\n")
          (insert "NS_INTERFACE_MAP_ENTRY(")
          (insert (com-interface-name iprotocol :setting))
          (insert ")\n"))
    (insert "NS_INTERFACE_MAP_ENTRY_AMBIGUOUS(nsISupports, ")
    (insert (com-interface-name protocol phase))
    (insert ")\n")
    (insert "NS_INTERFACE_MAP_END\n")
    (insert "\n")))

(defun com-simplify-type (type)
  (cond (type (strip-regexp type " *<[^>]*> *"))
        (t "id")))

(defun com-simplify-return-type (ret-type)
    (com-remove-const (com-simplify-type ret-type)))

(defun com-impl-print-get-imp-pointer (method)
  (insert "  ")
  (insert (com-simplify-return-type (method-return-type method)))
  (if (method-factory-flag method)
      (insert " (*COMswarm_imp) (Class target, SEL sel")
    (insert " (*COMswarm_imp) (id target, SEL sel"))
  (when (has-arguments-p method)
    (loop for argument in (method-arguments method)
          do
          (insert ", ")
          (com-impl-print-objc-argument argument)))
  (insert ");\n")
  (if (method-factory-flag method)
      (progn
        (insert "  MetaClass mClass = class_get_meta_class (COMswarm_target);\n")
        (insert "  (IMP) COMswarm_imp = class_get_class_method (mClass, COMswarm_sel)->method_imp;\n"))
    (insert "  (IMP) COMswarm_imp = objc_msg_lookup (COMswarm_target, COMswarm_sel);\n")))

(defun com-impl-print-call-imp-pointer-body (method)
  (insert "(*COMswarm_imp) (COMswarm_target, COMswarm_sel")
  (when (has-arguments-p method)
    (loop for argument in (method-arguments method)
          for name = (com-impl-argument-name (argument-name argument))
          for objc-type = (argument-type argument)
          for idl-type = (com-idl-type objc-type)
          for type = (com-simplify-type objc-type)
          do
          (insert ", ")
          (cond ((string= idl-type "string-array")
                 (insert name))
                ((string= type "id")
                 (insert "SD_COM_ENSURE_OBJECT_OBJC (")
                 (insert name)
                 (insert ")"))
                ((string= type "SEL")
                 (insert "SD_COM_ENSURE_SELECTOR_OBJC (")
                 (insert name)
                 (insert ")"))
                ((string= type "Class")
                 (insert "SD_COM_ENSURE_CLASS_OBJC (")
                 (insert name)
                 (insert ")"))
                (t (insert name)))))
  (insert ")"))

(defun com-impl-print-call-imp-pointer (protocol phase method)
  (let ((ret-type (com-simplify-return-type (method-return-type method))))
    (insert "  ")
    (if (method-factory-flag method)
        (progn
          (insert "COMswarm_newobj = ")
          (com-impl-print-call-imp-pointer-body method)
          (insert ";\n")
          ;; there is no QueryInterface here because we shouldn't
          ;; ever be dealing with a non-interface to begin with?
          (insert "  SD_COM_ADD_THIS_OBJECT_COM (COMswarm_newobj);\n")
          (insert "  NS_ADDREF (*ret = NS_STATIC_CAST (")
          (insert (com-impl-return-type protocol phase method))
          (insert ", this));\n"))
      (progn
        (unless (string= ret-type "void")
          (insert "*ret = "))
        (cond ((string= ret-type "id")
               (insert "SD_COM_ENSURE_OBJECT_COM_CAST (")
               (insert (com-impl-return-type protocol phase method))
               (insert ", ")
               (com-impl-print-call-imp-pointer-body method)
               (insert ")"))
              ((string= ret-type "Class")
               (insert "SD_COM_FIND_CLASS_COM_CAST (")
               (insert (com-impl-return-type protocol phase method))
               (insert ", ")
               (com-impl-print-call-imp-pointer-body method)
               (insert ")"))
              ((string= ret-type "char *")
               (insert "(char *) SD_COM_COPY_STRING (")
               (com-impl-print-call-imp-pointer-body method)
               (insert ")"))
              (t (com-impl-print-call-imp-pointer-body method)))
        (insert ";\n")))))
    
(defun com-impl-print-method-definition (protocol phase method)
  (let* ((arguments (method-arguments method))
         (first-argument (car arguments)))
    (insert "NS_IMETHODIMP\n")
    (insert (com-impl-name protocol phase))
    (insert "::")
    (insert (com-c++-method-name arguments))
    (insert " (")
    (let ((ret (com-impl-return-type protocol phase method)))
      (flet ((print-return-argument (preceding-arguments-flag)
                                    (unless (string= ret "void")
                                      (when preceding-arguments-flag
                                        (insert ", "))
                                      (insert ret)
                                      (insert "* ret"))))
        (if (com-impl-print-argument first-argument)
            (progn
              (loop for argument in (cdr arguments)
                    do
                    (insert ", ")
                    (com-impl-print-argument argument))
              (print-return-argument t))
          (print-return-argument nil))))
    (insert ")\n")
    (insert "{\n")
    (insert "  nsresult rv = NS_OK;\n")
    (if (method-factory-flag method)
        (progn
          (insert "  id COMswarm_newobj;\n")
          (insert "  Class COMswarm_target = objc_lookup_class (\"")
          (insert (protocol-name protocol))
          (insert "\");\n"))
      (insert "  id COMswarm_target = SD_COM_ENSURE_THIS_OBJECT_OBJC ();\n"))
    (insert "  SEL COMswarm_sel = sel_get_uid (\"")
    (insert (substring (get-method-signature method) 1))
    (insert "\");\n")
    (com-impl-print-get-imp-pointer method)
    (com-impl-print-call-imp-pointer protocol phase method)
    ;(when ht (com-impl-print-objc-method-declaration ht method))
    (insert "  return rv;\n")
    (insert "}\n")
    (insert "\n")))

(defun com-impl-print-method-definitions (protocol actual-phase phase)
  (loop for method in (expanded-method-list protocol phase)
        do
        (when (creating-phase-method-p method)
          (com-impl-print-method-definition protocol actual-phase method)
          (insert "\n"))))

(defun com-impl-print-create-method-definitions (protocol)
  (loop for method in (expanded-method-list protocol :creating)
        do
        (when (convenience-create-method-p protocol method)
          (com-impl-print-method-definition protocol :using method)
          (insert "\n"))))

(defun com-impl-print-impl-include (protocol phase)
  (insert "#include \"")
  (insert (com-impl-name protocol phase))
  (insert ".h\"\n"))

(defun com-impl-print-namestring (protocol phase)
  (insert "\"")
  (insert (com-impl-name protocol phase))
  (insert "\""))

(defun com-impl-print-objc-method-declaration (ht method)
  (insert "  extern ")
  (insert " ")
  (insert (com-simplify-type (method-return-type method)))
  (insert " ")
  (insert (gethash (get-method-signature method) ht))
  (insert " ")
  (insert "(id obj, SEL sel")
  (when (has-arguments-p method)
    (loop for argument in (method-arguments method)
          do
          (insert ", ")
          (insert (com-simplify-type argument))
          (insert " ")
          (insert (argument-name argument))))
  (insert ");\n"))

(defun com-impl-print-typedefs ()
  (loop for typedef in (collect-objects-of-type 'typedef)
        for name = (typedef-name typedef)
        unless (or (string= name "types_t")
                   (string= name "fcall_type_t")
                   (string= name "compare_t")
                   (string= name "member_t")
                   (string= name "dupmember_t")
                   (string= name "val_t"))
        do
        (insert "typedef ")
        (insert (typedef-type typedef))
        (insert " ")
        (insert (typedef-name typedef))
        (insert ";\n"))
  (insert "\n"))

(defun com-impl-print-get-iid (protocol phase)
    (insert "NS_IMETHODIMP\n")
    (insert (com-impl-name protocol phase))
    (insert "::GetPrimaryiid (nsIID **aiid)\n")
    (insert "{\n")
    ;; NS_STATIC_CAST doesn't work here
    (insert "  *aiid = (nsIID *) &NS_GET_IID (")
    (insert (com-interface-name protocol phase))
    (insert ");\n")
    (insert "  return NS_OK;\n")
    (insert "}\n\n"))

(defun com-impl-print-get-cid (protocol phase)
    (insert "NS_IMETHODIMP\n")
    (insert (com-impl-name protocol phase))
    (insert "::GetCid (nsCID **acid)\n")
    (insert "{\n")
    (insert "  static NS_DEFINE_CID (cid, ")
    (insert (com-protocol-sym protocol phase "CID"))
    (insert ");\n")
    (insert "  *acid = (nsCID *) &cid;\n")
    (insert "  return NS_OK;\n")
    (insert "}\n\n"))

(defun com-impl-generate-c++ (protocol phase)
  (with-temp-file 
      (com-impl-pathname protocol phase ".cpp")
    (when (inclusive-phase-p phase :using)
      (com-impl-print-impl-include protocol :using))
    (com-impl-print-impl-include protocol :creating)
    (loop for argument-protocol being each hash-value of
          (create-type-hash-table protocol phase)
          do
          (com-impl-print-interface-include argument-protocol :using))
    (insert "\n")
    (insert "#include \"COMsupport.h\"\n")
    (insert "#include \"componentIDs.h\"\n")
    (when (inclusive-phase-p phase :using)
      (insert "\n")
      (loop for argument-protocol being each hash-value of
            (create-type-hash-table-for-convenience-create-methods protocol)
            do
            (com-impl-print-interface-include argument-protocol :using))
      (insert "\n"))
    (insert "extern \"C\" {\n")
    (insert "#include <objc/objc.h>\n")
    (insert "#include <objc/objc-api.h>\n")
    (insert "}\n")
    (insert "#include <defobj/COM.h>\n")

    (insert "\n")
    (com-impl-generate-supports protocol phase)
    (com-impl-print-basic-constructor protocol phase)
    (com-impl-print-destructor protocol phase)
    (com-impl-print-get-iid protocol phase)
    (com-impl-print-get-cid protocol phase)
    (when (and (string= (protocol-name protocol) "SwarmEnvironment")
               (inclusive-phase-p phase :using))
      (com-impl-print-startup-init-method))
    (com-impl-print-typedefs)
    (com-impl-print-method-definitions protocol phase :setting)
    (com-impl-print-method-definitions protocol phase phase)
    (when (inclusive-phase-p phase :using)
      (com-impl-print-create-method-definitions protocol))))

(defun com-protocol-sym (protocol phase suffix)
  (concat
   "SWARM_"
   (let ((sym (module-sym (protocol-module protocol))))
     (if (eq sym 'swarm)
         ""
       (concat
        (upcase (symbol-name sym))
        "_")))
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
  (insert (module-path (protocol-module protocol)))
  (insert (com-phase-name protocol phase))
  (insert "Impl\"\n"))

(defun com-impl-generate-component-ids ()
  (with-temp-file (c-path "componentIDs.h")
    (com-impl-map-complete-protocols #'com-print-cid-define)
    (insert "\n")
    (com-impl-map-complete-protocols #'com-print-progid-define)
    (insert "\n")))

(defun com-impl-generate-module ()
  (with-temp-file (c-path "module.cpp")
    (insert "#include \"nsIModule.h\"\n")
    (insert "#include \"nsIGenericFactory.h\"\n")
    (com-impl-map-complete-protocols #'com-impl-print-impl-include)
    (insert "\n")
    (com-impl-map-complete-protocols
     #'(lambda (protocol phase)
         (if (and (string= (protocol-name protocol) "SwarmEnvironment")
                  (inclusive-phase-p phase :using))
             (progn
               (insert "NS_GENERIC_FACTORY_CONSTRUCTOR_INIT (")
               (insert (com-impl-name protocol phase))
               (insert ", Init);\n"))
           (progn
               (insert "NS_GENERIC_FACTORY_CONSTRUCTOR (")
               (insert (com-impl-name protocol phase))
               (insert ");\n")))))
    (insert "#include \"componentIDs.h\"\n")
    (insert "static nsModuleComponentInfo components[] = {\n")
    (let ((first t))
      (com-impl-map-complete-protocols
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
                                  "-setJavaMethod:inObject:"
                                  "-initSwarmUsing:version:bugAddress:args:"))
  
  (setq *extra-unwanted-create-method-signatures*
        '("+create:setAutoDrop:")))

(defun run ()
  (com-init)
  (let ((*idl-flag* t))
    (load-and-process-modules :uniquify-method-lists t))
  (print-makefile.common)
  (com-idl-generate)
  (com-impl-map-wrapped-protocols #'com-impl-generate-headers)
  (com-impl-map-wrapped-protocols #'com-impl-generate-c++)
  (com-impl-generate-component-ids)
  (com-impl-generate-module)
  (com-save-uuid-table))



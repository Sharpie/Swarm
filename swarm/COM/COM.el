(require 'cl)
(eval-and-compile
  (push (getenv "TOP_BUILDDIR") load-path))
(require 'protocol)
(require 'interface)

(defconst *com-interface-prefix* "swarmI")
(defconst *com-impl-prefix* "swarm")

(defconst *com-objc-to-idl-type-alist*
    '(("id +<\\(.*\\)>" . protocol)
      ("id .*" . "nsISupports")
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

      ("JOBJECT" . "nsISupports")

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
    ("string" . "const char *")
    ("wstring" . "PRUnichar *")))

(defvar *com-uuid-hash-table* (make-hash-table :test #'equal))

(defun com-interface-name (protocol phase)
  (concat *com-interface-prefix*
          (protocol-name protocol) (suffix-for-phase phase)))

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
    (loop for interface-name in
          (sort 
           (loop for interface-name being each hash-key of
                 *com-uuid-hash-table*
                 collect interface-name)
           #'string<)
          for uuid = (gethash interface-name *com-uuid-hash-table*)
          do
          (insert "  ")
          (insert (prin1-to-string (cons interface-name uuid)))
          (insert "\n"))
    (insert "))")))
                        
  
(defun com-ensure-uuid (interface-name)
  (let ((uuid (gethash interface-name *com-uuid-hash-table*)))
    (unless uuid
      (message (concat "Generating uuid for `"
                       interface-name
                       "'"))
      (setq uuid
            (with-temp-buffer
              (call-process "uuidgen" nil t nil)
              (goto-char (point-min))
              (end-of-line)
              (buffer-substring (point-min) (point))))
      (setf (gethash interface-name *com-uuid-hash-table*) uuid))
    uuid))

(defun com-type-category-to-com-type (objc-type com-type-category)
  (cond ((eq com-type-category 'freaky)
         (freaky-message objc-type)
         com-type-category)
        ((eq com-type-category 'protocol)
         (let* ((protocol-name (match-string 1 objc-type))
                (protocol (lookup-protocol protocol-name)))
           (if (real-class-p protocol)
               (com-interface-name protocol :using)
             "nsISupports")))
        (t com-type-category)))

(defun com-objc-to-idl-type-category (objc-type)
  (if objc-type
      (cdr (find objc-type *com-objc-to-idl-type-alist*
                 :key #'car
                 :test #'(lambda (a b)
                           (let ((expr (concat (concat "^" b) "$")))
                             (string-match expr a)))))
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

(defun com-argument-type (argument)
  (let* ((type-and-varname (cdr argument))
         (varname (cadr type-and-varname)))
    ;; the case of method with no arguments
    (when varname
      (car type-and-varname))))

(defun com-idl-type (objc-type)
  (let ((idl-type (com-objc-to-idl-type objc-type)))
    (unless idl-type
      (error "No Java type for `%s'" objc-type))
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
  (let ((ht (make-hash-table :test #'equal)))
    (loop for method in (protocol-method-list protocol)
          when (included-method-p protocol method phase)
          do
          (let* ((return-type (method-return-type method))
                 (idl-return-type (com-idl-type return-type)))
            (when (com-swarm-type-p idl-return-type)
              (setf (gethash idl-return-type ht) return-type)))
          (loop for argument in (method-arguments method)
                for argument-type = (com-argument-type argument)
                for idl-argument-type = (com-idl-type argument-type)
                when (com-swarm-type-p idl-argument-type)
                do
                (setf (gethash idl-argument-type ht) argument-type)))
    (insert "\n")
    (loop for idl-type being each hash-key of ht
          do
          (insert "interface ")
          (insert idl-type)
          (insert ";\n"))
    (insert "\n")
    (loop for idl-type being each hash-key of ht
          do (com-idl-print-include idl-type))))

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
                )))
  (com-save-uuid-table))

(defun com-impl-name (protocol phase)
  (concat *com-impl-prefix*
          (protocol-name protocol) (suffix-for-phase phase) "Impl"))

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
      (concat idl-type " *"))))

(defun com-impl-print-include (iprotocol phase)
  (insert "#include <")
  (insert (com-interface-name iprotocol phase))
  (insert ".h>\n"))

(defun com-impl-generate-headers ()
  (loop for protocol being each hash-value of *protocol-hash-table*
        when (and (not (removed-protocol-p protocol))
                  (real-class-p protocol))
        do
        (loop for phase in '(:creating :using)
              for iprotocols = (cons protocol
                                     (included-protocol-list protocol))
              do
              (with-temp-file
                  (com-impl-pathname protocol phase ".h")
                (loop for iprotocol in iprotocols
                      do
                      (com-impl-print-include iprotocol phase)
                      (com-impl-print-include iprotocol :setting))
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
                (insert "};\n")))))
        
(defun com-impl-print-constructor (protocol phase)
  (let ((name (com-impl-name protocol phase)))
    (insert name)
    (insert "::")
    (insert name)
    (insert " ()\n")
    (insert "{\n")
    (insert "  NS_INIT_REFCNT ();\n")
    (insert "}\n")
    (insert "\n")))

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
                                      (insert "*ret"))))
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
        
(defun com-impl-generate-c++ ()
  (loop for protocol being each hash-value of *protocol-hash-table*
        unless (removed-protocol-p protocol)
        do
        (loop for phase in '(:creating :using)
              do
              (with-temp-file 
                  (com-impl-pathname protocol phase ".cpp")
                (insert "#include \"")
                (insert (com-impl-name protocol phase))
                (insert ".h\"\n")
                (insert "\n")
                (com-impl-print-constructor protocol phase)
                (com-impl-print-destructor protocol phase)
                (com-impl-print-method-definitions protocol phase)))))

(defun run ()
  (let ((*idl-flag* t))
    (load-and-process-modules :uniquify-method-lists t))
  (print-makefile.common)
  (com-idl-generate)
  (com-impl-generate-headers)
  (com-impl-generate-c++))
  

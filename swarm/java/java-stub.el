(require 'cl)
(eval-and-compile
 (push (getenv "BUILDDIR") load-path))
(require 'protocol)

(defvar *last-protocol*)
(defconst *stub-directory* "./")

(defun freakyp (java-type)
  (eq java-type 'freaky))

(defconst *objc-to-java-type-alist*
    '(("id .*" . "Object")
      ("void" . "void")
      ("const char \\*" . "String")
      
      ("char \\*" . "String")
      ("char" . "char")
      ("unsigned char" . "byte")
      ("int" . "int")
      ("long" . "long")
      ("double" . "double")
      ("float" . "float")
      ("unsigned" . "int")
      ("unsigned long" . "int")
      ("unsigned long long int" . "long"); really?
      ("BOOL" . "boolean")
      ("void \\*" . "Object")
      ("ref_t" . "Object")
      ("val_t" . "Object")
      ("Protocol \\*" . "Class")
      ("SEL" . "java.lang.reflect.Method")
      ;("notify_t" . "Notify_t")
      ("notify_t" . freaky)
      
      ("const char \\* const \\*" . freaky)
      ("int \\*" . freaky)
      ("double \\*" . freaky)
      ("BOOL \\*" . freaky)
      ("Class" . freaky)
      ("FILE \\*" . freaky)
      ("compare_t" . freaky)
      ("timeval_t" . freaky)
      ("func_t" . freaky)
      ("unsigned \\*" . freaky)
      ("size_t" . freaky)
      ("IMP" . freaky)
      ("const char \\*\\*" . freaky)
      ("int (\\*) (int, const char \\*)" . freaky)
      ("struct argp_option \\*" . freaky)
      ("PixelValue \\*" . freaky)
      ("PixelValue" . freaky)
      ("Color" . freaky)
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

(defun freaky-message- (objc-type)
  (message "Objective C type `%s' in protocol `%s' is freaky!"
           objc-type
           (protocol-name *last-protocol*)))

(defun freaky-message (objc-type)
  )

(defun java-type-for-objc-type (objc-type)
  (if objc-type
      (let ((ret
             (find objc-type *objc-to-java-type-alist*
                   :key #'car
                   :test #'(lambda (a b)
                             (let ((expr (concat (concat "^" b) "$")))
                               (string-match expr a))))))
        (when (freakyp (cdr ret))
          (freaky-message objc-type))
        (cdr ret))
      "Object"))

(defun java-print-type (objc-type)
  (let ((java-type (java-type-for-objc-type objc-type)))
    (unless java-type
      (error "No Java type for `%s'" objc-type))
    (if (freakyp java-type)
        ;(insert "FreakyType")
        (insert "Object")
        (insert java-type))))

(defun java-print-argument (argument)
  (let* ((type-and-varname (cdr argument))
         (varname (cadr type-and-varname)))
    ;; the case of method with no arguments
    (when varname
      (java-print-type (car type-and-varname))
      (insert " ")
      (insert
       (if (string= varname "class")
           "aClass"
           varname)))))

(defun java-print-method (method)
  (let* ((arguments (method-arguments method))
         (first-argument (car arguments)))
    (java-print-type (method-return-type method))
    (insert " ")
    (insert (car first-argument))
    (loop for argument in (cdr arguments)
          for nameKey = (car argument)
          when nameKey
          do
          (insert (upcase (substring nameKey 0 1)))
          (insert (substring nameKey 1)))
    (insert " (")
    (java-print-argument first-argument)
    (loop for argument in (cdr arguments)
	      do
              (insert ", ")
              (java-print-argument argument))
    (insert ");\n")))

(defun removed-method-p (method)
  (let ((arguments (method-arguments method)))
    (or (and (not (cdr arguments))
             (let ((name (caar arguments)))
               (find name '("getClass" "getDisplayName" "setDisplayName"
                            "getTypeName" "copy" "remove")
                     :test #'string=)))
        (loop for argument in arguments
              when (and (null (first argument))
                        (null (second argument))
                        (string= (third argument) "..."))
              return t
              finally return nil))))

(defun method-list (protocol phase)
  (remove-if #'removed-method-p
             (remove-if-not
              #'(lambda (method)
                  (eq (method-phase method) phase))
              (mapcar #'caddr 
                      (protocol-expanded-methodinfo-list protocol)))))
  
  (defun java-print-methods-in-phase (protocol phase native)
  (loop for method in (method-list protocol phase) 
	do
        (when native
          (insert "public native "))
        (java-print-method method)))

(defun java-print-class-methods-in-phase (protocol phase)
  (java-print-methods-in-phase protocol phase t))

(defun java-print-interface-methods-in-phase (protocol phase)
  (java-print-methods-in-phase protocol phase nil))

(defun java-suffix-for-phase (phase)
  (case phase
    (:setting "_s")
    (:using "_u")
    (:creating "")))

(defun java-print-interface-name (protocol phase)
  (insert "i_")
  (insert (protocol-name protocol))
  (insert (java-suffix-for-phase phase)))

(defun java-print-implemented-interfaces-list (protocol phase separator)
  (let ((first t))
    (loop for iprotocol in (included-protocol-list protocol)
          do
          (if first
              (setq first nil)
              (insert separator))
          (java-print-interface-name iprotocol :setting)
          (unless (eq phase :setting)
            (insert separator)
            (java-print-interface-name iprotocol phase)))
    (not first)))

(defun the-CREATABLE-protocol-p (protocol)
  (string= (protocol-name protocol) "CREATABLE"))

(defun included-protocol-list (protocol)
  (remove-if #'the-CREATABLE-protocol-p 
             (protocol-included-protocol-list protocol)))

(defun CREATABLE-p (protocol)
  (member-if #'the-CREATABLE-protocol-p
             (protocol-included-protocol-list protocol)))

(defun java-print-import (protocol phase)
  (insert "import swarm.")
  (insert (module-name (protocol-module protocol)))
  (insert ".")
  (java-print-interface-name protocol phase)
  (insert ";\n"))

(defun java-print-imports (protocol phase)
  (loop for iprotocol in (included-protocol-list protocol)
        do (java-print-import iprotocol phase)))

(defun java-print-implemented-protocols (protocol phase separator interface)
  (if interface
      (when (included-protocol-list protocol)
        (insert "extends")
        (insert " ")
        (java-print-implemented-interfaces-list protocol phase separator))
      (progn
        (insert "implements")
        (insert " ")
        (java-print-implemented-interfaces-list protocol phase separator))))

(defun java-print-class-phase (protocol phase)
  (unless (CREATABLE-p protocol)
    (insert "abstract "))
  (insert "class ")
  (insert (protocol-name protocol))
  (insert (java-suffix-for-phase phase))
  (insert " ")
  (when (java-print-implemented-protocols protocol phase ", " nil)
    (insert ", "))
  (java-print-interface-name protocol :setting)
  (insert " {\n")
  (java-print-class-methods-in-phase protocol phase)
  (java-print-class-methods-in-phase protocol :setting)
  (insert "}\n"))

(defun java-print-interface-phase (protocol phase)
  (insert "public interface ")
  (java-print-interface-name protocol phase)
  (insert " ")
  (java-print-implemented-protocols protocol phase ", " t)
  (insert " {\n")
  (java-print-interface-methods-in-phase protocol phase)
  (insert "}\n"))

(defun ensure-directory (dir)
  (unless (file-directory-p dir)
    (make-directory dir)))

(defmacro with-protocol-file (protocol phase interface &rest body)
  (let ((dir (make-symbol "dir")))
    `(let ((,dir
            (concat *stub-directory*
                    "swarm/"
                    (module-name (protocol-module ,protocol))
                    "/")))
      (ensure-directory ,dir)
      (with-temp-file (concat ,dir
                              (if ,interface "i_" "")
                              (protocol-name ,protocol)
                              (java-suffix-for-phase ,phase)
                              ".java")
        ,@body))))

(defun java-print-package (protocol)
  (insert "package swarm.")
  (insert (symbol-name (module-sym (protocol-module protocol))))
  (insert ";\n"))
      
(defun java-print-class-phase-to-file (protocol phase)
  (with-protocol-file protocol phase nil
                      (java-print-package protocol)
                      (java-print-imports protocol phase)
                      (java-print-imports protocol :setting)
                      (java-print-class-phase protocol phase)))

(defun java-print-class (protocol)
  (loop for phase in '(:creating :using)
        do (java-print-class-phase-to-file protocol phase)))

(defun java-print-interface-phase-to-file (protocol phase)
  (with-protocol-file protocol phase t
                      (java-print-package protocol)
                      (java-print-imports protocol phase)
                      (unless (eq phase :setting)
                        (java-print-imports protocol :setting))
                      (java-print-interface-phase protocol phase)))

(defun java-print-interface (protocol)
  (loop for phase in '(:creating :setting :using)
        do (java-print-interface-phase-to-file protocol phase)))
  
(defun java-print-classes ()
  (interactive)
  (ensure-directory "swarm")
  (loop for protocol being each hash-value of *protocol-hash-table* 
        unless (the-CREATABLE-protocol-p protocol)
        do
        (setq *last-protocol* protocol)
        (java-print-interface protocol)
        (java-print-class protocol)))

(defun java-run-all ()
  (load-and-process-modules)
  (java-print-classes))
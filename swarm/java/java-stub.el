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
      ("long double" . "double"); really?
      ("unsigned long long int" . "long"); really?
      ("BOOL" . "boolean")
      ("void \\*" . "Object")
      ("ref_t" . "Object")
      ("val_t" . "Object")
      ("Protocol \\*" . "Class")
      ("SEL" . "java.lang.reflect.Method")
      ("notify_t" . "Notify_t")
      
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
      ("unsigned long \\*" . freaky)
      ("float \\*" . freaky)
      ("void (\\*) (unsigned rank, unsigned \\*vec, double val)" . freaky)
      ("void (\\*) (unsigned rank, unsigned \\*vec, int val)" . freaky)
      ("ProbeMap \\*" . freaky)
      ))

(defun java-type-for-objc-type (objc-type)
  (if objc-type
      (let ((ret
             (find objc-type *objc-to-java-type-alist*
                   :key #'car
                   :test #'(lambda (a b)
                             (let ((expr (concat (concat "^" b) "$")))
                               (string-match expr a))))))
        (when (freakyp (cdr ret))
          (message "Objective C type `%s' in protocol `%s' is freaky!"
                 objc-type
                 (protocol-name *last-protocol*)))
        (cdr ret))
      "Object"))

(defun java-print-type (objc-type)
  (let ((java-type (java-type-for-objc-type objc-type)))
    (unless java-type
      (error "No Java type for `%s'" objc-type))
    (if (freakyp java-type)
        ;(java-print "FreakyType")
        (java-print "Object")
        (java-print java-type))))

(defun java-print-argument (argument)
  (let* ((type-and-varname (cdr argument))
         (varname (cadr type-and-varname)))
    ;; the case of method with no arguments
    (when varname
      (java-print-type (car type-and-varname))
      (java-print " ")
      (java-print varname))))

(defun java-print-method (method)
  (let ((arguments (car (method-arguments method))))
    (java-print-type (method-return-type method))
    (java-print " ")
    (java-print (car arguments))
    (java-print " (")
    (java-print-argument arguments)
    (loop for argument in (cdr (method-arguments method))
	      do
              (java-print ", ")
              (java-print-argument argument))
    (java-print ");\n")))

(defun java-print-methods-in-phase (protocol phase native)
  (loop for method in (protocol-method-list protocol) 
	do
        (when (eq (method-phase method) phase)
          (when native
            (java-print "native"))
          (java-print " ")
          (java-print-method method))))

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
  (java-print "i_")
  (java-print (protocol-name protocol))
  (java-print (java-suffix-for-phase phase)))

(defun java-print-implemented-interfaces-list (protocol phase separator)
  (let ((first t))
    (loop for iprotocol in (included-protocol-list protocol)
          do
          (if first
              (setq first nil)
              (java-print separator))
          (java-print-interface-name iprotocol :setting)
          (unless (eq phase :setting)
            (java-print separator)
            (java-print-interface-name iprotocol phase)))
    (not first)))

(defun CREATABLE-p (protocol)
  (string= (protocol-name protocol) "CREATABLE"))

(defun included-protocol-list (protocol)
  (remove-if #'CREATABLE-p (protocol-included-protocol-list protocol)))

(defun java-print-import (protocol phase)
  (java-print "import ")
  (java-print-interface-name protocol phase)
  (java-print ";\n"))

(defun java-print-imports (protocol phase)
  (loop for iprotocol in (included-protocol-list protocol)
        do (java-print-import iprotocol phase)))

(defun java-print-implemented-protocols (protocol phase separator interface)
  (if interface
      (when (included-protocol-list protocol)
        (java-print "extends")
        (java-print " ")
        (java-print-implemented-interfaces-list protocol phase separator))
      (progn
        (java-print "implements")
        (java-print " ")
        (java-print-implemented-interfaces-list protocol phase separator))))

(defun java-print-class-phase (protocol phase)
  (unless (CREATABLE-p protocol)
    (java-print "abstract "))
  (java-print "class ")
  (java-print (protocol-name protocol))
  (java-print (java-suffix-for-phase phase))
  (java-print " ")
  (when (java-print-implemented-protocols protocol phase ", " nil)
    (java-print ", "))
  (java-print-interface-name protocol :setting)
  (java-print " {\n")
  (java-print-class-methods-in-phase protocol phase)
  (java-print-class-methods-in-phase protocol :setting)
  (java-print "}\n"))

(defun java-print-interface-phase (protocol phase)
  (java-print "interface ")
  (java-print-interface-name protocol phase)
  (java-print " ")
  (java-print-implemented-protocols protocol phase ", " t)
  (java-print " {\n")
  (java-print-interface-methods-in-phase protocol phase)
  (java-print "}\n"))

(defun stub-protocol-path (protocol phase interface)
  (concat *stub-directory* 
          (if interface "i_" "")
          (protocol-name protocol)
          (java-suffix-for-phase phase)
          ".java"))

(defun java-print-class-phase-to-file (protocol phase)
  (with-temp-file (stub-protocol-path protocol phase nil)
    (java-print-imports protocol phase)
    (java-print-imports protocol :setting)
    (java-print-class-phase protocol phase)))

(defun java-print-class (protocol)
  (loop for phase in '(:creating :using)
        do (java-print-class-phase-to-file protocol phase)))

(defun java-print-interface-phase-to-file (protocol phase)
  (with-temp-file (stub-protocol-path protocol phase t)
    (java-print-imports protocol phase)
    (unless (eq phase :setting)
      (java-print-imports protocol :setting))
    (java-print-interface-phase protocol phase)))

(defun java-print-interface (protocol)
  (loop for phase in '(:creating :setting :using)
        do (java-print-interface-phase-to-file protocol phase)))
  
(defun java-print-classes ()
  (interactive)
  (loop for protocol being each hash-value of *protocol-hash-table* 
        do
        (setq *last-protocol* protocol)
        (java-print-interface protocol)
        (java-print-class protocol)))

(defun java-run-all ()
  (load-and-process-modules)
  (java-print-classes))
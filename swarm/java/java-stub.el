(require 'cl)
(eval-and-compile
 (push (getenv "BUILDDIR") load-path))
(require 'protocol)

(defconst *java-path* "swarm/")
(defconst *c-path* "c/")

(defvar *last-protocol*)

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
      ("BOOL" . "boolean")
      ("SEL" . "java.lang.reflect.Method")

      ("void \\*" . freaky)
      ("ref_t" . freaky)
      ("val_t" . freaky)
      ("Protocol \\*" . freaky)

      ("notify_t" . freaky)
      ("unsigned long long int" . "long"); really?
      
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

(defun java-objc-to-java-type (objc-type)
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
  (let ((java-type (java-objc-to-java-type objc-type)))
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

(defun java-print-class-methods-in-phase (protocol phase)
  (loop for method in (method-list protocol phase) 
	do
        (insert "public native ")
        (java-print-method method)))

(defun java-print-interface-methods-in-phase (protocol phase)
  (loop for method in (protocol-method-list protocol)
        when (and (not (removed-method-p method))
                  (eq phase (method-phase method)))
	do (java-print-method method)))

(defun java-suffix-for-phase (phase)
  (case phase
    (:setting "_s")
    (:using "_u")
    (:creating "")))

(defun java-class-name (protocol phase)
  (concat (protocol-name protocol) (java-suffix-for-phase phase)))

(defun java-interface-name (protocol phase)
  (concat "i_" (java-class-name protocol phase)))

(defun java-print-implemented-interfaces-list (protocol phase separator)
  (let ((first t))
    (loop for iprotocol in (included-protocol-list protocol)
          do
          (if first
              (setq first nil)
              (insert separator))
          (insert (java-interface-name iprotocol :setting))
          (unless (eq phase :setting)
            (insert separator)
            (insert (java-interface-name iprotocol phase))))
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
  (insert (java-interface-name protocol phase))
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
  (insert (java-class-name protocol phase))
  (insert " ")
  (when (java-print-implemented-protocols protocol phase ", " nil)
    (insert ", "))
  (insert (java-interface-name protocol :setting))
  (insert " {\n")
  (java-print-class-methods-in-phase protocol phase)
  (java-print-class-methods-in-phase protocol :setting)
  (insert "}\n"))

(defun java-print-interface-phase (protocol phase)
  (insert "public interface ")
  (insert (java-interface-name protocol phase))
  (insert " ")
  (java-print-implemented-protocols protocol phase ", " t)
  (insert " {\n")
  (java-print-interface-methods-in-phase protocol phase)
  (insert "}\n"))

(defun ensure-directory (dir)
  (unless (file-directory-p dir)
    (make-directory dir)))

(defun module-path (module-sym)
  (concat *java-path*
          (symbol-name module-sym)
          "/"))

(defmacro with-protocol-java-file (protocol phase interface &rest body)
  (let ((dir (make-symbol "dir")))
    `(let ((,dir (module-path (module-sym (protocol-module ,protocol)))))
      (ensure-directory ,dir)
      (with-temp-file (concat ,dir
                              (if ,interface "i_" "")
                              (protocol-name ,protocol)
                              (java-suffix-for-phase ,phase)
                              ".java")
        ,@body))))

(defmacro with-protocol-c-file (protocol &rest body)
  `(progn
    (ensure-directory *c-path*)
    (with-temp-file (concat *c-path* (protocol-name ,protocol) ".c")
      ,@body)))

(defun java-print-package (protocol)
  (insert "package swarm.")
  (insert (module-name (protocol-module protocol)))
  (insert ";\n"))
      
(defun java-print-class-phase-to-file (protocol phase)
  (with-protocol-java-file protocol phase nil
                           (java-print-package protocol)
                           (java-print-imports protocol phase)
                           (java-print-imports protocol :setting)
                           (java-print-class-phase protocol phase)))

(defun java-print-class (protocol)
  (loop for phase in '(:creating :using)
        do (java-print-class-phase-to-file protocol phase)))

(defun java-print-interface-phase-to-file (protocol phase)
  (with-protocol-java-file protocol phase t
                           (java-print-package protocol)
                           (java-print-imports protocol phase)
                           (unless (eq phase :setting)
                             (java-print-imports protocol :setting))
                           (java-print-interface-phase protocol phase)))

(defun java-print-interface (protocol)
  (loop for phase in '(:creating :setting :using)
        do (java-print-interface-phase-to-file protocol phase)))

(defun mangle-signature (str)
  (with-output-to-string 
      (with-temp-buffer
        (let ((beg (point)))
          (insert str)
          (let ((end (point)))
            (save-excursion
              (save-restriction
                (narrow-to-region beg end)
                (goto-char (point-min))
                (while (search-forward "." nil t)
                  (replace-match "_"))))))
        (princ (buffer-string)))))

(defun java-type-to-signature (type)
  (cond ((string= "boolean" type) "Z")
        ((string= "byte" type) "B")
        ((string= "char" type) "C")
        ((string= "short" type) "S")
        ((string= "int" type) "I")
        ((string= "long" type) "J")
        ((string= "float" type) "F")
        ((string= "double" type) "D")
        ((string= "String" type) "Ljava_lang_String_2")
        ((or (string= "Object" type)
             (eq type 'freaky)) "Ljava_lang_Object_2")
        (t (mangle-signature (concat "L" type "_2")))))

(defun java-type-to-native-type (type)
  (cond ((string= "byte" type) "jbyte")
        ((string= "char" type) "jchar")
        ((string= "short" type) "jshort")
        ((string= "int" type) "jint")
        ((string= "long" type) "jlong")
        ((string= "float" type) "jfloat")
        ((string= "double" type) "jdouble")
        ((string= "String" type) "jstring")
        (t "jobject")))

(defun java-argument-convert (argument convert)
  (let* ((type-and-varname (cdr argument))
         (varname (cadr type-and-varname)))
    ;; the case of method with no arguments
    (when varname
      (funcall convert
               (java-objc-to-java-type
                (car type-and-varname))))))

(defun java-argument-empty-p (argument)
  (null (third argument)))

(defun java-print-native-method (method protocol)
  (flet ((insert-arg (arg)
           (insert-char ?\  30)
           (insert arg)))
    (let* ((arguments (method-arguments method))
           (first-argument (car arguments)))
      (insert (java-type-to-native-type (java-objc-to-java-type 
                                         (method-return-type method))))
      (insert "\n")
      (insert "Java_swarm_")
      (insert (module-name (protocol-module protocol)))
      (insert "_")
      (insert (car first-argument))
      (loop for argument in (cdr arguments)
            for nameKey = (car argument)
            when nameKey
            do
            (insert (upcase (substring nameKey 0 1)))
            (insert (substring nameKey 1)))
      (unless (java-argument-empty-p first-argument)
        (insert "_")
        (insert (java-argument-convert first-argument
                                       #'java-type-to-signature))
        (loop for argument in (cdr arguments)
              do
              (insert "_")
              (insert (java-argument-convert argument
                                             #'java-type-to-signature))))
      (insert " (JNIEnv *env, jobject jobj")
      (unless (java-argument-empty-p (car arguments))
        (loop for argument in arguments
              do
              (insert ",\n")
              (insert-arg
               (java-argument-convert argument #'java-type-to-native-type))
              (insert " ")
              (insert (third argument))))
      (insert ")\n")
      (insert "{\n")
      (insert "}\n"))))

(defun java-print-native-class (protocol)
  (with-protocol-c-file protocol
    (insert "#include <jni.h>\n")
    (loop for method in (protocol-method-list protocol)
          unless (removed-method-p method)
          do
          (java-print-native-method method protocol)
          (insert "\n"))))

(defun java-print-makefiles ()
  (ensure-directory *c-path*)
  (with-temp-file "Makefile.common"
    (loop for module-sym being each hash-key of *module-hash-table* 
          using (hash-value protocol-list)
          for dir = (module-path module-sym)
          do
          (insert (symbol-name module-sym))
          (insert "PROTOCOLS =")
          (loop for obj in protocol-list
                when (and (protocol-p obj)
                          (not (the-CREATABLE-protocol-p obj)))
                do
                (insert " ")
                (insert (protocol-name obj)))
          (insert "\n\n"))
    (insert "MODULES =")
    (loop for module-sym being each hash-key of *module-hash-table*
          do
          (insert " ")
          (insert (symbol-name module-sym)))
    (insert "\n"))
  (loop for module-sym being each hash-key of *module-hash-table*
        using (hash-value protocol-list)
        for dir = (module-path module-sym)
        do
        (ensure-directory dir)
        (with-temp-file (concat dir "Makefile")
          (insert "include ../../Makefile.common\n")
          (insert "modulePROTOCOLS = $(")
          (insert (symbol-name module-sym))
          (insert "PROTOCOLS")
          (insert ")\n")
          (insert "include ../Makefile.rules\n"))))

(defun java-print-classes ()
  (interactive)
  (ensure-directory *java-path*)
  (java-print-makefiles)
  (loop for protocol being each hash-value of *protocol-hash-table* 
        unless (the-CREATABLE-protocol-p protocol)
        do
        (setq *last-protocol* protocol)
        (java-print-interface protocol)
        (java-print-class protocol)
        (java-print-native-class protocol)))

(defun java-run-all ()
  (load-and-process-modules :uniquify-method-lists t)
  (java-print-classes))



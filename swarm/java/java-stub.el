(require 'cl)
(eval-and-compile
 (push (getenv "BUILDDIR") load-path))
(require 'protocol)

(defvar *last-protocol*)

(defun freakyp (java-type)
  (eq java-type 'freaky))

(defvar *dollar-sign*)

(defconst *objc-to-java-type-alist*
    '(("id .*" . "Object")
      ("SEL" . "swarm.Selector")
      ("void" . "void")
      ("const char \\*" . "String")
      
      ("char \\*" . "String")
      ("char" . "char")
      ("unsigned char" . "byte")
      ("int" . "int")
      ("short" . "short")
      ("unsigned short" . "short")
      ("long" . "long")
      ("unsigned long" . "long")
      ("double" . "double")
      ("float" . "float")
      ("unsigned" . "int")
      ("BOOL" . "boolean")
      ("timeval_t" . "long")
      ("size_t" . "long")
      ("Color" . "byte")
      ("Class" . "Class")

      ("void \\*" . freaky)
      ("ref_t" . freaky)
      ("val_t" . freaky)
      ("Protocol \\*" . freaky)

      ("notify_t" . freaky)
      ("unsigned long long int" . freaky)
      ("unsigned long long" . freaky)
      ("long long" . freaky)
      
      ("const char \\* const \\*" . freaky)
      ("int \\*" . freaky)
      ("double \\*" . freaky)
      ("BOOL \\*" . freaky)
      ("Class" . freaky)
      ("FILE \\*" . freaky)
      ("compare_t" . freaky)
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

(defconst *removed-protocols* '("CREATABLE"
                                "InputStream"
                                "ArchiverKeyword"
                                "ArchiverArray"
                                "ArchiverValue"
                                "ArchiverPair"
                                "OutputStream"
                                "VarProbe"
                                "HDF5"
                                "HDF5CompoundType"
                                "InFile"
                                "Arguments"
                                ))

(defconst *removed-methods* 
    '("-getClass" ; conflict with Java
      "-getDisplayName" ; conflict with Java
      "-getTypeName" ; conflict with Java
      "-setDisplayName:" ; conflict with Java
      "-copy:" ; conflict with Java
      "-remove:" ; conflict with Java

      ;; fooey
      "+customizeBegin:"
      "-customizeCopy:"
      "-customizeEnd"

      ;; DefinedClass
      "+getMethodFor:" ; IMP return

      ;; CreatedClass
      "-at:addMethod:" ; IMP parameter
      
      ;; Object_s
      "-addRef:withArgument:" ; void* parameter, ref_t return
      "-removeRef:" ; ref_t parameter
      "+conformsTo:" ; Protocol* parameter

      ;; Zone
      "-alloc:" ; void* return
      "-allocBlock:" ; void* return
      "-free:" ; void* parameter
      "-freeBlock:blockSize:"; void* parameter
      "-containsAlloc:" ; void* parameter
      "-getMemberBlock" ; void* return
      "-getData" ; void* return

      ;; FCall
      "-setJavaMethod:inObject:" ; void* parameter
      "-getRetVal:buf:"; retval_t return

      ;; FArguments
      "-addArgument:ofObjCType:" ; void* parameter
      "-addLongLong:" ; long long parameter
      "-addUnsignedLongLong:" ; unsigned long long parameter

      ;; FCall, FArguments
      "-getResult"; void* return

      ;; Array
      "-getData"; void* return
      "+create:setMemberBlock:setCount:" ; id* parameter
      "-setMemberBlock:setCount:" ; id* parameter
      "-getMemberBlock" ; void* return

      ;; Map, passthru in MultiVarProbeWidget
      "-setCompareFunction:"; compare_t parameter
      "-getCompareFunction"; compare_t return
      "-next:" ; id* parameter
      "-prev:" ; id* parameter
      "-get:" ; id* parameter

      ;; Schedule
      ;;  (func_t parameter)
      "-at:createActionCall:"
      "-at:createActionCall::"
      "-at:createActionCall:::"
      "-at:createActionCall::::"

      ;; ActionCreatingCall (func_t parameters)
      "-createActionCall:"
      "-createActionCall::"
      "-createActionCall:::"
      "-createActionCall::::"

      ;; ActionCall / FCall
      "-setFunctionPointer:" ; func_t parameter

      ;; ActionCall
      "-getFunctionPointer" ; func_t return

      ;; Probe
      "-probeRaw:" ; void* return
      "-probeAsPointer:" ; void* return
      "-setData:To:" ; void* parameter

      ;; MessageProbe
      "-getArg:" ; val_t return
      "-dynamicCallOn:" ; val_t return

      ;; random
      "-putStateInto:" ; void* parameter
      "-setStateFrom:" ; void* parameter
      "+create:setStateFromSeeds:"; unsigned* parameter
      "+create:setA:setv:setw:setStateFromSeeds:" ; unsigned * parameter
      "-setStateFromSeeds:" ; unsigned* parameter
      "-getMaxSeedValues" ; unsigned* return
      "-getLongDoubleSample" ; long double return
      "-getLongDoubleSample:" ; long double return
      "-jumpGenerator:toSegment:" ; unsigned long long parameter
      "-jumpAllToSegment:" ; unsigned long long parameter
      "-getCurrentCount" ; unsigned long long return
      "-getCurrentSegment" ; unsigned long long return
      "-getCurrentCount:" ; unsigned long long return
      "-getCurrentSegment:" ; unsigned long long return
      "-getInitialSeeds" ; unsigned* return

      ;; InputWidget
      "-linkVariableBoolean:" ; BOOL* parameter
      "-linkVariableInt:" ; int* parameter
      "-linkVariableDouble:" ; double* parameter

      ;; Form
      "-addLineName:Boolean:" ; BOOL* parameter
      "-addLineName:Int:" ; int* parameter
      "-addLineName:Double:" ; double* parameter

      ;; Histogram
      "-setLabels:count:" ; const char * const * parameter
      "-setColors:count:" ; const char * const * parameter
      "-drawHistogramWithInt:" ; int * parameter
      "-drawHistogramWithInt:atLocations:" ; int*, double* parameters
      "-drawHistogramWithDouble:" ; double * parameter
      "-drawHistogramWithDouble:atLocations:" ; double * parameter

      ;; Colormap
      "-map" ; PixelValue * return
      "-black" ; PixelValue return
      "-white" ; PixelValue return

      ;; EZBin
      "-getDistribution" ; int* return

      ;; Discrete2d
      "-getOffsets" ; long* return
      "-allocLattice" ; id* return
      "-setLattice:" ; id* parameter
      "-getLattice"; id* return

      ;; EZDistribution
      "-getProbabilities"; double* return

      ;; DblBuffer2d
      "-getNewLattice" ; id* return

      ;; QSort
      "+sortNumbersIn:using:" ; function pointer parameter

      ;; ActionTo
      "-getMessageSelector"
      ))

(defun method-in-protocol-p (protocol method)
  (loop for sig in (protocol-method-list protocol)
        if (eq method sig) return 't
        finally return nil))

(defun java-print-javadoc-method (method protocol)
  (insert "\n/**\n * ")
  (loop for text in (method-description-list method)
        do
        (insert text))
  (if (not (method-in-protocol-p protocol method))
      (insert "\n * @hide")
    (message "Supress javadoc `@hide' for `%s' in `%s'"
             (get-method-signature method)
             (protocol-name protocol)))
  (if (not (eq (length (method-deprecated-list method)) 0))
      (progn (insert "\n * @deprecated ")
             (loop for text in (method-deprecated-list method)
                   do
                   (insert text))
             (message "`%s' in `%s' is `@deprecated'"
                      (get-method-signature method) (protocol-name protocol))))
  (insert "\n */\n"))  

(defun java-print-javadoc-protocol (protocol)
  (insert "\n/**\n * ")
  (insert "<strong>")
  (insert (protocol-summary protocol))
  (insert "</strong>.\n\n")
  (loop for text in (protocol-description-list protocol)
        do
        (insert text))
  (if (not (eq (length (protocol-deprecated-list protocol)) 0))
      (progn (insert "\n * @deprecated ")
             (loop for text in (protocol-deprecated-list protocol)
                   do
                   (insert text))
             (message "`%s' protocol is `@deprecated'"
                      (protocol-name protocol))))
  (insert "\n */\n"))  

(defun freaky-message (objc-type)
  (error "Objective C type `%s' in protocol `%s' is freaky!"
         objc-type
         (protocol-name *last-protocol*)))

(defun freaky-message- (objc-type)
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
        (progn
          (freaky-message objc-type)
          "Object")
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

(defun java-print-method-name (arguments native-flag)
  (insert (car (first arguments)))
  (loop for argument in (cdr arguments)
        for nameKey = (car argument)
        when nameKey
        do
        (if native-flag
            (insert *dollar-sign*)
            (insert "$"))
        (insert nameKey)))

(defun java-print-method (protocol method)
  (let* ((arguments (method-arguments method))
         (first-argument (car arguments))
         (signature (get-method-signature method))
         (module (protocol-module protocol)))
    (if nil ; sadly, no
        (cond ((string= signature "+createBegin:")
               (insert (java-qualified-class-name module protocol :creating)))
              ((or (string= signature "-createEnd")
                   (create-method-p method))
               (insert (java-qualified-interface-name module protocol :using)))
              (t (java-print-type (method-return-type method))))
        (java-print-type (method-return-type method)))
    (insert " ")
    (java-print-method-name arguments nil)
    (insert " (")
    (java-print-argument first-argument)
    (loop for argument in (cdr arguments)
          do
          (insert ", ")
          (java-print-argument argument))
    (insert ");\n")))
  
(defun removed-method-p (method)
  (or (find (get-method-signature method) *removed-methods* :test #'string=)
      (method-ellipsis-p method)))

(defun method-ellipsis-p (method)
  (let ((arguments (method-arguments method)))
    (loop for argument in arguments
          when (and (null (first argument))
                    (null (second argument))
                    (string= (third argument) "..."))
          return t
          finally return nil)))

(defun create-method-p (method)
  (let* ((signature (get-method-signature method))
         (len (length signature))
         (min-len (min len 7)))
    (string= (substring signature 0 min-len) "+create")))

(defun included-method-p (protocol method phase)
  (and (not (removed-method-p method))
       (eq phase (method-phase method))
       (if (and (not (creatable-p protocol))
                (create-method-p method))
           (progn
             (message "Skipping method `%s' in non-creatable protocol `%s'"
                      (get-method-signature method)
                      (protocol-name protocol))
             nil)
         t)))

(defun expanded-method-list (protocol phase)
  (remove-if-not #'(lambda (method) (included-method-p protocol method phase))
                 (mapcar #'caddr 
                         (protocol-expanded-methodinfo-list protocol))))

(defun java-print-class-methods-in-phase (protocol phase)
  (loop for method in (expanded-method-list protocol phase) 
	do
        (java-print-javadoc-method method protocol)
        (insert "public native ")
        (java-print-method protocol method)))

(defun java-print-interface-methods-in-phase (protocol phase)
  (loop for method in (protocol-method-list protocol)
        when (included-method-p protocol method phase)
	do (progn (java-print-javadoc-method method protocol)
                  (java-print-method protocol method))))

(defun java-suffix-for-phase (phase)
  (case phase
    (:setting "S")
    (:using "U")
    (:creating "")))

(defun java-protocol-name (protocol phase)
  (concat (protocol-name protocol) (java-suffix-for-phase phase)))

(defun java-class-name (protocol phase)
  (concat (java-interface-name protocol phase) "Impl"))

(defun java-interface-name (protocol phase)
  (java-protocol-name protocol phase))

(defun java-name (protocol phase interface-flag)
  (if interface-flag
      (java-interface-name protocol phase)
      (java-class-name protocol phase)))

(defun java-qualified-name (current-module protocol phase interface-flag)
  (let ((module-name (module-name (protocol-module protocol)))
        (name (java-name protocol phase interface-flag)))
    (if current-module
        (if (string= module-name (module-name current-module))
            name
            (concat "swarm." module-name "." name))
        (concat "swarm/" module-name "/" name))))

(defun java-qualified-native-name (protocol phase interface-flag)
  (java-qualified-name nil protocol phase interface-flag))
  
(defun java-qualified-class-name (current-module protocol phase)
  (java-qualified-name current-module protocol phase nil))

(defun java-qualified-interface-name (current-module protocol phase)
  (java-qualified-name current-module protocol phase t))

(defun java-qualified-native-class-name (protocol phase)
  (java-qualified-class-name nil protocol phase))

(defun java-print-implemented-interfaces-list (protocol phase separator)
  (let ((first t)
        (module (protocol-module protocol)))
    (loop for iprotocol in (included-protocol-list protocol)
          do
          (if first
              (setq first nil)
              (insert separator))
          (insert (java-qualified-interface-name module iprotocol :setting))
          (unless (eq phase :setting)
            (insert separator)
            (insert (java-qualified-interface-name module iprotocol phase))))
    (not first)))

(defun removed-protocol-p (protocol)
  (find (protocol-name protocol) *removed-protocols* :test #'string=))

(defun included-protocol-list (protocol)
  (remove-if #'removed-protocol-p 
             (protocol-included-protocol-list protocol)))

(defun the-CREATABLE-protocol-p (protocol)
  (string= (protocol-name protocol) "CREATABLE"))

(defun creatable-p (protocol)
  (member-if #'the-CREATABLE-protocol-p
             (protocol-included-protocol-list protocol)))

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
  (java-print-javadoc-protocol protocol)
  (insert "public ")
  (unless (creatable-p protocol)
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
  (java-print-javadoc-protocol protocol)
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

(defun java-path (&optional subpath)
  (concat (get-builddir) "swarm/" subpath))

(defun c-path (&optional subpath)
  (concat (get-builddir) "c/" subpath))

(defun module-path (module-sym)
  (java-path (concat (symbol-name module-sym) "/")))

(defmacro with-protocol-java-file (protocol phase interface &rest body)
  (let ((dir (make-symbol "dir")))
    `(let ((,dir (module-path (module-sym (protocol-module ,protocol)))))
      (ensure-directory ,dir)
      (with-temp-file (concat ,dir
                              (java-name ,protocol ,phase ,interface)
                              ".java")
        ,@body))))

(defmacro with-protocol-c-file (protocol &rest body)
  `(progn
    (ensure-directory (c-path))
    (with-temp-file (c-path (concat (protocol-name ,protocol) ".m"))
      ,@body)))

(defun java-print-package (protocol)
  (insert "package swarm.")
  (insert (module-name (protocol-module protocol)))
  (insert ";\n"))
      
(defun java-print-class-phase-to-file (protocol phase)
  (with-protocol-java-file protocol phase nil
                           (java-print-package protocol)
                           (java-print-class-phase protocol phase)))

(defun java-print-class (protocol)
  (loop for phase in '(:creating :using)
        do (java-print-class-phase-to-file protocol phase)))

(defun java-print-interface-phase-to-file (protocol phase)
  (with-protocol-java-file protocol phase t
                           (java-print-package protocol)
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
        ((string= "Class" type) "Ljava_lang_Class_2")
        ((string= "Object" type) "Ljava_lang_Object_2")
        (t (mangle-signature (concat "L" type "_2")))))

(defun java-type-to-native-type (type)
  (cond ((string= "byte" type) "jbyte")
        ((string= "char" type) "jchar")
        ((string= "short" type) "jshort")
        ((string= "int" type) "jint")
        ((string= "long" type) "jlong")
        ((string= "boolean" type) "jboolean")
        ((string= "float" type) "jfloat")
        ((string= "double" type) "jdouble")
        ((string= "String" type) "jstring")
        ((string= "Class" type) "jclass")
        ((string= "void" type) "void")
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

(defun java-argument-print-conversion (argument string-pos)
  (let ((type (cadr argument))
        (jni-type (java-argument-convert argument #'java-type-to-native-type))
        (argname (third argument)))
    (if jni-type
        (cond ((string= type "SEL")
               (insert "JFINDOBJCMETHOD (env, ")
               (insert argname)
               (insert ")"))
              ((string= jni-type "jstring")
               (insert "strings[")
               (insert (format "%d" string-pos))
               (insert "]"))
              ((string= jni-type "jclass")
               (insert "JFINDOBJCCLASS (env, ")
               (insert argname)
               (insert ")"))
              ((string= jni-type "jobject")
               (insert "JFINDOBJC (env, ")
               (insert argname)
               (insert ")"))
              ((or (string= jni-type "jint")
                   (string= jni-type "jdouble")
                   (string= jni-type "jfloat")
                   (string= jni-type "jbyte")
                   (string= jni-type "jlong")
                   (string= jni-type "jchar")
                   (string= jni-type "jboolean")
                   (string= jni-type "jshort"))
               (insert argname))
              (t (error "unkown type %s/%s" type jni-type)))
        (error "unknown type for argument `%s'" argument))))

(defun java-argument-string-p (argument)
  (string= (java-argument-convert argument #'java-type-to-native-type)
           "jstring"))

(defun java-print-method-invocation (protocol method)
  (insert "[")
  (if (method-factory-flag method)
      (insert (protocol-name protocol))
      (insert "JFINDOBJC (env, jobj)"))
  (insert " ")
  (let ((arguments (method-arguments method))
        (string-pos 0))
    (insert (first (car arguments)))
    (unless (java-argument-empty-p (car arguments))
      (insert ": ")
      (java-argument-print-conversion (car arguments) string-pos)
      (when (java-argument-string-p (car arguments))
        (incf string-pos))
      (loop for argument in (cdr arguments)
            for key = (first argument)
            when key do 
            (insert " ")
            (insert key)
            end
            do
            (insert ": ")
            (java-argument-print-conversion argument string-pos)
            (when (java-argument-string-p argument)
              (incf string-pos)))))
  (insert "]"))

(defun java-print-native-method-name (arguments)
  (insert (car (car arguments)))
  (loop for argument in (cdr arguments)
        for nameKey = (car argument)
        when nameKey
        do
        (insert *dollar-sign*) 
        (insert nameKey)))

(defun java-print-native-method (method protocol phase)
  (flet ((insert-arg (arg)
           (insert-char ?\  30)
           (insert arg)))
    (let* ((arguments (method-arguments method))
           (first-argument (car arguments))
           (strings nil)
           (ret-type 
            (java-type-to-native-type (java-objc-to-java-type 
                                       (method-return-type method)))))
      (insert ret-type)
      (insert "\n")
      (insert "Java_swarm_")
      (insert (module-name (protocol-module protocol)))
      (insert "_")
      (insert (java-class-name protocol phase))
      (insert "_")
      (java-print-method-name arguments t)
      (unless (java-argument-empty-p first-argument)
        (insert "__")
        (insert (java-argument-convert first-argument
                                       #'java-type-to-signature))
        (loop for argument in (cdr arguments)
              do
              (insert (java-argument-convert argument
                                             #'java-type-to-signature))))
      (insert " (JNIEnv *env, ")
      (insert "jobject jobj")
      (unless (java-argument-empty-p (car arguments))
        (loop for argument in arguments
              do
              (insert ",\n")
              (let ((native-type (java-argument-convert argument #'java-type-to-native-type)))
                (insert-arg native-type)
                (when (string= native-type "jstring")
                  (push (third argument) strings)))
              (insert " ")
              (insert (third argument))))
      (insert ")\n")
      (insert "{\n")
      (if strings
          (progn
            (insert "  const char *strings[] = { COPYSTRING (env, ")
            (insert (first strings))
            (insert ")")
            (loop for string in (cdr strings)
                  do
                  (insert ",\n")
                  (insert "                        COPYSTRING (env, ")
                  (insert string)
              (insert ")"))
            (if (cdr strings) (insert "\n  ") (insert " "))
            (insert "};\n")
            (insert "  ")
            (unless (string= ret-type "void")
              (insert ret-type)
              (insert " ret = ")))
          (insert "  return "))
      (let* ((signature (get-method-signature method))
             (java-return (java-objc-to-java-type (method-return-type method)))
             (wrapped-flag 
              (cond ((string= "+createBegin:" signature)
                     (insert "JUPDATE (env, jobj, ")
                     t)
                    ((create-method-p method)
                     (insert "JUPDATE (env, JINSTANTIATEUSING (env, jobj), ")
                     t)
                    ((string= "-createEnd" signature)
                     (insert "JSWITCHUPDATE (env, jobj, JINSTANTIATEUSING (env, jobj), ")
                     t)
                    ((string= java-return "Class")
                     (insert "(jclass) JFINDJAVA (")
                     t)
                    ((string= java-return "Object")
                     (insert "JFINDJAVA (")
                     t)
                    ((string= java-return "String")
                     (insert "(*env)->NewStringUTF (env, ")
                     t)
                    (t nil))))
        
        (java-print-method-invocation protocol method)
        (when wrapped-flag (insert ")")))
      (insert ";\n")
      (when strings
        (insert "  CLEANUPSTRINGS (env, strings);\n")
        (unless (string= ret-type "void")
          (insert "  return ret;\n")))
      (insert "}\n"))))

(defun java-print-native-class (protocol)
  (with-protocol-c-file protocol
    (insert "#include \"directory.h\"\n")
    (insert "#import <defobj.h>\n")
    (let ((module-name (module-name (protocol-module protocol))))
      (unless (string= module-name "defobj")
        (insert "#import <")
        (insert module-name)
        (insert ".h>\n")
        (insert "\n")))
    (loop for phase in '(:creating :using)
          do
          (loop for method in (expanded-method-list protocol phase)
                do
                (java-print-native-method method protocol phase)
                (insert "\n")))))

(defun java-print-makefiles ()
  (ensure-directory (c-path))
  (with-temp-file (concat (get-builddir) "Makefile.common")
    (loop for module-sym being each hash-key of *module-hash-table* 
          using (hash-value protocol-list)
          for dir = (module-path module-sym)
          do
          (insert (symbol-name module-sym))
          (insert "PROTOCOLS =")
          (loop for obj in protocol-list
                when (and (protocol-p obj)
                          (not (removed-protocol-p obj)))
                do
                (insert " ")
                (insert (protocol-name obj)))
          (insert "\n\n"))
    (insert "MODULES = defobj collections activity objectbase random gui simtoolsgui simtools analysis space\n")
    ;;(loop for module-sym being each hash-key of *module-hash-table*
    ;;      do
    ;;      (insert " ")
    ;;      (insert (symbol-name module-sym)))
    ;; (insert "\n")
    )
  (loop for module-sym being each hash-key of *module-hash-table*
        using (hash-value protocol-list)
        for dir = (module-path module-sym)
        do
        (ensure-directory dir)
        (with-temp-file (concat dir "Makefile")
          (insert "include ../../Makefile.common\n")
          (insert "module = ")
          (insert (symbol-name module-sym))
          (insert "\n")
          (insert "include ../Makefile.rules\n"))))

(defun java-print-javadoc-module-summary ()
  (loop for module-sym being each hash-key of *module-hash-table*
        using (hash-value protocol-list)
        for dir = (module-path module-sym)
        do
        (ensure-directory dir)
        (with-temp-file (concat dir "package.html")
          (insert "<body><strong>")
          (insert (module-summary (lookup-module module-sym)))
          (insert "</strong>.\n\n")
          (loop for text in (module-description-list 
                             (lookup-module module-sym))
                do
                (insert text))
          (insert "</body>")
          )))

(defun java-print-classes ()
  (interactive)
  (ensure-directory (java-path))
  (java-print-makefiles)
  (java-print-javadoc-module-summary)  
  (loop for protocol being each hash-value of *protocol-hash-table* 
        unless (removed-protocol-p protocol)
        do
        (setq *last-protocol* protocol)
        (java-print-interface protocol)
        (java-print-class protocol)
        (java-print-native-class protocol)))

(defun set-dollar-sign (unicode-flag)
  (setq *dollar-sign* (if unicode-flag "_00024" "$")))

(defun java-run-all (&key unicode)
  (set-dollar-sign unicode)
  (load-and-process-modules :uniquify-method-lists t)
  (java-print-classes))

(defun java-run-all-javadoc ()
  (set-dollar-sign t)
  (load-and-process-modules :uniquify-method-lists t)
  (ensure-directory (java-path))
  (java-print-javadoc-module-summary)
  (loop for protocol being each hash-value of *protocol-hash-table* 
        unless (removed-protocol-p protocol)
        do
        (setq *last-protocol* protocol)
        (java-print-interface protocol)
        (java-print-class protocol)))

(defun java-run-all-unicode ()
  (java-run-all :unicode t))

(defun java-run-all-literal ()
  (java-run-all :unicode nil))
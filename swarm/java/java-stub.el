;; Copyright © 1999, 2000 Swarm Development Group
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA
;; 
;; The Swarm Development Group can be reached via our website at:
;; http://www.swarm.org/

(require 'cl)
(eval-and-compile
 (push (getenv "TOP_BUILDDIR") load-path))
(require 'protocol)
(require 'interface)

(defvar *dollar-sign*)

(defconst *objc-to-java-type-alist*
    '(("id +<\\(.*\\)>" . protocol)
      ("id .*" . "Object")
      ("SEL" . "swarm.Selector")
      ("void" . "void")
      ("const char \\*" . "java.lang.String")
      ("char \\*" . "java.lang.String")

      ("const char \\*\\*" . "java.lang.String[]")
      ("const char \\* const \\*" . "java.lang.String[]")
      
      ("char" . "char")
      ("unsigned char" . "byte")
      ("int" . "int")
      ("short" . "short")
      ("unsigned short" . "short")
      ("double" . "double")
      ("float" . "float")
      ("unsigned" . "int")
      ("BOOL" . "boolean")

      ("long" . "int")
      ("unsigned long" . "int")
      ("timeval_t" . "int")
      ("size_t" . "int")

      ("Class" . "Class")

      ("unsigned long long int" . "long")
      ("unsigned long long" . "long")
      ("long long" . "long")

      ("Color" . "byte")

      ("JOBJECT" . "Object")
      ("COMOBJECT" . freaky)

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

(defun method-in-protocol-p (protocol method)
  (loop for sig in (protocol-method-list protocol)
        if (eq method sig) return 't
        finally return nil))

(defun java-print-deprecated-doc (object)
  (insert "\n * @deprecated ")
  (loop for text in (generic-deprecated-list object)
        do
        (insert text)))

(defun java-print-javadoc-default-constructor ()
  (insert "\n/**\n * Default constructor for Impl class\n */\n"))

(defun java-print-javadoc-method (method protocol)
  (insert "\n/**\n * ")
  (loop for text in (method-description-list method)
        do
        (insert text))
  (when (deprecated-p method)
    (java-print-deprecated-doc method))
  (insert "\n */\n"))

(defun java-print-javadoc-protocol (protocol &optional is-class)
  (insert "\n/**\n * ")
  (insert "<strong>")
  (insert (protocol-summary protocol))
  (insert "</strong>.\n\n")
  (loop for text in (protocol-description-list protocol)
        do
        (insert text))
  (if (deprecated-p protocol)
      (java-print-deprecated-doc protocol))
  (if (and is-class (returnable-p protocol))
      (insert "\n * @hide"))
  (insert "\n */\n"))

(defun java-objc-to-java-type-category (objc-type)
  (if objc-type
      (cdr (find objc-type *objc-to-java-type-alist*
                 :key #'car
                 :test #'(lambda (a b)
                           (let ((expr (concat (concat "^" b) "$")))
                             (string-match expr a)))))
      "Object"))

(defun java-type-category-to-java-type (current-module
                                        objc-type 
                                        java-type-category)
  (cond ((eq java-type-category 'freaky)
         (freaky-message objc-type)
         java-type-category)
        ((eq java-type-category 'protocol)
         (let* ((protocol-name (match-string 1 objc-type))
                (protocol (lookup-protocol protocol-name)))
           (java-qualified-name current-module protocol :using t)))
        (t java-type-category)))

(defun java-objc-to-java-type (current-module objc-type)
  (java-type-category-to-java-type current-module
                                   objc-type
                                   (java-objc-to-java-type-category objc-type)))

(defun java-print-type (current-module objc-type)
  (let ((java-type (java-objc-to-java-type current-module objc-type)))
    (unless java-type
      (error "No Java type for `%s'" objc-type))
    (if (eq java-type 'freaky)
        (progn
          (freaky-message objc-type)
          (insert "Object"))
      (insert java-type))))

(defun java-print-argument (current-module argument)
  (let* ((type-and-varname (cdr argument))
         (varname (cadr type-and-varname)))
    ;; the case of method with no arguments
    (when varname
      (java-print-type current-module (car type-and-varname))
      (insert " ")
      (insert
       (if (string= varname "class")
           "aClass"
           varname)))))

(defun java-print-method-name (arguments native-flag)
  (let ((nameKey (argument-key (first arguments))))
    (cond ((string= nameKey "initSwarmUsing")
           (insert
            (if native-flag
                "_initSwarm_"
              "initSwarm")))
          ((or (string= nameKey "createArchivedProbeDisplay")
               (string= nameKey "setWindowGeometryRecordName"))
           (insert nameKey))
          (t
           (insert nameKey)
           (loop for argument in (cdr arguments)
                 for nameKey = (car argument)
                 when nameKey
                 do
                 (if native-flag
                     (insert *dollar-sign*)
                   (insert "$"))
                 (insert nameKey))))))

(defun java-print-method (protocol method)
  (let* ((arguments (method-arguments method))
         (first-argument (first arguments))
         (signature (get-method-signature method))
         (module (protocol-module protocol)))
    (java-print-type module (method-return-type method))
    (insert " ")
    (java-print-method-name arguments nil)
    (insert " (")
    (java-print-argument module first-argument)
    (loop for argument in (cdr arguments)
          do
          (insert ", ")
          (java-print-argument module argument))
    (insert ");\n")))

(defun java-print-class-methods-in-phase (protocol phase)
  (loop for method in (expanded-method-list protocol phase) 
	do
        (java-print-javadoc-method method protocol)
        (insert "public native ")
        (java-print-method protocol method)))

(defun java-print-interface-methods-in-phase (protocol phase)
  (loop for method in (protocol-method-list protocol)
        when (included-method-p protocol method phase)
	do
        (java-print-javadoc-method method protocol)
        (java-print-method protocol method)))

(defun java-protocol-name (protocol phase)
  (concat (protocol-name protocol) (suffix-for-phase phase)))

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
        (if (and (not (eq current-module t))
                 (string= module-name (module-name current-module)))
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

(defun java-print-implemented-interfaces-list (protocol phase)
  (print-implemented-interfaces-list protocol phase
                                     #'java-qualified-interface-name
                                     nil))

(defun java-print-implemented-protocols (protocol phase interface)
  (if interface
      (when (included-protocol-list protocol)
        (insert "extends")
        (insert " ")
        (java-print-implemented-interfaces-list protocol phase))
      (progn
        (insert "implements")
        (insert " ")
        (java-print-implemented-interfaces-list protocol phase))))

(defun java-print-class-constructor-method (protocol method)
  (let* ((arguments (method-arguments method))
         (module (protocol-module protocol))
         (zone-protocol (lookup-protocol "Zone"))
         (zone-class-name
          (java-qualified-interface-name module zone-protocol :using))
         (creating-class-name (java-class-name protocol :creating))
         (using-class-name (java-class-name protocol :using)))

    ;; if constructor accepts more than one argument, then print it's
    ;; method documentation, otherwise default to boilerplate
    ;; documentation
    (if arguments
        (java-print-javadoc-method method protocol)
        (java-print-javadoc-default-constructor))
    (insert "public ")
    (insert using-class-name)
    (insert " (")
    (java-print-argument module (first arguments))
    (loop for argument in (cdr arguments)
          do
          (insert ", ")
          (java-print-argument module argument))
    (insert ") { super (); ")
    (insert "new ")
    (insert creating-class-name)
    (insert " (this).")
    (insert (argument-key (first arguments)))
    (loop for argument in (cdr arguments)
          do
          (insert "$")
          (insert (argument-key argument)))
    (insert " (")
    (insert (argument-name (first arguments)))
    (loop for argument in (cdr arguments)
          do
          (insert ", ")
          (insert (argument-name argument)))
    (insert "); }\n")))

(defun java-print-basic-constructor (protocol)
  (insert "public ")
  (insert (java-class-name protocol :using))
  (insert " () {\n")
  (insert "  super ();\n")

  (loop for method in (expanded-method-list protocol :getters)
        do
        (let ((ret-type (method-return-type method)))
          (when (eq (java-objc-to-java-type-category ret-type) 'protocol)
            (insert (get-variable-name-for-getter-method method))
            (insert " = new ")
            (insert (java-objc-to-java-type t ret-type))
            (insert "Impl ();\n"))))
  (insert "}\n"))

(defun java-print-convenience-constructors (protocol)
  (loop for method in (collect-convenience-create-methods protocol)
        do (java-print-class-constructor-method protocol method)))

(defun java-print-nextphase-class-constructor (protocol)
  (insert "public ")
  (insert (java-class-name protocol :creating))
  (insert " (")
  (insert (java-interface-name protocol :using))
  (insert " nextPhase) { super (); this.nextPhase = nextPhase; }\n")
  (insert "public ")
  (insert (java-class-name protocol :creating))
  (insert " () {super ();}\n"))

(defun inheritance-cases-disabled (pname)
  (cond ((string= pname "GUISwarm")
	 (insert " extends swarm.objectbase.SwarmImpl"))
	((string= pname "ZoomRaster")
	 (insert " extends swarm.gui.RasterImpl"))
	((or (string= pname "DblBuffer2d")
	     (string= pname "Grid2d"))
	 (insert " extends swarm.space.Discrete2dImpl"))
	((or (string= pname "ConwayLife2d")
	     (string= pname "Diffuse2d"))
	 (insert " extends swarm.space.DblBuffer2dImpl")) ;; Ca2d is abstract
	((string= pname "ScheduleActivity")
	 (insert " extends swarm.activity.ActivityImpl"))
	((string= pname "SwarmActivity")
	 (insert " extends swarm.activity.ScheduleActivityImpl"))))

(defun inheritance-cases (pname) (insert " extends swarm.BaseImpl"))

(defun java-print-class-getter-variables (protocol)
  (loop for method in (expanded-method-list protocol :getters)
        do
        (insert "public ") 
        (insert (java-objc-to-java-type t (method-return-type method)))
        (insert " ")
        (insert (get-variable-name-for-getter-method method))
        (insert ";\n"))
  (insert "\n"))

(defun java-print-class-phase (protocol phase)
  (java-print-javadoc-protocol protocol t)
  (insert "public ")
  (unless (real-class-p protocol)
    (insert "abstract "))
  (insert "class ")
  (insert (java-class-name protocol phase))
  (let ((pname (protocol-name protocol)))
    (cond ((inclusive-phase-p phase :creating)
           (insert " extends swarm.PhaseCImpl"))
	  (t (inheritance-cases pname))
          ))
  (insert " ")
  (when (java-print-implemented-protocols protocol phase nil)
    (insert ", "))
  (insert (java-qualified-interface-name (protocol-module protocol)
                                         protocol :setting))
  (loop for interface-phase in '(:creating :using)
        when (inclusive-phase-p phase interface-phase)
        do
        (insert ", ")
        (insert (java-qualified-interface-name (protocol-module protocol)
                                               protocol interface-phase)))
  (insert " {\n")
  (when (inclusive-phase-p phase :using)
    (java-print-class-getter-variables protocol))
    
  (java-print-class-methods-in-phase protocol phase)
  (java-print-class-methods-in-phase protocol :setting)
  (cond ((inclusive-phase-p phase :using)
         (java-print-basic-constructor protocol)
         (when (creatable-p protocol)
           (java-print-convenience-constructors protocol)))
        ((inclusive-phase-p phase :creating)
         (when (creatable-p protocol)
           (java-print-nextphase-class-constructor protocol))))
  (insert "}\n"))

(defun java-print-interface-phase (protocol phase)
  (java-print-javadoc-protocol protocol)
  (insert "public interface ")
  (insert (java-interface-name protocol phase))
  (insert " ")
  (java-print-implemented-protocols protocol phase t)
  (insert " {\n")
  (java-print-interface-methods-in-phase protocol phase)
  (insert "}\n"))

(defun java-path (&optional subpath)
  (concat (get-builddir) "swarm/" subpath))

(defun java-module-path (module)
  (java-path (module-path module)))

(defmacro with-protocol-java-file (protocol phase interface &rest body)
  (let ((dir (make-symbol "dir")))
    `(let ((,dir (java-module-path (protocol-module ,protocol))))
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
  (insert "package swarm")
  (let ((module (protocol-module protocol)))
    (unless (eq (module-sym module) 'SwarmTop)
      (insert ".")
      (insert (module-name module))))
  (insert ";\n"))

(defun java-print-imports (protocol)
  (unless (string= (protocol-name protocol) "Zone")
    (insert "import swarm.defobj.")
    (insert (java-class-name (lookup-protocol "Zone") :using))
    (insert ";")))
      
(defun java-print-class-phase-to-file (protocol phase)
  (with-protocol-java-file protocol phase nil
                           (java-print-package protocol)
                           (java-print-imports protocol)
                           (java-print-class-phase protocol phase)))

(defun java-print-class (protocol)
  (loop for phase in '(:creating :using)
        do (java-print-class-phase-to-file protocol phase)))

(defun java-print-interface-phase-to-file (protocol phase)
  (with-protocol-java-file protocol phase t
                           (java-print-package protocol)
                           (java-print-interface-phase protocol phase)))

(defun java-print-interface (protocol)
  (loop for phase in *phases*
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
        ((string= "java.lang.String" type) "Ljava_lang_String_2")
        ((string= "java.lang.String[]" type) "_3Ljava_lang_String_2")
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
        ((string= "java.lang.String" type) "jstring")
        ((string= "java.lang.String[]" type) "jobjectArray")
        ((string= "Class" type) "jclass")
        ((string= "void" type) "void")
        (t "jobject")))

(defun java-argument-convert (argument convert)
  (let* ((type-and-varname (cdr argument))
         (varname (cadr type-and-varname)))
    ;; the case of method with no arguments
    (when varname
      (funcall convert (java-objc-to-java-type t (car type-and-varname))))))

(defun java-argument-print-conversion (current-module argument string-pos)
  (let ((type (argument-type argument))
        (jni-type (java-argument-convert argument
                                         #'java-type-to-native-type))
        (argname (third argument)))
    (if jni-type
        (cond ((string= type "SEL")
               (insert "SD_JAVA_ENSURE_SELECTOR_OBJC (")
               (insert argname)
               (insert ")"))
              ((string= jni-type "jstring")
               (insert "strings[")
               (insert (format "%d" string-pos))
               (insert "]"))
              ((string= jni-type "jclass")
               (insert "SD_JAVA_ENSURE_CLASS_OBJC (")
               (insert argname)
               (insert ")"))
              ((string= type "JOBJECT")
               (insert argname))
              ((string= jni-type "jobject")
               (insert "SD_JAVA_ENSURE_OBJECT_OBJC (")
               (insert argname)
               (insert ")"))
              ((string= jni-type "jobjectArray")
               (insert "JAVA_CONVERT_STRING_ARRAY (")
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

(defun argname-number (num)
  (if (= num -1)
      "swarm_target"
      (concat "swarm_arg" (prin1-to-string num))))

(defun java-print-class-target (protocol)
  (let ((name (protocol-name protocol)))
    
    (insert "  Class swarm_target = objc_lookup_class (\"")
    (insert (if (string= name "Arguments") "Arguments_c" name))
    (insert "\");\n")))

(defun java-print-method-invocation-arguments (protocol method)
  (if (method-factory-flag method)
      (java-print-class-target protocol)
    (progn
      (insert "  id ")
      (insert (argname-number -1))
      (insert " = SD_JAVA_FIND_OBJECT_SWARM_OBJC (jobj);\n")))
  (let ((arguments (method-arguments method))
        (string-pos 0)
        (module (protocol-module protocol)))
    (when (has-arguments-p method)
      (loop for argument in arguments
            for arg-pos from 0
            for arg-type = (argument-type argument)
            when (and (local-ref-p argument)
                      (not (java-argument-string-p argument)))
            do
            (insert "  ")
            (insert (if arg-type arg-type "id"))
            (insert " ")
            (insert (argname-number arg-pos))
            (insert " = ")
            (java-argument-print-conversion module argument string-pos)
            (insert ";\n")
            (when (java-argument-string-p argument)
              (incf string-pos))))))

(defun local-ref-p (argument)
  (let ((type (argument-type argument))
        (jni-type (java-argument-convert argument #'java-type-to-native-type)))
    (or (string= type "SEL")
        (string= jni-type "jobjectArray")
        (string= jni-type "jstring")
        (string= jni-type "jclass")
        (string= jni-type "jobject"))))

(defun java-print-method-invocation-arguments-lref-deletion (protocol method)
  (insert "  (*env)->DeleteLocalRef (env, jobj);\n")
  (when (has-arguments-p method)
    (let ((arguments (method-arguments method)))
      (loop for argument in arguments
            when (local-ref-p argument)
            do
            (insert "  (*env)->DeleteLocalRef (env, ")
            (insert (argument-name argument))
            (insert ");\n")))))

(defun java-argument-ref (argument arg-pos string-pos)
  (cond ((java-argument-string-p argument)
         (concat "strings["
                 (prin1-to-string string-pos)
                 "]"))
        ((local-ref-p argument) (argname-number arg-pos))
        (t (argument-name argument))))

(defun java-print-method-invocation (protocol method dht)
  (let* ((signature (get-method-signature method))
         (funcsym (gethash method dht)))
    (if funcsym
        (progn
          (insert funcsym)
          (insert "(swarm_target, M(")
          (insert (substring signature 1))
          (insert ")"))
      (insert "(*swarm_imp) (swarm_target, swarm_sel"))
    (let ((arguments (method-arguments method))
          (string-pos 0)
          (module (protocol-module protocol)))
      (when (has-arguments-p method)
        (loop for argument in arguments
              for arg-pos from 0
              do
              (insert ", ")
              (insert (java-argument-ref argument arg-pos string-pos))
              (when (java-argument-string-p argument)
                (incf string-pos)))))
    (insert ")")))

(defun java-print-native-method-name (arguments)
  (insert (argument-key (first arguments)))
  (loop for argument in (cdr arguments)
        for nameKey = (argument-key argument)
        when nameKey
        do
        (insert *dollar-sign*) 
        (insert nameKey)))

(defun java-print-native-method (method protocol phase dht)
  (flet ((insert-arg (arg)
                     (insert-char ?\  30)
                     (insert arg)))
    (let* ((arguments (method-arguments method))
           (first-argument (first arguments))
           (strings nil)
           (string-arrays nil)
           (module (protocol-module protocol))
           (ret-type 
            (java-type-to-native-type (java-objc-to-java-type 
                                       module
                                       (method-return-type method)))))
      (insert "JNIEXPORT ")
      (insert ret-type)
      (insert " JNICALL")
      (insert "\n")
      (insert "Java_swarm_")
      (unless (eq (module-sym module) 'SwarmTop)
        (insert (module-name module))
        (insert "_"))
      (insert (java-class-name protocol phase))
      (insert "_")
      (java-print-method-name arguments t)
      (when (has-arguments-p method)
        (insert "__")
        (insert (java-argument-convert first-argument
                                       #'java-type-to-signature))
        (loop for argument in (cdr arguments)
              do
              (insert (java-argument-convert argument
                                             #'java-type-to-signature))))
      (insert " (JNIEnv *env, ")
      (insert "jobject jobj")
      (when (has-arguments-p method)
        (loop for argument in arguments
              for arg-pos from 0
              do
              (insert ",\n")
              (let ((native-type
                     (java-argument-convert argument
                                            #'java-type-to-native-type))
                    (name (argument-name argument)))
                (insert-arg native-type)
                (when (string= native-type "jstring")
                  (push name strings))
                (when (string= native-type "jobjectArray")
                  (push (cons arg-pos name) string-arrays)))
              (insert " ")
              (insert (third argument))))
      (insert ")\n")
      (insert "{\n")
      (unless (string= ret-type "void")
        (when (or strings (create-method-p method))
          (insert "  ")
          (insert ret-type)
          (insert " ret;\n")))
      (when strings
        (let ((rstrings (reverse strings)))
          (insert "  const char *strings[] = { JAVA_COPY_STRING (")
          (insert (first rstrings))
          (insert ")")
          (loop for string in (cdr rstrings)
                do
                (insert ",\n")
                (insert "                        JAVA_COPY_STRING (")
                (insert string)
                (insert ")"))
          (if (cdr rstrings) (insert "\n  ") (insert " "))
          (insert "};\n")))
      (when (convenience-create-method-p protocol method)
        (insert "  jobject nextPhase = SD_JAVA_NEXTPHASE (jobj);\n"))
      
      (let ((signature (get-method-signature method)))
        (if (string= signature "+createBegin")
            (progn
              (insert "  extern id java_swarmEnvironmentCreating;\n")
              (insert "  extern void swarm_java_constructors ();\n")
              (insert "  extern void __objc_exec_class_for_all_initial_modules ();\n")
              (insert "  extern void swarm_force_references ();\n")
              (insert "  extern void swarm_java_predispatch ();\n")
              (insert "  extern void swarm_directory_associate_objects_startup (jobject swarmEnvironment);\n")
              (insert "  jobject nextPhase;\n\n")
              (insert "  jniEnv = env;\n")
	      (insert "  __objc_exec_class_for_all_initial_modules ();\n")
	      (insert "  swarm_force_references ();\n")
              (insert "  swarm_java_predispatch ();\n")
              (insert "  swarm_java_constructors ();\n")
              (insert "  java_create_refs ();\n")
              (insert "  defobj_init_java_call_tables ((void *) env);\n")
              (insert "  java_swarmEnvironmentCreating = [SwarmEnvironment createBegin];\n")
              (insert "  swarmDirectory = [Directory create: globalZone];\n")
              (insert "  ret = SD_JAVA_ADD_OBJECT_JAVA (jobj, java_swarmEnvironmentCreating);\n")
              (insert "  nextPhase = SD_JAVA_NEXTPHASE (jobj);\n")
              (insert "  swarm_directory_java_associate_objects_startup (nextPhase);\n")
              (insert "  (*jniEnv)->DeleteLocalRef (jniEnv, nextPhase);\n")
              (insert "  return ret;\n"))
          
          (progn
            (java-print-method-invocation-arguments protocol method)
            (impl-print-method-setup method dht #'c-objc-type nil t)

            (unless (or (string= ret-type "void")
                        (if (or strings (create-method-p method))
                            (insert "ret = ")
                          (insert "return "))))
            (let* ((objc-return (method-return-type method))
                   (java-type-category
                    (java-objc-to-java-type-category objc-return))
                   (java-return
                    (java-type-category-to-java-type module
                                                     objc-return
                                                       java-type-category))
                   (wrapped-flag 
                    (cond ((or (string= "+createBegin:" signature)
                               (string= "+customizeBegin:" signature))
                           (insert "SD_JAVA_ADD_OBJECT_JAVA (jobj, ")
                           t)
                          ((create-method-p method)
                           (insert "SD_JAVA_ADD_OBJECT_JAVA (nextPhase, ")
                           t)
                          ((or (string= "-createEnd" signature))
                           (insert "SD_JAVA_SWITCHPHASE (jobj, ")
                           t)
                          ((string= java-return "Class")
                           (insert "(jclass) SD_JAVA_FIND_CLASS_JAVA (")
                           t)
                          ((string= objc-return "SEL")
                           (insert "(jclass) SD_JAVA_FIND_SELECTOR_JAVA (")
                           t)
                          ((string= java-return "java.lang.String")
                           (insert "(*env)->NewStringUTF (env, ")
                           t)
                          ((or (string= java-return "Object")
                               (eq java-type-category 'protocol))
                           (insert "SD_JAVA_ENSURE_OBJECT_JAVA (")
                           t)
                          (t nil))))
              (java-print-method-invocation protocol method dht)
              (when wrapped-flag (insert ")")))
            (insert ";\n")
            (when strings
              (insert "  JAVA_CLEANUP_STRINGS (strings);\n"))
            (loop for arg-pair in string-arrays
                  do
                  (insert "  JAVA_CLEANUP_STRING_ARRAY (")
                  (insert (argname-number (car arg-pair)))
                  (insert ", ")
                  (insert (cdr arg-pair))
                  (insert ");\n"))
            (when (convenience-create-method-p protocol method)
              (insert "  (*env)->DeleteLocalRef (env, nextPhase);\n"))
            (unless (string= ret-type "void")
              (when (or strings (create-method-p method))
                (insert "  return ret;\n"))))))
      (insert "}\n"))))

(defun java-print-native-class (protocol)
  (with-protocol-c-file protocol
    (insert "#import \"../../src/defobj/java.h\"\n")
    (insert "#import <defobj.h>\n")
    (when (string= (protocol-name protocol) "Arguments")
      (insert "#import <defobj/Arguments.h>\n"))
    (insert "#import <objectbase.h>\n")
    (let* ((module (protocol-module protocol))
           (module-name (module-name module)))
      (unless (or (string= module-name "defobj")
                  (string= module-name "objectbase"))
        (insert "#import <")
        (insert module-name)
        (insert ".h>\n")
        (insert "\n"))
      (loop for phase in '(:creating :using)
            for dht = (create-dispatch-hash-table protocol phase)
            do
	    (dump-dispatch-hash-table dht protocol phase)
            (loop for method in (expanded-method-list protocol phase)
                  unless (unwanted-create-method-p protocol method)
                  do
                  (java-print-native-method method protocol phase dht)
                  (insert "\n")))
      (loop for method in (expanded-method-list protocol :setting)
            for dht = (create-dispatch-hash-table protocol :setting)
	    do 
	    (dump-dispatch-hash-table dht protocol :setting)
	    (java-print-native-method method protocol :creating dht)
	    (insert "\n")
	    (java-print-native-method method protocol :using dht)))))

(defun java-print-javadoc-module-summary ()
  (loop for module-sym being each hash-key of *module-hash-table*
        using (hash-value protocol-list)
        for module = (lookup-module module-sym)
        for dir = (java-module-path module)
        do
        (ensure-directory dir)
        (with-temp-file (concat dir "package.html")
          (insert "<body><strong>")
          (insert (module-summary module))
          (insert "</strong>.\n\n")
          (loop for text in (module-description-list module)
                do
                (insert text))
          (insert "</body>")
          )))

(defun java-print-classes ()
  (interactive)
  (ensure-directory (java-path))
  (java-print-javadoc-module-summary)  
  (loop for protocol being each hash-value of *protocol-hash-table* 
        unless (removed-protocol-p protocol)
        do
        (setq *last-protocol* protocol)
        (java-print-interface protocol)
        (when (real-class-p protocol)
          (java-print-class protocol)
          (java-print-native-class protocol))))

(defun set-dollar-sign (unicode-flag)
  (setq *dollar-sign* (if unicode-flag "_00024" "$")))

(defun java-print-makefiles ()
  (print-makefile.common)
  (loop for module-sym being each hash-key of *module-hash-table*
        using (hash-value protocol-list)
        unless (eq module-sym 'SwarmTop)
        do
        (let* ((module (lookup-module module-sym))
               (dir (java-module-path module)))
          (ensure-directory dir)
          (with-temp-file (concat dir "Makefile")
            (insert "include ")
            (insert "../../Makefile.common\n")
            (insert "module = ")
            (insert (symbol-name module-sym))
            (insert "\n")
            (insert "include ../Makefile.rules\n")))))

(defun java-init ()
  (setq *extra-removed-methods* '("-setCOMMethod:inObject:")))

(defun java-run-all (&key unicode)
  (java-init)
  (setq *java-flag* t)
  (set-dollar-sign unicode)
  (load-and-process-modules :uniquify-method-lists t)
  (java-print-makefiles)
  (java-print-classes))

(defun java-run-all-javadoc ()
  (interactive)
  (set-dollar-sign t)
  (setq *java-flag* t)
  (load-and-process-modules :uniquify-method-lists t)
  (ensure-directory (java-path))
  (java-print-javadoc-module-summary)
  (loop for protocol being each hash-value of *protocol-hash-table* 
        unless (removed-protocol-p protocol)
        do
        (setq *last-protocol* protocol)
        (java-print-interface protocol)
        (when (real-class-p protocol)
          (java-print-class protocol))))

(defun java-run-all-unicode ()
  (java-run-all :unicode t))

(defun java-run-all-literal ()
  (java-run-all :unicode nil))

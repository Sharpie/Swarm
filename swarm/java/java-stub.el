;; Copyright � 1999 Santa Fe Institute

(require 'cl)
(eval-and-compile
 (push (getenv "TOP_BUILDDIR") load-path))
(require 'protocol)

(defvar *last-protocol*)

(defvar *dollar-sign*)

(defconst *objc-to-java-type-alist*
    '(("id +<\\(.*\\)>" . protocol)
      ("id .*" . "Object")
      ("SEL" . "swarm.Selector")
      ("void" . "void")
      ("const char \\*" . "java.lang.String")
      
      ("char \\*" . "java.lang.String")
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

      ("JOBJECT" . "Object")

      ("Color" . "byte")

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

(defconst *removed-protocols* 
    '(
      "CREATABLE"
      "RETURNABLE"
      
      ;; problematic types
      "Arguments"

      ;; should be done by archiver
      "ArchiverKeyword"
      "ArchiverArray"
      "ArchiverValue"
      "ArchiverPair"
      "ArchiverList"
      "ArchiverQuoted"
      "InputStream"
      "OutputStream"

      "HDF5CompoundType"

      ;; deprecated
      "InFile"
      "OutFile" 
      "AppendFile"
      "ObjectSaver"
      "ObjectLoader"
                                
      ;; weird / broken collections
      "Set"
      "OrderedSet"
      "_Set"

      ;; objectbase non-creatable
      "ProbeConfig"

      ;; activity
      "ActivityIndex"

      ;; gui creatable
      "Frame"
      "Canvas"
      "ProbeCanvas"
      "Label"
      "ClassDisplayLabel"
      "VarProbeLabel"
      "CompleteProbeDisplayLabel"
      "Button"
      "ClassDisplayHideButton"
      "SimpleProbeDisplayHideButton"
      "SuperButton"
      "Entry"
      "MessageProbeEntry"
      "VarProbeEntry"
      "ButtonPanel"
      "Form"
      "CheckButton"
      "CanvasItem"
      "NodeItem"
      "LinkItem"
      "ScheduleItem"
      "OvalNodeItem"
      "RectangleNodeItem"
      "TextItem"
      "Circle"
      "Rectangle"
      "Line"

      "Graph"
      "Histogram"
      "Pixmap"
                                
      ;; gui non-creatable
      "WindowGeometryRecord"
      "GraphElement"
      "InputWidget"
      "CompositeItem"
      "CanvasAbstractItem"
      "Drawer"

      ;; simtoolsgui non-creatable
      "WindowGeometryRecordName"
      "CompositeWindowGeometryRecordName"
      "MessageProbeWidget"
      "MultiVarProbeWidget"

      ;; random generators
      "LCGgen" "LCG1gen" "LCG2gen" "LCG3gen"


      ;; "PMMLCGgen" "PMMLCG1gen" 
      "PMMLCG2gen" "PMMLCG3gen" "PMMLCG4gen"
      "PMMLCG5gen" "PMMLCG6gen" "PMMLCG7gen" "PMMLCG8gen" "PMMLCG9gen"

      "ACGgen"
      "SCGgen"

      "SWBgen" "SWB1gen" "SWB2gen" "SWB3gen"

      "MWCAgen"
      "MWCBgen"
      "RWC2gen"
      "RWC8gen"

      "TGFSRgen" "TT403gen" "TT775gen" "TT800gen"

      "MRGgen" "MRG5gen" "MRG6gen" "MRG7gen"
      "C2MRG3gen"
      "C3MWCgen"

      "Split" "SplitOut" "SplitSingleSeed" "SplitMultiSeed" 
      "SplitRandomGenerator"

      "C2LCGXgen" "C4LCGXgen"
      ))

(defconst *removed-modules* '())

(defconst *removed-methods* 
    '("-getClass" ; conflict with Java
;;      "-getDisplayName" ; conflict with Java
;;      "-getTypeName" ; conflict with Java
;;      "-setDisplayName:" ; conflict with Java
;;      "-copy:" ; conflict with Java
;;      "-remove:" ; conflict with Java

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
      "-getRetVal:buf:"; retval_t return
      "-getCallType"; call_t return

      ;; FArguments
      "-addArgument:ofObjCType:" ; void* parameter
      "-addLongDouble:" ; long double parameter

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

      ;; VarProbe
      "-probeRaw:" ; void* return
      "-probeAsPointer:" ; void* return
      "-setData:To:" ; void* parameter
      "-getDims" ; unsigned * return; removal of this method 
		 ; makes all array methods unusable
      "-iterateAsDouble:using:" ; array iterator
      "-iterateAsInteger:using:" ; array iterator
      "-getBaseType:" ; array type
      "-getRank:" ; array rank
      "-probeAsString:Buffer:"; 2nd arg is char * but that fails because
                             ; strings[] is const char **;
      "-probeAsString:Buffer:withFullPrecision:" ;same reason

      ;; MessageProbe
      "-getArg:" ; val_t return
      "-dynamicCallOn:" ; val_t return

      ;; random
      "-putStateInto:" ; void* parameter
      "-setStateFrom:" ; void* parameter
      "+create:setStateFromSeeds:"; unsigned* parameter
      "+create:setA:setV:setW:setStateFromSeeds:" ; unsigned * parameter
      "-setStateFromSeeds:" ; unsigned* parameter
      "-getMaxSeedValues" ; unsigned* return
      "-getLongDoubleSample" ; long double return
      "-getLongDoubleSample:" ; long double return
      "-getInitialSeeds" ; unsigned* return

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

      ;; EZGraph
      "-getGraph" ; id <Graph> return -- a disabled protocol

      ;; EZBin
      "-getHistogram" ; id <Histogram> return -- a disabled protocol

      ;; HDF5
      "-iterate:"
      "-iterate:drop:"
      "-iterateAttributes:"
      "-loadDataset:"
      "-storeAsDataset:typeName:type:rank:dims:ptr:"
      "-readRowNames"
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

(defun java-print-javadoc-default-contructor ()
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

(defun freaky-message (objc-type)
  (error "Objective C type `%s' in protocol `%s' is freaky!"
         objc-type
         (protocol-name *last-protocol*)))

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
           (if (real-class-p protocol)
               (java-qualified-name current-module protocol :using t)
               "Object")))
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
          "Object")
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

(defun convenience-create-method-p (method)
  (let* ((signature (get-method-signature method))
         (len (length signature))
         (min-len (min len 8)))
    (string= (substring signature 0 min-len) "+create:")))

(defun included-method-p (protocol method phase)
  (and (not (removed-method-p method))
       (eq phase (method-phase method))))

(defun expanded-method-list (protocol phase)
  (remove-if-not #'(lambda (method) (included-method-p protocol method phase))
                 (mapcar #'methodinfo-method
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
	do
        (java-print-javadoc-method method protocol)
        (java-print-method protocol method)))

(defun java-suffix-for-phase (phase)
  (case phase
    (:setting "S")
    (:using "")
    (:creating "C")))

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
  (or (find (module-sym (protocol-module protocol)) *removed-modules*)
      (find (protocol-name protocol) *removed-protocols* :test #'string=)))

(defun included-protocol-list (protocol)
  (remove-if #'removed-protocol-p 
             (protocol-included-protocol-list protocol)))

(defun the-CREATABLE-protocol-p (protocol)
  (string= (protocol-name protocol) "CREATABLE"))

(defun the-RETURNABLE-protocol-p (protocol)
  (string= (protocol-name protocol) "RETURNABLE"))

(defun creatable-p (protocol)
  (member-if #'the-CREATABLE-protocol-p
             (protocol-included-protocol-list protocol)))

(defun returnable-p (protocol)
  (member-if #'the-RETURNABLE-protocol-p
             (protocol-included-protocol-list protocol)))

(defun real-class-p (protocol)
  (or (returnable-p protocol)
      (creatable-p protocol)))

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


(defun collect-convenience-create-methods (protocol)
  (loop for methodinfo in (protocol-expanded-methodinfo-list protocol)
        for method = (methodinfo-method methodinfo)
        when (and (not (removed-method-p method))
                  (convenience-create-method-p method))
        collect method))

(defun collect-convenience-constructor-name.arguments (method)
    (flet ((strip (key) 
             (if (> (length key) 3)
                 (if (string-match (substring key 0 3) "set")
                     (substring key 3)
                     key)
                 key))
           (fix (key) (concat (downcase (substring key 0 1))
                              (substring key 1))))
      (loop for argument in (cdr (method-arguments method))
            collect (cons (fix (strip (car argument)))
                          argument))))

(defun java-print-class-constructor-method (protocol method)
  (let* ((name.arguments
          (collect-convenience-constructor-name.arguments method))
         (module (protocol-module protocol))
         (zone-protocol (lookup-protocol "Zone"))
         (zone-class-name
          (java-qualified-interface-name module zone-protocol :using))
         (creating-class-name (java-class-name protocol :creating))
         (using-class-name (java-class-name protocol :using)))

    ;; if constructor accepts more than one argument, then print it's
    ;; method documentation, otherwise default to boilerplate
    ;; documentation
    (if name.arguments
        (java-print-javadoc-method method protocol)
      (java-print-javadoc-default-contructor))
    (insert "public ")
    (insert using-class-name)
    (insert " (")
    (insert zone-class-name)
    (insert " aZone")
    (loop for name.argument in name.arguments
          do
          (insert ", ")
          (java-print-argument module (cdr name.argument)))
    (insert ") { super (); ")
    (insert "new ")
    (insert creating-class-name)
    (insert " (this).create")
    (flet ((fix (key) (concat (upcase (substring key 0 1))
                              (substring key 1))))
      (loop for name.argument in name.arguments
            do
            (insert "$")
            (insert (car (cdr name.argument)))))
    (insert " (")
    (insert "aZone")
    (loop for name.argument in name.arguments
          do
          (insert ", ")
          (insert (caddr (cdr name.argument))))
    (insert "); }\n")))

(defun java-print-basic-constructor (protocol)
  (insert "public ")
  (insert (java-class-name protocol :using))
  (insert " () { super (); }\n"))

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

(defun inheritance-cases (pname))

(defun java-print-class-phase (protocol phase)
  (java-print-javadoc-protocol protocol t)
  (insert "public ")
  (unless (real-class-p protocol)
    (insert "abstract "))
  (insert "class ")
  (insert (java-class-name protocol phase))
  (let ((pname (protocol-name protocol)))
    (cond ((eq phase :creating)
           (insert " extends swarm.PhaseCImpl"))
	  (t (inheritance-cases pname))
          ))
  (insert " ")
  (when (java-print-implemented-protocols protocol phase ", " nil)
    (insert ", "))
  (insert (java-qualified-interface-name (protocol-module protocol)
                                         protocol :setting))
  (loop for interface-phase in '(:creating :using)
        when (eq phase interface-phase)
        do
        (insert ", ")
        (insert (java-qualified-interface-name (protocol-module protocol)
                                               protocol interface-phase)))
  (insert " {\n")
  (java-print-class-methods-in-phase protocol phase)
  (java-print-class-methods-in-phase protocol :setting)
  (case phase
    (:using
     (java-print-basic-constructor protocol)
     (when (creatable-p protocol)
       (java-print-convenience-constructors protocol)))
    (:creating
     (when (creatable-p protocol)
       (java-print-nextphase-class-constructor protocol))))
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
        ((string= "java.lang.String" type) "jstring")
        ((string= "Class" type) "jclass")
        ((string= "void" type) "void")
        (t "jobject")))

(defun java-argument-convert (argument convert)
  (let* ((type-and-varname (cdr argument))
         (varname (cadr type-and-varname)))
    ;; the case of method with no arguments
    (when varname
      (funcall convert (java-objc-to-java-type t (car type-and-varname))))))

(defun java-argument-empty-p (argument)
  (null (third argument)))

(defun java-argument-print-conversion (current-module argument string-pos)
  (let ((type (second argument))
        (jni-type (java-argument-convert argument
                                         #'java-type-to-native-type))
        (argname (third argument)))
    (if jni-type
        (cond ((string= type "SEL")
               (insert "SD_ENSUREOBJCMETHOD (env, ")
               (insert argname)
               (insert ")"))
              ((string= jni-type "jstring")
               (insert "strings[")
               (insert (format "%d" string-pos))
               (insert "]"))
              ((string= jni-type "jclass")
               (insert "SD_ENSUREOBJCCLASS (env, ")
               (insert argname)
               (insert ")"))
              ((string= type "JOBJECT")
               (insert argname))
              ((string= jni-type "jobject")
               (insert "SD_ENSUREOBJC (env, ")
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
      "javaswarm_target"
      (concat "javaswarm_arg" (prin1-to-string num))))

(defun java-print-method-invocation-arguments (protocol method)
  (unless (method-factory-flag method)
    (insert "  id ")
    (insert (argname-number -1))
    (insert " = SD_ENSUREOBJC (env, jobj);\n"))
  (let ((arguments (method-arguments method))
        (string-pos 0)
        (module (protocol-module protocol)))
    (loop for argument in arguments
          for arg-pos from 0
          for arg-name = (third argument)
          for type = (second argument)
          when (and (not (java-argument-empty-p argument))
                    (local-ref-p argument)
                    (not (java-argument-string-p argument)))
          do
          (insert "  ")
          (insert (if type type "id"))
          (insert " ")
          (insert (argname-number arg-pos))
          (insert " = ")
          (java-argument-print-conversion module argument string-pos)
          (insert ";\n")
          (when (java-argument-string-p argument)
            (incf string-pos)))))

(defun local-ref-p (argument)
  (let ((type (second argument))
        (jni-type (java-argument-convert argument #'java-type-to-native-type)))
    (or (string= type "SEL")
        (string= jni-type "jstring")
        (string= jni-type "jclass")
        (string= jni-type "jobject"))))

(defun java-print-method-invocation-arguments-lref-deletion (protocol method)
  (insert "  (*env)->DeleteLocalRef (env, jobj);\n")
  (let ((arguments (method-arguments method)))
    (loop for argument in arguments
          for argname = (third argument)
          when (and (not (java-argument-empty-p argument))
                    (local-ref-p argument))
          do
          (insert "  (*env)->DeleteLocalRef (env, ")
          (insert argname)
          (insert ");\n"))))

(defun java-argument-ref (argument arg-pos string-pos)
  (cond ((java-argument-string-p argument)
         (concat "strings["
                 (prin1-to-string string-pos)
                 "]"))
        ((local-ref-p argument) (argname-number arg-pos))
        (t (third argument))))

(defun java-print-method-invocation (protocol method)
  (insert "[")
  (if (method-factory-flag method)
      (insert (protocol-name protocol))
      (insert (argname-number -1)))
  (insert " ")
  (let ((arguments (method-arguments method))
        (string-pos 0)
        (module (protocol-module protocol)))
    (insert (first (car arguments)))
    (unless (java-argument-empty-p (car arguments))
      (insert ": ")
      (insert (java-argument-ref (car arguments) 0 string-pos))
      (when (java-argument-string-p (car arguments))
        (incf string-pos))
      (loop for argument in (cdr arguments)
            for key = (first argument)
            for arg-pos from 1
            when key do 
            (insert " ")
            (insert key)
            end
            do
            (insert ": ")
            (insert (java-argument-ref argument arg-pos string-pos))
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
      (insert (module-name module))
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
              (let ((native-type
                     (java-argument-convert argument
                                            #'java-type-to-native-type)))
                (insert-arg native-type)
                (when (string= native-type "jstring")
                  (push (third argument) strings)))
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
          (insert "  const char *strings[] = { SD_COPYSTRING (env, ")
          (insert (first rstrings))
          (insert ")")
          (loop for string in (cdr rstrings)
                do
                (insert ",\n")
                (insert "                        SD_COPYSTRING (env, ")
                (insert string)
                (insert ")"))
          (if (cdr rstrings) (insert "\n  ") (insert " "))
          (insert "};\n")))
      (when (create-method-p method)
        (insert "  jobject nextPhase = SD_NEXTJAVAPHASE (env, jobj);\n"))
      (java-print-method-invocation-arguments protocol method)
      ;; (java-print-method-invocation-arguments-lref-deletion protocol method)

      (insert "  ")
      (unless (string= ret-type "void")
        (if (or strings (create-method-p method))
            (insert "ret = ")
            (insert "return ")))
      
      (let* ((signature (get-method-signature method))
             (objc-type (method-return-type method))
             (java-type-category (java-objc-to-java-type-category objc-type))
             (java-return (java-type-category-to-java-type module
                                                           objc-type
                                                           java-type-category))
             (wrapped-flag 
              (cond ((string= "+createBegin:" signature)
                     (insert "SD_ADDJAVA (env, jobj, ")
                     t)
                    ((create-method-p method)
                     (insert "SD_ADDJAVA (env, nextPhase, ")
                     t)
                    ((string= "-createEnd" signature)
                     (insert "SD_NEXTPHASE (env, jobj, ")
                     t)
                    ((string= java-return "Class")
                     (insert "(jclass) SD_FINDJAVACLASS (env, ")
                     t)
                    ((string= java-return "java.lang.String")
                     (insert "(*env)->NewStringUTF (env, ")
                     t)
                    ((or (string= java-return "Object")
                         (eq java-type-category 'protocol))
                     (insert "SD_ENSUREJAVA (env, ")
                     t)
                    (t nil))))
        (java-print-method-invocation protocol method )
        (when wrapped-flag (insert ")")))
      (insert ";\n")
      (when strings
        (insert "  SD_CLEANUPSTRINGS (env, strings);\n"))
      (when (create-method-p method)
        (insert "  (*env)->DeleteLocalRef (env, nextPhase);\n"))
      (unless (string= ret-type "void")
        (when (or strings (create-method-p method))
          (insert "  return ret;\n")))
      (insert "}\n"))))

(defun java-print-native-class (protocol)
  (with-protocol-c-file protocol
    (insert "#import <defobj/directory.h>\n")
    (insert "#import <defobj.h>\n")
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
            do
            (loop for method in (expanded-method-list protocol phase)
                  do
                  (java-print-native-method method protocol phase)
                  (insert "\n")))
      (loop for method in (expanded-method-list protocol :setting)
	    do 
	    (java-print-native-method method protocol :creating)
	    (insert "\n")
	    (java-print-native-method method protocol :using)))))

(defun java-print-makefiles ()
  (ensure-directory (c-path))
  (with-temp-file (concat (get-builddir) "Makefile.common")
    (loop for module-sym being each hash-key of *module-hash-table* 
          using (hash-value protocol-list)
          for dir = (module-path module-sym)
          do
          (insert (symbol-name module-sym))
          (insert "_noncreatable_PROTOCOLS =")
          (loop for obj in protocol-list
                when (and (protocol-p obj)
                          (not (real-class-p obj))
                          (not (removed-protocol-p obj)))
                do
                (insert " ")
                (insert (protocol-name obj)))
          (insert "\n\n")
          (insert (symbol-name module-sym))
          (insert "_creatable_PROTOCOLS =")
          (loop for obj in protocol-list
                when (and (protocol-p obj)
                          (real-class-p obj)
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

(defun java-run-all (&key unicode)
  (set-dollar-sign unicode)
  (load-and-process-modules :uniquify-method-lists t)
  (java-print-makefiles)
  (java-print-classes))

(defun java-run-all-javadoc ()
  (interactive)
  (set-dollar-sign t)
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

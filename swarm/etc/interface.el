(require 'cl)
(eval-and-compile
 (push (getenv "TOP_BUILDDIR") load-path))
(require 'protocol)
(provide 'interface)

(defvar *last-protocol* nil)

(defvar *extra-removed-methods* nil)

(defvar *extra-unwanted-create-method-signatures* nil)

(defconst *removed-protocols* 
    '(
      "CREATABLE"
      "RETURNABLE"
      
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
      "InputWidget"
      "CompositeItem"
      "CanvasAbstractItem"

      ;; simtoolsgui non-creatable
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

      ;; Arguments
      "-setOptionFunc:"
      "-addOptions:"
      "+createArgc:Argv:appName:version:bugAddress:options:optionFunc:inhibitExecutableSearchFlag:"
      "-getArgv"

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

      ;; (IDL doesn't like leading underscore)
      ;; Activity 
      "-_performAction_:"
      ;; GetSubActivityAction
      "-_getSubactivityAction_"
      ;; ConcurrentGroup
      "-_setActionConcurrent_:"
      "-_getEmptyActionConcurrent_"
      
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

(defun module-path (module)
  (let ((sym (module-sym module)))
    (if (eq sym 'swarm)
        ""
      (concat (symbol-name sym) "/"))))
       
(defun c-path (&optional subpath)
  (concat (get-builddir) "c/" subpath))

(defun ensure-directory (dir)
  (unless (file-directory-p dir)
    (make-directory dir)))

(defun suffix-for-phase (phase)
  (cond ((inclusive-phase-p phase :setting) "S")
        ((inclusive-phase-p phase :using) "")
        ((inclusive-phase-p phase :creating) "C")))

(defun method-ellipsis-p (method)
  (let ((arguments (method-arguments method)))
    (loop for argument in arguments
          when (and (null (first argument))
                    (null (second argument))
                    (string= (third argument) "..."))
          return t
          finally return nil)))

(defun removed-method-p (method)
  (let ((signature (get-method-signature method)))
    (or (find signature *removed-methods* :test #'string=)
        (find signature *extra-removed-methods* :test #'string=)
        (method-ellipsis-p method))))

(defun removed-protocol-p (protocol)
  (or (find (module-sym (protocol-module protocol)) *removed-modules*)
      (find (protocol-name protocol) *removed-protocols* :test #'string=)))

(defun included-protocol-list (protocol)
  (remove-if #'removed-protocol-p 
             (protocol-included-protocol-list protocol)))

(defun print-implemented-interfaces-list (protocol phase name-func expand-flag)
  (let ((first t)
        (module (protocol-module protocol)))
    (loop for iprotocol in (if expand-flag
                               (generate-complete-protocol-list protocol)
                             (included-protocol-list protocol))
          do
          (if first
              (setq first nil)
            (insert ", "))
          (unless (inclusive-phase-p phase :setting)
            (insert (funcall name-func module iprotocol phase))
            (insert ", "))
          (insert (funcall name-func module iprotocol :setting)))
    (not first)))


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

(defun regexp-match-p (str regexp)
  (let ((expr (concat (concat "^" regexp) "$")))
    (string-match expr str)))

(defun objc-protocol-for-type (objc-type)
  (when objc-type
    (when (regexp-match-p objc-type "id +<\\(.*\\)>")
      (let ((protocol-name (match-string 1 objc-type)))
        (lookup-protocol protocol-name)))))

(defun freaky-message (objc-type)
  (error "Objective C type `%s' in protocol `%s' is freaky!"
         objc-type
         (protocol-name *last-protocol*)))

(defun included-method-p (protocol method phase)
  (and (not (removed-method-p method))
       (inclusive-phase-p (method-phase method) phase)))

(defun extract-protocol-name-from-objc-type (type)
  (when (string-match "<\\(.*\\)>" type)
    (match-string 1 type)))

(defun print-makefile.common (&optional c-subdir)
  (with-temp-file (concat (get-builddir) "Makefile.common")
    (loop for module-sym being each hash-key of *module-hash-table* 
          using (hash-value protocol-list)
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
    ))
  
(defun unwanted-create-method-p (protocol method)
  (let ((signature (get-method-signature method)))
    (or
     (and (string= signature "+create:")
          (find (protocol-name protocol)
                '("UniformUnsignedDist"
                  "PMMLCG1gen"
                  "C2TAUS3gen"
                  "RandomBitDist"
                  "ExponentialDist"
                  "MT19937gen"
                  "UniformDoubleDist"
                  "NormalDist"
                  "C2TAUS1gen"
                  "GammaDist"
                  "UniformIntegerDist"
                  "PSWBgen"
                  "LogNormalDist"
                  "BernoulliDist"
                  "C2TAUS2gen")
                :test #'string=))
     (find signature 
           '("+create:setGenerator:setMean:setVariance:"
             "+create:setGenerator:setVirtualGenerator:setMean:setVariance:"
             "+createParent:")
           :test #'string=)
     (find signature
           *extra-unwanted-create-method-signatures*
           :test #'string=))))
             

(defun match-signature (signature match-signature)
  (let* ((len (length signature))
         (sig-len (length match-signature)))
    (when (>= len sig-len)
      (string= (substring signature 0 sig-len) match-signature))))
  
(defun match-create-signature (signature)
  (or (match-signature signature "+createParent:") ; gui
      (match-signature signature "+createWithDefaults:"); random
      (match-signature signature "+create:")))

(defun convenience-create-method-p (protocol method)
  (unless (unwanted-create-method-p protocol method)
    (match-create-signature (get-method-signature method))))
  
(defun collect-convenience-create-methods (protocol)
  (loop for methodinfo in (protocol-expanded-methodinfo-list protocol)
        for method = (methodinfo-method methodinfo)
        when (and (not (removed-method-p method))
                  (convenience-create-method-p protocol method))
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

(defun create-method-p (method)
  (let* ((signature (get-method-signature method))
         (len (length signature))
         (min-len (min len 7)))
    (string= (substring signature 0 min-len) "+create")))

(defun method-list-for-phase (protocol phase)
  (remove-if-not #'(lambda (method)
                     (included-method-p protocol method phase))
                 (protocol-method-list protocol)))

(defun expanded-method-list (protocol phase)
  (remove-if-not #'(lambda (method) (included-method-p protocol method phase))
                 (mapcar #'methodinfo-method
                         (protocol-expanded-methodinfo-list protocol))))

(defun augment-type-hash-table (ht method)
  (let* ((return-type (method-return-type method))
         (mprotocol (objc-protocol-for-type return-type)))
    (when mprotocol
      (setf (gethash return-type ht) mprotocol)))
  (loop for argument in (method-arguments method)
        for argument-type = (argument-type argument)
        for mprotocol = (objc-protocol-for-type argument-type)
        when mprotocol
        do
        (setf (gethash argument-type ht) mprotocol)))

(defun create-type-hash-table (protocol phase)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for method in (method-list-for-phase protocol phase)
          do
          (augment-type-hash-table ht method))
    ht))

(defun create-type-hash-table-for-convenience-create-methods (protocol)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for method in (collect-convenience-create-methods protocol)
          do
          (augment-type-hash-table ht method))
    ht))
    
(defun create-hash-table-for-initialization-parameters (protocol)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for method in (collect-convenience-create-methods protocol)
          do
          (when (has-arguments-p method)
            (loop for argument in (method-arguments method)
                  for pos from 0
                  do
                  (setf (gethash (argument-name argument) ht)
                        (cons pos (argument-type argument))))))
    ht))

(defun print-argument (argument
                       convert-type-func
                       convert-name-func)
  (let* ((type-and-varname (cdr argument))
         (varname (cadr type-and-varname)))
    ;; the case of method with no arguments
    (when varname
      (insert (funcall convert-type-func (car type-and-varname)))
      (insert " ")
      (insert (funcall convert-name-func varname))
      t)))

(defun get-variable-name-for-getter-method (method)
  (let* ((first-argument (first (method-arguments method)))
         (name (strip-regexp (first first-argument) "^get"))
         (ret-type (method-return-type method)))
    (cond ((string= "id <Symbol>" ret-type) name)
          (t (concat (downcase (substring name 0 1))
                     (substring name 1))))))

(defun protocol-implementation-class-name (protocol)
  (let ((name (protocol-name protocol)))
    (cond ((string= name "Index") "Index_any")
          ((string= name "List") "List_linked")
          ((string= name "ListIndex") "ListIndex_linked")
          ((member (module-sym (protocol-module protocol))
                   '(defobj collections activity))
           (concat name "_c"))
          (t name))))

(defun create-method-implementation-hash-table (protocol phase)
  (let ((ht (make-hash-table :test #'equal))
        (method-signatures (mapcar #'get-method-signature
                                   (expanded-method-list protocol phase))))
    (with-temp-buffer
      (let ((ret (apply
                  #'call-process
                  (concat (get-builddir) "findImp")
                  nil t nil
                  (protocol-implementation-class-name protocol)
                  method-signatures)))
        (if (and (numberp ret) (zerop ret))
            (progn
              (goto-char (point-min))
              (loop for method-sig in method-signatures
                    do
                    (let ((start (point)))
                      (forward-line)
                      (let ((str (buffer-substring start (- (point) 1))))
                        (setf (gethash method-sig ht) str))))
              ht)
          (progn
            (message "Could not convert protocol %s %s"
                     (protocol-name protocol)
                     method-signatures)
            nil))))))
                          



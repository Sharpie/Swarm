(require 'cl)
(eval-and-compile
 (push (getenv "TOP_BUILDDIR") load-path))
(require 'protocol)
(provide 'interface)

(defvar *last-protocol*)

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
  (concat (symbol-name (module-sym module)) "/"))

(defun c-path (&optional subpath)
  (concat (get-builddir) "c/" subpath))

(defun ensure-directory (dir)
  (unless (file-directory-p dir)
    (make-directory dir)))

(defun suffix-for-phase (phase)
  (case phase
    (:setting "S")
    (:using "")
    (:creating "C")))

(defun method-ellipsis-p (method)
  (let ((arguments (method-arguments method)))
    (loop for argument in arguments
          when (and (null (first argument))
                    (null (second argument))
                    (string= (third argument) "..."))
          return t
          finally return nil)))

(defun removed-method-p (method)
  (or (find (get-method-signature method) *removed-methods* :test #'string=)
      (method-ellipsis-p method)))

(defun removed-protocol-p (protocol)
  (or (find (module-sym (protocol-module protocol)) *removed-modules*)
      (find (protocol-name protocol) *removed-protocols* :test #'string=)))

(defun included-protocol-list (protocol)
  (remove-if #'removed-protocol-p 
             (protocol-included-protocol-list protocol)))

(defun print-implemented-interfaces-list (protocol phase name-func self-flag)
  (let ((first t)
        (module (protocol-module protocol))
        (iprotocols (included-protocol-list protocol)))
    (loop for iprotocol in (if self-flag (cons protocol iprotocols) iprotocols)
          do
          (if first
              (setq first nil)
            (insert ", "))
          (insert (funcall name-func module iprotocol :setting))
          (unless (eq phase :setting)
            (insert ", ")
            (insert (funcall name-func module iprotocol phase))))
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

(defun freaky-message (objc-type)
  (error "Objective C type `%s' in protocol `%s' is freaky!"
         objc-type
         (protocol-name *last-protocol*)))

(defun included-method-p (protocol method phase)
  (and (not (removed-method-p method))
       (eq phase (method-phase method))))

(defun extract-protocol-name-from-objc-type (type)
  (when (string-match "<\\(.*\\)>" type)
    (match-string 1 type)))

(defun print-makefile.common (&optional c-subdir)
  (with-temp-file (concat (get-builddir) "Makefile.common")
    (loop for module-sym being each hash-key of *module-hash-table* 
          using (hash-value protocol-list)
          for module = (lookup-module module-sym)
          for dir = (module-path module)
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

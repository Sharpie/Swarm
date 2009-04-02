;; Copyright © 2000, 2001 Swarm Development Group
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
(provide 'interface)

(defvar *last-protocol* nil)

(defvar *extra-removed-methods* nil)

(defvar *extra-unwanted-create-method-signatures* nil)

(defvar *java-flag* nil)

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

      ;; "Canvas"

      ;; "CanvasItem"
      ;; "CanvasAbstractItem"

      ;; "CompositeItem"
      ;; "NodeItem"
      ;; "OvalNodeItem"
      ;; "RectangleNodeItem"

      ;; gui creatable
      "Frame"
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
      "ScheduleItem"
      "TextItem"
      "Line"
      "Circle"
      "Rectangle"

      ;; gui non-creatable
      "WindowGeometryRecord"
      "InputWidget"

      ;; simtoolsgui non-creatable
      "MessageProbeWidget"
      "MultiVarProbeWidget"

      ;; random generators
      "LCGgen" "LCG1gen" "LCG2gen" "LCG3gen"
      
      ;; analysis
      "FunctionGraph"

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
      ;; DefinedClass
      "+getMethodFor:" ; IMP return

      ;; CreatedClass
      "-at:addMethod:" ; IMP parameter
      
      ;; Object_s
      "-addRef:withArgument:" ; void* parameter, ref_t return
      "-removeRef:" ; ref_t parameter
      "+conformsTo:" ; Protocol* parameter
      "-conformsTo:" ; Protocol* parameter

      "-lispStoreBooleanArray:Keyword:Rank:Dims:Stream:"
      "-lispStoreCharArray:Keyword:Rank:Dims:Stream:"
      "-lispStoreShortArray:Keyword:Rank:Dims:Stream:"
      "-lispStoreIntegerArray:Keyword:Rank:Dims:Stream:"
      "-lispStoreUnsignedArray:Keyword:Rank:Dims:Stream:"
      "-lispStoreLongArray:Keyword:Rank:Dims:Stream:"
      "-lispStoreUnsignedLongArray:Keyword:Rank:Dims:Stream:"
      "-lispStoreLongLongArray:Keyword:Rank:Dims:Stream:"
      "-lispStoreUnsignedLongLongArray:Keyword:Rank:Dims:Stream:"
      "-lispStoreFloatArray:Keyword:Rank:Dims:Stream:"
      "-lispStoreDoubleArray:Keyword:Rank:Dims:Stream:"

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
      "-addArgument:ofType:" ; void*, fcall_type_t parameters
      "-addArgument:ofObjCType:" ; void* parameter
      "-addLongDouble:" ; long double parameter
      "-setReturnType:" ; fcall_type_t parameter
      "-getRetVal" ; val_t

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

      ;; EZDistribution
      "-getProbabilities"; double* return

      ;; GridData
      "-getOffsets" ; long* return
      "-getLattice"; id* return

      ;; Discrete2d
      "-allocLattice" ; id* return
      "-setLattice:" ; id* parameter

      ;; DblBuffer2d
      "-getNewLattice" ; id* return

      ;; QSort
      "+sortNumbersIn:using:" ; function pointer parameter

      ;; InputWidget
      "-linkVariableBoolean:" ; BOOL* parameter
      "-linkVariableInt:" ; int* parameter
      "-linkVariableDouble:" ; double* parameter
      
      ;; Form
      "-addLineName:Boolean:" ; BOOL* parameter
      "-addLineName:Int:" ; int* parameter
      "-addLineName:Double:" ; double* parameter

      ;; Histogram
      ; "-setLabels:count:" ; const char * const * parameter
      ; "-setColors:count:" ; const char * const * parameter
      "-drawHistogramWithInt:" ; int * parameter
      "-drawHistogramWithInt:atLocations:" ; int*, double* parameters
      "-drawHistogramWithDouble:" ; double * parameter
      "-drawHistogramWithDouble:atLocations:" ; double * parameter

      ;; GraphElement
      "+createOwnerGraph:"

      ;; Colormap
      "-map" ; PixelValue * return
      "-black" ; PixelValue return
      "-white" ; PixelValue return

      ;; HDF5
      "-iterate:"
      "-iterate:drop:"
      "-iterateAttributes:"
      "-loadDataset:"
      "-storeAsDataset:typeName:type:rank:dims:ptr:"
      "-readRowNames"
      "-getDatasetType" ; fcall_type_t return
      "-setExtensibleVectorType:" ; fcall_type_t argument
      ))

(defun module-path (module)
  (let ((sym (module-sym module)))
    (if (eq sym 'SwarmTop)
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
     (and (and *java-flag*
               ;; results in constructor conflict for Java because create
               ;; name is dropped, however COM needs to implement
               ;; all the claimed interfaces
               (string= signature "+create:"))
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
                  "C2TAUS2gen"
                  "PoissonDist"
                  "BinomialDist"
                  )
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
  
(defun create-signature-p (signature)
  (or (match-signature signature "+createParent:") ; gui
      (match-signature signature "+createWithDefaults:"); random
      (match-signature signature "+create:")
      (match-signature signature "+createFromMethod:"); COM Selector
      (match-signature signature "+initSwarm:version:bugAddress:argCount:args:")))

(defun convenience-create-method-p (protocol method)
  (unless (unwanted-create-method-p protocol method)
    (create-signature-p (get-method-signature method))))

(defun creating-phase-method-p (method)
  "for use when moving convenience create methods entirely into using phase"
  (let ((sig (get-method-signature method)))
    (or (string= sig "+createBegin:")
        (string= sig "+customizeBegin:")
        (not (method-factory-flag method)))))
  
(defun collect-convenience-create-methods (protocol &optional immediate-flag)
  (loop for methodinfo in (protocol-expanded-methodinfo-list protocol)
        for method = (methodinfo-method methodinfo)
        when (and (not (removed-method-p method))
                  (convenience-create-method-p protocol method)
                  (or (not immediate-flag)
                      (zerop (methodinfo-level methodinfo))))
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
      (loop for argument in (method-arguments method)
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

(defun create-type-hash-table-for-convenience-create-methods (protocol &optional immediate-flag)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for method in
          (collect-convenience-create-methods protocol immediate-flag)
          do
          (augment-type-hash-table ht method))
    ht))

(defun create-type-hash-table-for-immediate-convenience-create-methods (protocol)
  (create-type-hash-table-for-convenience-create-methods protocol t))
    
(defun print-argument (argument
                       convert-type-func
                       convert-name-func)
  (let* ((varname (argument-name argument)))
    ;; the case of method with no arguments
    (when varname
      (insert (funcall convert-type-func (argument-type argument)))
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

(defun impl-print-imp-pointer (method print-return-type print-argument name-flag) 
  (funcall print-return-type method)
  (insert " (*")
  (when name-flag (insert "swarm_imp"))
  (insert ") ")  
  (if (method-factory-flag method)
      (insert "(Class objcTarget, SEL objcSel")
      (insert "(id objcTarget, SEL objcSel"))
  (when (has-arguments-p method)
    (loop for argument in (method-arguments method)
          do
          (insert ", ")
          (funcall print-argument argument)))
  (insert ")"))

(defun impl-print-get-imp-pointer (method print-return-type print-argument)
  (insert "  ")
  (impl-print-imp-pointer method print-return-type print-argument t)
  (insert ";\n")
  (if (method-factory-flag method)
      (progn
        (insert "  MetaClass mClass = class_get_meta_class (swarm_target);\n")
        (insert "  swarm_imp = (")
        (impl-print-imp-pointer method print-return-type print-argument nil)
        (insert ") class_get_class_method (mClass, swarm_sel)->method_imp;\n"))
      (progn
        (insert "  swarm_imp = (")
        (impl-print-imp-pointer method print-return-type print-argument nil)
        (insert ") objc_msg_lookup (swarm_target, swarm_sel);\n"))))

(defun impl-print-get-sel (method)
  (insert "  SEL swarm_sel = sel_get_uid (\"")
  (insert (substring (get-method-signature method) 1))
  (insert "\");\n"))

(defun load-dispatch-hash-table (protocol phase ht)
  (let ((ml (expanded-method-list protocol phase)))
    (set-verbosity nil)
    (with-temp-buffer 
      (if (eql 0 (apply #'call-process
                        (concat (get-top-builddir) "tools/findImp")
                        nil t nil
                        (protocol-name protocol)
                        (mapcar #'get-method-signature ml)))
          (progn
            (beginning-of-buffer)
            (modify-syntax-entry ?: "w")
            (skip-whitespace)
            (while (< (point) (point-max))
              (let* ((signature
                      (let ((beg (point)))
                        (forward-sexp)
                        (buffer-substring beg (point))))
                     (funcsym 
                      (let ((beg (progn
                                   (skip-whitespace)
                                   (point))))
                        (forward-sexp)
                        (buffer-substring beg (point))))
                     (method (find signature ml :key #'get-method-signature :test #'string=)))
                (if method
                    (setf (gethash method ht) (concat "swarm" funcsym))
                  (message (progn
                             (beginning-of-line)
                             (let ((beg (point)))
                               (end-of-line)
                               (buffer-substring beg (point)))))))
              (skip-whitespace)))
        (message (concat (protocol-name protocol) " failed"))))
    (set-verbosity t)))
    
(defun create-dispatch-hash-table (protocol phase)
  (let ((ht (make-hash-table)))
    (load-dispatch-hash-table protocol phase ht)
    ht))

(defun funcsyms-suffix (protocol phase)
  (concat 
   (protocol-name protocol)
   (suffix-for-phase phase)
   "-funcsyms"))

(defun dump-dispatch-hash-table (ht protocol phase)
  (with-temp-file (concat (c-path)
			  (funcsyms-suffix protocol phase) 
			  "-head.c")
    (loop for method being each hash-key of ht
	  using (hash-value funcsym)
	  do
	  (insert "IMP ")
	  (insert funcsym)
	  (insert ";\n")))
  (with-temp-file (concat (c-path)
			  (funcsyms-suffix protocol phase) 
			  "-body.c")
    (loop for method being each hash-key of ht
	  using (hash-value funcsym)
	  do
	  (insert funcsym)
	  (insert " = ")
	  (let* ((end
		  (car
		   (remove ""
			   (if (method-factory-flag method)
			       (split-string funcsym "swarm_c_")
			     (split-string funcsym "swarm_i_")))))
		 (class (first (split-string end "__")))
		 (selName (substring (get-method-signature method) 1)))
	    (if (method-factory-flag method)
		(progn
		  (insert "class_get_class_method (class_get_meta_class (objc_lookup_class (\"")
		  (insert class)
		  (insert "\")), sel_get_uid (\"")
		  (insert selName)
		  (insert "\"))->method_imp"))
	      (progn
		(insert "get_imp (objc_lookup_class (\"")
		(insert class)
		(insert "\"), sel_get_uid (\"")
		(insert selName)
		(insert "\"))")))
	    (insert ";\n")))))
  
(defun c-objc-type (type)
  (if type type "id"))

(defun impl-print-method-declaration (method funcsym convert-func)
  (insert "  extern ")
  (insert (funcall convert-func (method-return-type method)))
  (insert " (*")
  (insert funcsym)
  (insert ") ")
  (if (method-factory-flag method)
      (insert " (Class objcTarget, SEL objcSel")
    (insert " (id objcTarget, SEL objcSel"))
  (when (has-arguments-p method)
    (loop for argument in (method-arguments method)
          do
          (insert ", ")
          (print-argument argument convert-func #'identity)))
  (insert ");\n"))

(defun impl-print-method-setup (method dht convert-func sel-flag decl-flag)
  (let ((funcsym (gethash method dht)))
    (when (or (not funcsym) sel-flag)
      (impl-print-get-sel method))
    (if funcsym
        (when decl-flag
          (impl-print-method-declaration method funcsym convert-func))
      (progn
        (impl-print-get-imp-pointer
         method
         #'(lambda (method)
             (insert (funcall convert-func (method-return-type method))))
         #'(lambda (argument)
             (print-argument argument convert-func #'identity)))))))


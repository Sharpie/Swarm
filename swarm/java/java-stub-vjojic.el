(require 'cl)
(load (concat (getenv "SWARMSRCDIR") "/etc/protocol.el"))

(defvar *java-class-hash-table* (make-hash-table :test #'equal))
(defvar *java-interface-hash-table* (make-hash-table :test #'equal))
(defvar *java-package-hash-table* (make-hash-table :test #'equal))

(defstruct java-package
  name
  class-list
  interface-list)

(defstruct java-class
  name
  package
  super-class
  phase
  other-phase-class
  implemented-interface-list
  method-list)

(defstruct java-interface
  name
  package
  extended-interface-list		
  method-list)

(defstruct java-method
  name
  modifier
  return-type
  argument-list)

(defstruct java-argument
  name
  type)


(defun java-is-creatable (protocol)
  (loop for iprotocol in (protocol-included-protocol-list protocol)
	do 
	(if (string= (protocol-name iprotocol) "CREATABLE")
	  (return  t)
	  nil)))


(defun java-get-interface (interface-name phase)
  (let ((interface
	 (if phase    
	     (interface (gethash (concat interface-name "-s") 
				 *java-interface-hash-table*))
	   nil)))
    (if interface
	interface
      (gethash interface-name *java-interface-hash-table*))))

  
	

(defun java-get-protocol (protocolname)
  (gethash protocolname *protocol-hash-table*))

(defun java-get-super-class-name (class-name)
  (if (string= class-name "CreateDrop") 
      "Customize"
    (let* ((argument (concat " -c " class-name))
	   (super-class-name "")
	   (program-path "/opt/src/vjojic/build/tests/java/get-super")
	   (program (concat program-path argument))) 
      (setq super-class-name (shell-command-to-string program))
      (if (string= super-class-name "Nil")
	  nil
	(let ((sub-string-list (split-string super-class-name "_")))
	  if (string= (car sub-string-list) "")
	  super-class-name
	  (car sub-string-list))))))
  
(defun java-convert-type (type)
  (if type
      (cond ( (eq (compare-strings type 0 2 "id" 0 2) t) 
	      'Object)
	    ( (string= type "void") "void" )
	    ( (string= type "const char *") "String" )
	    ( (string= type "char *") "String" )
	    ( (string= type "int") "int" )
	    ( (string= type "long") "long" )
	    ( (string= type "double") "double" )
	    ( (string= type "float") "float" )
	    ( (string= type "unsigned") "int" )
	    ( (string= type "long double")  "double" )
	    ( (string= type "unsigned long long int")  "long" )
	    ( (string= type "BOOL")  "boolean" )
	    ( (string= type "void *")  "Object" )
	    ( (string= type "ref_t")  "Object" )
	    ( (string= type "val_t")  "Object" )
	    ( (string= type "Protocol *")  "Class" )
	    ( (string= type "SEL")  "java.lang.reflect.Method" )
	    ( (string= type "notify_t")  "Notify_t" )
	    
	    ( t  type ))
    'Object ))

(defun java-make-argument (argument)
  (make-java-argument
   :name (car (cdr (cdr argument)))
   :type (java-convert-type (car (cdr argument)))))

(defun java-make-argument-list (argument-list)
  (loop for arg in argument-list
      collect (java-make-argument arg)))

(defun java-make-method (method modifier)  
  (let ((arguments  (car (method-arguments method))))
    (make-java-method
     :name (car arguments)
     :modifier modifier
     :return-type (java-convert-type (method-return-type  method))
     :argument-list (java-make-argument-list arguments))))

(defun java-copy-method (method modifier)
  
  (make-java-method
   :name (java-method-name method)
   :modifier (if modifier
		 modifier
	       (java-method-modifier method))
   :return-type (java-method-return-type method)
   :argument-list (java-method-argument-list method)))


(defun java-make-method-list-with-modifier (phase modifier super 
						  interface-list)
  (let ((super-method-list
	 (let ((method-list 
		(if super
		    (if (java-interface-p super)
			(java-interface-method-list super)
		      (if (java-class-p super)
			  (java-class-method-list super)
			(if (protocol-p super)
			   (progn
			     (princ "ObjectiveC: ")
			     (princ 
			      (protocol-name super))
			     (princ "\n"))
			  (progn
			    (princ (java-interface-p super))
			    (princ (java-class-p super))
			    (princ "Problem"); super)
			    nil))))
		  nil)))
	     (loop for method in method-list
		  collect 
		  (if phase
		      (if (eq (method-phase phase) phase)
			  (java-copy-method method modifier))
		   (java-copy-method method modifier)))))
	(interface-method-list
	 (if interface-list
	     (java-make-method-list-with-modifier phase modifier 
						  (car interface-list) 
						  (cdr interface-list))
	   nil)))
	(union super-method-list interface-method-list)))

(defun java-make-method-list (phase super interface-list) 
  (java-make-method-list-with-modifier phase nil super interface-list))
	       
(defun java-make-interface-list (protocol super-class phase)
  (let ((super-class-name 
	 (if super-class
	     (java-class-name super-class)
	   nil)))
  (loop for iprotocol in (protocol-included-protocol-list protocol)
       unless (string= (protocol-name iprotocol) super-class-name)
       collect (let ((interface 
		      (java-get-interface (protocol-name iprotocol) phase)))
		 (if interface
		     interface
		   (progn
		     (java-make-interface iprotocol)
		     (java-get-interface (protocol-name iprotocol) phase)))))))

; add interface to hash-table!
(defun java-make-interface (protocol)
  (if (java-is-creatable protocol)
      (progn
	(princ "Trying to make an interface out of creatable protocol: ")
	(princ (protocol-name protocol))
	(princ "\n")
	(let ((class (gethash (protocol-name protocol) 
			      *java-class-hash-table*)))
	  (if class
	      class
	    (java-derive-classes protocol))))
    (let* ((interface-name (protocol-name protocol))
	  (interface-list (java-make-interface-list protocol nil nil))
	  (method-list (java-make-method-list-with-modifier 
			nil nil nil interface-list))
	  (new-interface (make-java-interface
			  :name interface-name
			  :package (symbol-name (module-sym (protocol-module protocol)))
			  :extended-interface-list interface-list
			  :method-list method-list)))
      (java-add-interface new-interface)
      new-interface)))
		    
(defun java-make-interface-setting (protocol)
  (let* ((interface-name (concat (protocol-name protocol) "-s"))
	  (interface-list (java-make-interface-list protocol nil :setting))
	  (method-list (java-make-method-list-with-modifier 
			nil nil nil interface-list))
	  (new-interface (make-java-interface
			  :name interface-name
			  :package (symbol-name (module-sym (protocol-module protocol)))
			  :extended-interface-list interface-list
			  :method-list method-list)))
      (java-add-interface new-interface)
      new-interface))
   

(defun java-get-class (class-name)
  (gethash class-name *java-class-hash-table*))

(defun java-get-modified-class-name (class-name phase)
  (case phase
    (:creating class-name)
    (:setting (concat class-name "-s"))
    (:using (concat class-name "-u"))))

(defun java-get-package (package-name)
  (let ((package (gethash package-name 
			    *java-package-hash-table*)))
    (if package
	package
      (let ((new-package (make-java-package
			  :name package-name
			  :class-list nil
			  :interface-list nil)))
	(setf (gethash package-name *java-package-hash-table*) new-package)
	new-package))))

(defun java-add-class (class)
  (let ((package (java-get-package (java-class-package class))))
    (setf (gethash (java-class-name class) *java-class-hash-table*) class)
    (setf (java-package-class-list package)
	    (cons class (java-package-class-list package)))))
  
(defun java-add-interface (interface)
  (let ((package (java-get-package (java-interface-package interface))))
    (setf (gethash (java-interface-name interface) *java-interface-hash-table*)
	  interface)
    (setf (java-package-interface-list package)
	    (cons interface (java-package-interface-list package)))))

(defun java-get-super-class (super-class-name phase)
  (progn
    (if super-class-name
	(let* ((super-class-modified-name 
		(java-get-modified-class-name 
		 super-class-name phase))
	       (super-class
		(java-get-class super-class-modified-name)))
	  (if super-class
	      super-class
	    (let ((super-protocol (java-get-protocol super-class-name)))
	      (if super-protocol
		  (java-make-class super-protocol phase)
		(progn
		  (princ super-class-name)
		  nil)))))
      nil)))

(defun java-make-class (protocol phase)
  (let* ((super
	 (let ((super-class-name (java-get-super-class-name 
				  (protocol-name protocol))))
	   (java-get-super-class super-class-name phase)))
	 (class-name (java-get-modified-class-name 
		      (protocol-name protocol)
		      phase))
	 (interface-list (java-make-interface-list protocol super phase))
	 (method-list (java-make-method-list-with-modifier 
		       phase 'native super interface-list)))

    (let ((new-class (make-java-class
		      :name class-name
		      :package (symbol-name (module-sym (protocol-module protocol)))
		      :super-class super
		      :other-phase-class nil
		      :implemented-interface-list interface-list
		      :method-list method-list)))
      (java-add-class new-class)
      new-class)))

(defun java-link-classes (setting-interface creating-class using-class)
  (let ((setting-method-list 
	(java-make-method-list-with-modifier nil 'native 
					     setting-interface nil)))
      
    (setf (java-class-method-list creating-class)
	  (union
	   (java-class-method-list creating-class)
	   setting-method-list))
    (setf (java-class-other-phase-class creating-class) using-class)
    (setf (java-class-implemented-interface-list creating-class)
	  (cons setting-interface 
		(java-class-implemented-interface-list creating-class)))

    (setf (java-class-method-list using-class)
	  (union
	   (java-class-method-list using-class)
	   setting-method-list))
    (setf (java-class-other-phase-class using-class) creating-class)
    (setf (java-class-implemented-interface-list using-class)  
	  (cons setting-interface
	   (java-class-implemented-interface-list using-class)))))
	  
(defun java-derive-classes (protocol)
  (if (java-is-creatable protocol)
      (let ((setting (java-make-interface-setting protocol))
	    (using (java-make-class protocol :using))
	    (creating (java-make-class protocol :creating)))
	(java-link-classes setting creating using))
    (java-make-interface protocol)))

(defun java-print-argument (argument buffer)
  (progn
    (princ (java-argument-type argument) buffer)
    (princ " " buffer)
    (princ (java-argument-name argument) buffer)))

(defun java-print-argument-list (argument-list buffer)
  (let ((first t))
    (loop for argument in argument-list
	do  (if first
	      (setq first nil)
	    (princ ", " buffer))
	  (java-print-argument argument buffer))))
			     
(defun java-print-method (method buffer)
  (progn
    (princ (java-method-modifier method) buffer)
    (princ " " buffer)
    (princ name buffer)
    (princ " " buffer)
    (princ "(" buffer)
    (java-print-argument-list (java-method-argument-list method) buffer)
    (princ ");\n" buffer)))

(defun java-print-method-list (method-list buffer)
  (loop for method in method-list
	do (java-print-method method buffer)))

(defun java-print-dependencies (class)
  (with-output-to-string
    (let ((package (if (java-class-p class)
		       (java-class-package class)
		     (java-interface-package class)))
	  (first t)
	  (interface-list (if (java-class-p class)
			      (java-class-implemented-interface-list class)
			    (java-interface-extended-interface-list class))))
      (loop for interface in interface-list
	    do 
	    (if (java-class-p interface)
		(if (not (equal (java-class-package interface)
				package))
		    (progn
		      (if first
			  (setq first nil)
			(princ ", " )) 
		      (princ "swarm.")
		      (princ (java-class-package interface))
		      (princ ".")
		      (princ (java-class-name interface))))
	      (if (not (equal (java-interface-package interface)
			      package))
		  (progn
		    (if first
			(setq first nil)
		      (princ ", " )) 
		    (princ "swarm.")
		    (princ (java-interface-package interface))
		    (princ ".")
		    (princ (java-interface-name interface)))))))))


(defun java-print-class (class buffer)
  (princ "package " buffer )
  (princ (java-class-package class) buffer)
  (princ ";\n" buffer)
  (let ((deps (java-print-dependencies class)))
    (if deps
	(progn
	  (princ "\nimport " buffer)
	  (princ deps buffer)
	  (princ ";\n" buffer))))
  (princ "\n\nclass " buffer)
  (princ (java-class-name class) buffer)
  (princ '{ buffer)
  (princ "\n" buffer)
  (java-print-method-list (java-class-method-list class) buffer)
  (princ '} buffer))

(defun java-print-interface (interface buffer)
  (princ "package " buffer)
  (princ (java-interface-package interface) buffer)
  (princ ";\n" buffer)
  (let ((deps (java-print-dependencies interface)))
    (if deps
	(progn
	  (princ "\nimport " buffer)
	  (princ deps buffer)
	  (princ ";\n" buffer))))

  (princ "\n\ninterface " buffer)
  (princ (java-interface-name interface) buffer)
  (princ '{ buffer)
  (princ "\n" buffer)
  (java-print-method-list (java-interface-method-list interface) buffer)
  (princ '} buffer)) 

(defun java-print-package (package path)
  (let* ((new-path (concat (concat path (java-package-name package)) "/"))
	 (command (concat "mkdir " new-path)))
    (progn
      (shell-command-to-string command)
      (loop for class in (java-package-class-list package)
	    do 
	    (let ((buffer (generate-new-buffer 
			   (concat new-path (java-class-name class)))))
	      (java-print-class class buffer)
	      (set-buffer buffer)
	      (save-current-buffer)))
     (loop for interface in (java-package-interface-list package)
	    do 
	    (let ((buffer (generate-new-buffer 
			   (concat new-path (java-interface-name interface)))))
	      (java-print-interface interface buffer)
	      (set-buffer buffer)
	      (save-current-buffer))))))

(defun java-print-all (stub-directory)
  (let ((path (concat stub-directory "/")))
    (loop for package being each hash-value of *java-package-hash-table*
	 do (java-print-package package path))))

(defun java-ptest ()
  (java-print-all "/opt/src/vjojic/build/tests/java/stubs"))

(defun java-test ()
  (progn
    (clrhash *java-class-hash-table*)
    (clrhash *java-interface-hash-table*)
    (clrhash *java-package-hash-table*)
    (loop for protocol being each hash-value of  *protocol-hash-table* 
	  do (progn
	       (java-derive-classes protocol)))
    (java-print-all "/opt/src/vjojic/build/tests/java/stubs")))





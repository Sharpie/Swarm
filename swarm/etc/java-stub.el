(require 'cl)
(load (concat (getenv "SWARMSRCDIR") "etc/protocol.el"))

(defun java-is-creatable (protocol)
  (loop for iprotocol in (protocol-included-protocol-list protocol)
	do 
	(if (string= (protocol-name iprotocol) "CREATABLE")
	  (return  t)
	  nil)))

(defun java-print-protocol-name (protocol buffer)
  (princ (protocol-name protocol) buffer))
  
(defun java-print-type (type buffer)
  (if type
      (cond ( (eq (compare-strings type 0 2 "id" 0 2) t) 
	      (princ 'Object buffer))
	    ( (string= type "void") (princ "void" buffer))
	    ( (string= type "const char *") (princ "String" buffer))
	    ( (string= type "char *") (princ "String" buffer))
	    ( (string= type "int") (princ "int" buffer))
	    ( (string= type "long") (princ "long" buffer))
	    ( (string= type "double") (princ "double" buffer))
	    ( (string= type "float") (princ "float" buffer))
	    ( (string= type "unsigned") (princ "int" buffer))
	    ( (string= type "long double") (princ "double" buffer))
	    ( (string= type "unsigned long long int") (princ "long" buffer))
	    ( (string= type "BOOL") (princ "boolean" buffer))
	    ( (string= type "void *") (princ "Object" buffer))
	    ( (string= type "ref_t") (princ "Object" buffer))
	    ( (string= type "val_t") (princ "Object" buffer))
	    ( (string= type "Protocol *") (princ "Class" buffer))
	    ( (string= type "SEL") (princ "java.lang.reflect.Method" buffer))
	    ( (string= type "notify_t") (princ "Notify_t" buffer))
	    
	    ( t (princ type buffer)))
    (princ 'Object buffer)))

(defun java-print-argument (argument buffer)
  (progn
    (java-print-type (car (cdr argument)) buffer)
    (princ " " buffer)
    (princ (car (cdr (cdr argument))) buffer)))


(defun java-print-method (method buffer)
  (let ((arguments  (car (method-arguments method))))
    (java-print-type (method-return-type  method) buffer)
    (princ " " buffer)
    (princ (car arguments) buffer)
    (princ " (" buffer)
    (java-print-argument arguments buffer)	   	
    (loop for argument in (cdr (method-arguments method))
	      do
	      (progn
		(princ ", " buffer)
		(java-print-argument argument buffer)))		  
    (princ ");\n" buffer)))


(defun java-print-methods-in-protocol (protocol buffer)
  (loop for method in (protocol-method-list protocol) 
	do (java-print-method method buffer)))


(defun java-print-methods-in-phase (protocol prefix phase buffer)
  (loop for method in (protocol-method-list protocol) 
	do
	(if (eq (method-phase method) phase)
	    (progn 
	      (princ prefix buffer)
	      (java-print-method method buffer)))))

(defun java-print-all-methods (protocol buffer)
  (progn 
    (loop for iprotocol in (protocol-included-protocol-list  protocol)
	  do (java-print-all-methods iprotocol buffer)
    (java-print-methods-in-phase protocol " " :creating buffer)
    (java-print-methods-in-phase protocol " " :setting  buffer)
    (java-print-methods-in-phase protocol " " :using    buffer))))

(defun java-print-methods (protocol is-class)
  (progn 
    (loop for iprotocol in (protocol-included-protocol-list  protocol)
	  do (java-print-methods iprotocol is-class))
    (java-print-methods-in-protocol protocol is-class)))

(defun java-print-protocol-list-with-suffix (protocol-list separator suffix buffer)
  (progn 
    (let ((first t))
      (loop for protocol in protocol-list
	    do  (progn
		  (if first
		      (setq first nil)
		    (princ separator buffer))
		  (java-print-protocol-name protocol buffer)
		  (princ suffix buffer))))))
  
(defun java-print-implemented-protocols (protocol phase separator buffer)
  (if (protocol-included-protocol-list protocol)
      (progn 
	(if (or (java-is-creatable protocol) 
		(eq phase :creating)
		(eq phase :using ))
	    (princ "implements " buffer)
	  (princ "extends " buffer))
	(java-print-implemented-protocols-list protocol phase separator 
					       buffer))))
	

(defun java-print-implemented-protocols-list (protocol phase separator buffer)
  (if (protocol-included-protocol-list protocol)
      (let ((first t))
	(if phase
	    (case phase
	      (:setting 
	       (loop for iprotocol in 
		     (protocol-included-protocol-list protocol)
		     do (progn
			  (if first
			      (setq first nil)
			    (princ separator buffer))
			  (princ (protocol-name iprotocol) buffer)
			  (if (java-is-creatable iprotocol)
			      (princ "-s" buffer)))))
	      (:creating
	       (loop for iprotocol in 
		     (protocol-included-protocol-list protocol)
		     do (progn
			  (if first
			      (setq first nil)
			    (princ separator buffer))
			  (princ (protocol-name iprotocol) buffer)
			  (if (java-is-creatable iprotocol)
			      (progn 
				(princ "-s" buffer)
				(princ separator buffer)
				(princ (protocol-name iprotocol) buffer))))))
	      (:using
	       (loop for iprotocol in 
		     (protocol-included-protocol-list protocol)
		     do (progn
			  (if first
			      (setq first nil)
			    (princ separator buffer))
			  (princ (protocol-name iprotocol) buffer)
			  (if (java-is-creatable iprotocol)
			      (progn 
			  (princ "-s" buffer)
			  (princ separator buffer)
			  (princ (protocol-name iprotocol) buffer)
			  (princ "-u" buffer)))))))
	  (loop for iprotocol in (protocol-included-protocol-list protocol)
		do  (progn
		      (if first
			  (setq first nil)
			(princ separator buffer))
		  (java-print-protocol-name protocol buffer)
		  (princ separator buffer)))))))
  
   
(defun java-print-interface (protocol stub-directory)
  (let* 
       ((file-name (concat 
		    (concat stub-directory (protocol-name protocol)) ".java"))
	(buffer (generate-new-buffer file-name)))
    (princ "interface " buffer)
    (java-print-protocol-name protocol buffer)
    (princ " " buffer)
    (java-print-implemented-protocols protocol nil ", " buffer)
    (print '{ buffer)
    (java-print-methods-in-protocol protocol buffer)
    (print '} buffer)
    (set-buffer buffer)
    (write-file file-name)))

(defun java-print-interface-setting (protocol stub-directory)
   (let* 
       ((file-name (concat (concat stub-directory 
				   (protocol-name protocol)) "-s.java"))
	(buffer (generate-new-buffer file-name)))
    (princ "interface " buffer)
    (java-print-protocol-name protocol buffer)
    (princ "-s " buffer)
    (java-print-implemented-protocols protocol :setting ", " buffer)
    (print '{ buffer)
    (java-print-methods-in-phase protocol " " :setting buffer)
    (print '} buffer)
    (set-buffer buffer)
    (write-file file-name)))

(defun java-print-class-creating (protocol stub-directory)
 (let* 
       ((file-name (concat (concat stub-directory 
				   (protocol-name protocol)) ".java"))
	(buffer (generate-new-buffer file-name)))
    (princ "class " buffer)
    (java-print-protocol-name protocol buffer)
    (princ " " buffer)
    (java-print-implemented-protocols protocol :creating ", " buffer)
    (princ ", " buffer)
    (princ (protocol-name protocol) buffer)
    (princ "-s " buffer)
    (print '{ buffer)
    (java-print-methods-in-phase protocol "native " :creating buffer)
    (java-print-methods-in-phase protocol "native " :setting buffer)
    (print '} buffer)
    (set-buffer buffer)
    (write-file file-name)))

(defun java-print-class-using (protocol stub-directory)
 (let* 
       ((file-name (concat (concat stub-directory 
				   (protocol-name protocol)) "-u.java"))
	(buffer (generate-new-buffer file-name)))
    (princ "class " buffer)
    (java-print-protocol-name protocol buffer)
    (princ "-u " buffer)
    (java-print-implemented-protocols protocol :using ", " buffer)
    (princ ", " buffer)
    (princ (protocol-name protocol) buffer)
    (princ "-s " buffer)
    (print '{ buffer)
    (java-print-methods-in-phase protocol "native " :setting buffer)
    (java-print-methods-in-phase protocol "native " :using buffer)
    (print '} buffer)
    (set-buffer buffer)
    (write-file file-name)))

(defun java-print-makefile (protocol phase makefile-buffer)
  (progn
    (princ "\n" makefile-buffer)
    (java-print-protocol-name protocol makefile-buffer)
    (case phase
	(:creating (princ ".class" makefile-buffer))
	(:setting (princ "-s.class" makefile-buffer))
	(:using (princ "-u.class" makefile-buffer)))
    (princ ": " makefile-buffer)
    (java-print-protocol-name protocol makefile-buffer)
    (princ "-s.java " makefile-buffer)
    (case phase
      (:creating 
	 (java-print-protocol-list-with-suffix 
	  (protocol-included-protocol-list protocol)
	  " " ".class" makefile-buffer))
       (:using
	(java-print-protocol-list-with-suffix 
	  (protocol-included-protocol-list protocol)
	  " " "-u.class" makefile-buffer)))
    (java-print-protocol-list-with-suffix 
     (protocol-included-protocol-list protocol)
     " " "-s.class" makefile-buffer))
    (princ "\n\t" makefile-buffer)
    (princ "javac " makefile-buffer)
    (princ (protocol-name protocol) makefile-buffer)
    (case phase
	(:creating (princ ".java" makefile-buffer))
	(:setting (princ "-s.java" makefile-buffer))
	(:using (princ "-u.java" makefile-buffer))))



(defun java-print-class (protocol stub-directory makefile-buffer)
    (if (java-is-creatable protocol)
      (progn
	(java-print-interface-setting protocol stub-directory)
	(java-print-class-using protocol stub-directory)
	(java-print-class-creating protocol stub-directory)
	(java-print-makefile protocol t makefile-buffer)
	(java-print-makefile protocol nil makefile-buffer))
      (java-print-interface protocol stub-directory)))


(defun java-print-classes (stub-directory)
  (let* ((makefile-name (concat stub-directory "Makefile"))
	(makefile-buffer (generate-new-buffer makefile-name)))
    (loop for protocol being each hash-value of *protocol-hash-table* 
	  do 
	      (java-print-class protocol stub-directory makefile-buffer))
    (set-buffer makefile-buffer)
    (write-file makefile-name)))






(require 'cl)
(load (concat (getenv "SWARMSRCDIR") "/etc/protocol.el"))

(defvar *last-protocol*)
(defconst *stub-directory* "/tmp/stubs/")

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

(defun java-print (str)
  (insert str))

(defun java-protocol-creatable-p (protocol)
  (find "CREATABLE" 
        (protocol-included-protocol-list protocol)
        :test #'string=
        :key #'protocol-name))

(defun java-print-protocol-name (protocol)
  (java-print (protocol-name protocol)))

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
        (java-print "FreakyType")
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


(defun java-print-methods-in-protocol (protocol)
  (setq *last-protocol* protocol)
  (mapcar #'java-print-method (protocol-method-list protocol)))

(defun java-print-methods-in-phase (protocol prefix phase)
  (loop for method in (protocol-method-list protocol) 
	do
        (when (eq (method-phase method) phase)
          (java-print prefix)
          (java-print-method method))))

(defun java-print-all-methods (protocol)
  (loop for iprotocol in (protocol-included-protocol-list protocol)
        do
        (java-print-all-methods iprotocol)
        (java-print-methods-in-phase protocol " " :creating)
        (java-print-methods-in-phase protocol " " :setting)
        (java-print-methods-in-phase protocol " " :using)))

(defun java-print-methods (protocol is-class)
  (loop for iprotocol in (protocol-included-protocol-list protocol)
        do (java-print-methods iprotocol is-class))
  (java-print-methods-in-protocol protocol is-class))

(defun java-print-implemented-protocols (protocol phase separator)
  (when (protocol-included-protocol-list protocol)
    (if (or (java-protocol-creatable-p protocol) 
            (eq phase :creating)
            (eq phase :using))
        (java-print "implements ")
        (java-print "extends "))
    (java-print-implemented-protocols-list protocol phase separator "")))

(defun java-print-implemented-protocols-list (protocol phase separator suffix)
  (let ((first t)
        (included-protocols (protocol-included-protocol-list protocol)))
    (flet ((protocol-loop (func)
             (loop for iprotocol in included-protocols
                   do
                   (if first
                       (setq first nil)
                       (java-print separator))
                   (java-print (protocol-name iprotocol))
                   (funcall func iprotocol)
                   (java-print suffix))))
      (if phase
          (case phase
            (:setting 
             (protocol-loop #'(lambda (iprotocol)
                              (when (java-protocol-creatable-p iprotocol)
                                (java-print "-s")))))
            (:creating
             (protocol-loop #'(lambda (iprotocol)
                              (when (java-protocol-creatable-p iprotocol)
                                (java-print "-s")
                                (java-print suffix)
                                (java-print separator)
                                (java-print (protocol-name iprotocol))))))
            (:using
             (protocol-loop #'(lambda (iprotocol)
                              (when (java-protocol-creatable-p iprotocol)
                                (java-print "-s")
                                (java-print suffix)
                                (java-print separator)
                                (java-print (protocol-name iprotocol))
                                (java-print "-u"))))))
          (protocol-loop #'(lambda (iprotocol)))))))

(defun stub-protocol-path (protocol &optional suffix)
  (let ((path (concat *stub-directory* (protocol-name protocol))))
    (when suffix
      (setq path (concat path suffix)))
    (setq path (concat path ".java"))
    path))

(defun java-print-interface (protocol)
  (with-temp-file (stub-protocol-path protocol)
    (java-print "interface ")
    (java-print-protocol-name protocol)
    (java-print " ")
    (java-print-implemented-protocols protocol nil ", ")
    (java-print "\n{\n")
    (java-print-methods-in-protocol protocol)
    (java-print "}\n")))

(defun java-print-interface-setting (protocol)
  (with-temp-file (stub-protocol-path protocol "-s")
    (java-print "interface ")
    (java-print-protocol-name protocol)
    (java-print "-s ")
    (java-print-implemented-protocols protocol :setting ", ")
    (java-print "\n{\n")
    (java-print-methods-in-phase protocol " " :setting)
    (java-print "}\n")))

(defun java-print-class-creating (protocol)
   (with-temp-file (stub-protocol-path protocol)
     (java-print "class ")
     (java-print-protocol-name protocol)
     (java-print " ")
     (java-print-implemented-protocols protocol :creating ", ")
     (java-print ", ")
     (java-print (protocol-name protocol))
     (java-print "-s ")
     (java-print "\n{\n")
     (java-print-methods-in-phase protocol "native " :creating)
     (java-print-methods-in-phase protocol "native " :setting)
     (java-print "}\n")))

(defun java-print-class-using (protocol)
  (with-temp-file (stub-protocol-path protocol "-u")
    (java-print "class ")
    (java-print-protocol-name protocol)
    (java-print "-u ")
    (java-print-implemented-protocols protocol :using ", ")
    (java-print ", ")
    (java-print (protocol-name protocol))
    (java-print "-s ")
    (java-print "\n{\n")
    (java-print-methods-in-phase protocol "native " :setting)
    (java-print-methods-in-phase protocol "native " :using)
    (java-print "\n}\n")))

(defun java-print-makefile (protocol)
  (java-print "\n")
  (if (java-protocol-creatable-p protocol)
      (progn
        (java-print-protocol-name protocol)
        (java-print ".class: ")
        (java-print-protocol-name protocol)
        (java-print ".java ")
        (java-print-protocol-name protocol)
        (java-print "-s.class ")
        (java-print-implemented-protocols-list protocol :creating " " ".class")
        (java-print "\n\t javac ")
        (java-print (protocol-name protocol))
        (java-print ".java\n")
        
        (java-print-protocol-name protocol)
        (java-print "-u.class: ")
        (java-print-protocol-name protocol)
        (java-print "-u.java ")
        (java-print-protocol-name protocol)
        (java-print "-s.class ")
        (java-print-implemented-protocols-list protocol :using " " ".class")
        (java-print "\n\t javac ")
        (java-print (protocol-name protocol))
        (java-print ".java\n")
        
        (java-print-protocol-name protocol)
        (java-print "-s.class: ")
        (java-print-protocol-name protocol)
        (java-print "-s.java ")
        (java-print-implemented-protocols-list
         protocol :setting " " ".class")
        (java-print "\n\t javac ")
        (java-print (protocol-name protocol))
        (java-print ".java\n"))
      (progn 
        (java-print-protocol-name protocol)
        (java-print ".class: ")
        (java-print-implemented-protocols-list protocol nil " " ".class")
        (java-print "\n\t javac ")
        (java-print (protocol-name protocol))
        (java-print ".java\n"))))

(defun java-print-class (protocol makefile-buffer)
  (if (java-protocol-creatable-p protocol)
      (progn
	(java-print-interface-setting protocol)
	(java-print-class-using protocol)
	(java-print-class-creating protocol))
      (java-print-interface protocol))
  (with-current-buffer makefile-buffer
    (java-print-makefile protocol)))

(defun java-print-classes ()
  (interactive)
  (let* ((makefile-name (concat *stub-directory* "Makefile"))
         (makefile-buffer (generate-new-buffer makefile-name)))
    (loop for protocol being each hash-value of *protocol-hash-table* 
	  do (java-print-class protocol makefile-buffer))
    (set-buffer makefile-buffer)
    (write-file makefile-name)
    (kill-buffer makefile-buffer)))


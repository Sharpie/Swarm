(require 'cl)
(load (concat (getenv "SWARMDOCS") "common.el"))

(defvar *protocol-hash-table* (make-hash-table :test #'equal))

(defvar *module-hash-table* (make-hash-table))

(defconst *phases* '(:creating :setting :using))

(defvar *protocol-list*)

(defvar *method-signature-hash-table* (make-hash-table :test #'equal))

(defvar *method-signature-list*)

(defvar *general-example-counter-hash-table* (make-hash-table :test #'eq))

(defvar *method-example-counter-hash-table* (make-hash-table :test #'equal))

(defstruct protocol
  module
  name
  summary
  description-list
  included-protocol-list
  macro-list
  global-list
  example-list
  method-list
  expanded-methodinfo-list)

(defstruct method
  phase
  factory-flag
  return-type
  arguments
  description-list
  example-list)

(defstruct named-object
  name
  description-list)

(defconst *doc-types* '(:method-doc :summary-doc :description-doc
                        :macro-doc
                        :global-doc :global-begin :global-end :global-break))

(defun find-protocol ()
  (interactive)
  (re-search-forward "^@\\(protocol\\|deftype\\)" nil t))

(defun skip-whitespace ()
  (skip-chars-forward " \t\r\n"))

(defun skip-name ()
  (skip-chars-forward "[a-zA-Z_.][a-zA-Z0-9_.]")
  (point))

(defun next-paren-expr ()
  (when (looking-at "(")
    (let ((beg (point)))
      (forward-sexp)
      (buffer-substring (1+ beg) (- (point) 1)))))

(defun next-expr ()
  (list
   (progn
     (skip-whitespace)
     (next-paren-expr))
   (progn
     (skip-whitespace)
     (next-name))))

(defun end-of-line-position ()
  (save-excursion
    (end-of-line)
    (point)))

(defun parse-included-protocol-list ()
  (let ((eolpos (end-of-line-position)))
    (loop
     with beg = (search-forward "<" eolpos t)
     while beg
     for end = (re-search-forward "[ \t,>]" eolpos t)
     unless end do (error "Bad protocol syntax")
     do (backward-char)
     for next = (cond ((looking-at "[ \t,>]") 
                       (skip-chars-forward ", \t")
                       (if (looking-at ">")
                           nil
                         (point)))
                      (t (point)))
     collect (buffer-substring beg (- end 1))
     and do (setq beg next))))

(defun next-name ()
  (let* ((beg (point))
         (end (skip-name)))
    (prog1
        (buffer-substring beg end)
      (skip-whitespace))))

(defun parse-method (protocol-name
                     phase
                     factory-flag
                     method-description-list
                     method-example-list)
  (forward-char)
  (skip-whitespace)
  (let* ((return-type (next-paren-expr))
         arguments name)
    (loop
     unless (looking-at ":")
     do
     (setq name (next-name))
     (when (looking-at ";")
       (push (cons name nil) arguments))
     
     when (looking-at ":")
     do
     (forward-char)
     (push (cons name (next-expr)) arguments)
     (while (looking-at ",")
       (forward-char)
       (push (cons nil (next-expr)) arguments))
     (when (looking-at "//")
       (beginning-of-line 2))
     until (looking-at ";"))
    (unless phase
      (error "No phase in protocol: %s" protocol-name))
    (make-method
     :phase phase
     :factory-flag factory-flag
     :arguments (reverse arguments)
     :return-type return-type
     :description-list method-description-list
     :example-list method-example-list)))

(defun test-parse-method ()
  (interactive)
  (let ((method (parse-method nil :creating nil nil nil)))
    (princ (list (method-return-type method)
                 (method-arguments method)
                 (method-example-list method)
                 ))))

(defun line-text ()
  (buffer-substring (point) (end-of-line-position)))

(defun general-example-counter (protocol)
  (let ((index
         (let ((val (gethash protocol *general-example-counter-hash-table*)))
           (if val
               (progn
                 (incf (gethash protocol *general-example-counter-hash-table*))
                 val)
               (progn
                 (setf (gethash protocol *general-example-counter-hash-table*) 1)
                 0)))))
    (1+ index)))
        
(defun method-example-counter (protocol method)
  (let ((index
         (let* ((key (cons protocol method))
                (val (gethash key *general-example-counter-hash-table*)))
           (if val
               (progn
                 (incf (gethash key *general-example-counter-hash-table*))
                 val)
               (progn
                 (setf (gethash key *general-example-counter-hash-table*) 1)
                 0)))))
    (1+ index)))

(defun update-extern ()
  (unless (save-excursion
            (beginning-of-line)
            (looking-at "\\s-+//G:"))
    (setq extern-name
          (save-excursion
            (backward-word 1) 
            (let ((beg (point)))
              (forward-word 1)
              (buffer-substring beg (point)))))))

(defun load-protocol (module)
  (interactive)
  (skip-whitespace)
  (let* ((name
          (let ((beg (point)))
            (skip-name)
            (buffer-substring beg (point))))
         (included-protocol-list
          (parse-included-protocol-list))
         (last-tag nil)
         (description-doc-list nil)
         (summary-doc nil)
         (method-doc-list nil)
         (macro-doc-list nil)
         (global-doc-list nil)
         (global-list nil)
         (macro-list nil)
         (method-list nil)
         (global-example-list nil)
         (example-list nil)
         (phase nil)
         (tag nil)
         (macro-sig nil)
         (extern-sig nil)
         (extern-name nil)
         (buf nil))
    (beginning-of-line 1)
    (while (and (zerop (forward-line 1)) (not (eq tag :end)))
      (beginning-of-line)
      (cond ((looking-at "CREATING")
             (setq tag :creating)
             (setq phase :creating))
            ((looking-at "SETTING")
             (setq phase :setting)
             (setq tag :setting))
            ((looking-at "USING")
             (setq phase :using)
             (setq tag :using))
            ((looking-at "[ \t]*$") (setq tag :newline))
            ((looking-at "-")
             (setq tag :method))
            ((looking-at "+")
             (setq tag :factory-method))
            ((looking-at "//M:") 
             (setq tag :method-doc))
            ((looking-at "//S:") (setq tag :summary-doc))
            ((looking-at "//D:") (setq tag :description-doc))
            ((looking-at "//E:") (setq tag :example-doc))
            ((looking-at "//#:") (setq tag :macro-doc))
            ((looking-at "//G:") (setq tag :global-doc))
            ((looking-at ".+//G:")
             (setq tag :global-doc)
             (search-forward "//G:")
             (backward-char 4)
             (let ((last-extern extern-name))
               (update-extern)
               (cond ((save-excursion
                        (beginning-of-line)
                        (looking-at ".*;.*//G:"))
                      (setq tag :global-end))
                     ((not (string= last-extern extern-name))
                      (setq tag :global-break)))))
            ((looking-at "#if 0")
             (c-forward-conditional 1)
             (beginning-of-line 0))
            ((looking-at "@end") (setq tag :end))
            ((looking-at "// ") (setq tag :comment))
            ((looking-at "#define\\s-+\\([^(]+\\)\\((.*)\\)?\\s-")
             (setq tag :macro)
             (setq macro-sig (concat (match-string 1) (match-string 2))))
            ((looking-at "extern\\s-")
             (setq tag :global)
             (re-search-forward "\\s-+")
             (setq extern-sig
                   (let ((beg (point)))
                     (re-search-forward "\\(;\\|//G:\\)")
                     (backward-char)
                     (let ((have-global-tag (looking-at ":")))
                       (if have-global-tag
                           (setq tag :global-begin)
                           (progn
                             (setq have-global-tag
                                   (search-forward "//G:"
                                                   (end-of-line-position) t))
                             (when have-global-tag
                               (setq tag :global-end)
                               (backward-char))))
                       (when have-global-tag
                         (backward-char 3)
                         (update-extern)
                         (backward-word 1)
                         (skip-chars-backward " \t\r\n")))
                     (buffer-substring beg (point))))
             (when (or (eq tag :global-begin) (eq tag :global-end))
               (search-forward "//G: ")
               (backward-char 5)))
            ((looking-at "///M:") (setq tag :bogus-method))
            (t (error "Unknown text: [%s]"
                      (buffer-substring (point) (end-of-line-position)))))
      (let ((line (line-text))
            (is-doc-type (member tag *doc-types*)))
        (flet ((extract-doc-string (str)
                 (if (> (length str) 5)
                     (substring str 5)
                     "")))
          (if (member tag '(:global-begin :global-end :global-break))
              (push (extract-doc-string line) global-doc-list)
              
              (if (eq tag last-tag)
                  (if is-doc-type
                      (setq buf (concat 
                                 (if (string-match " $" buf) 
                                     buf
                                     (concat buf " "))
                                 (extract-doc-string line)))
                      (when (eq tag :example-doc)
                        (setq buf
                              (concat buf "\n" (extract-doc-string line)))))
                  (progn
                    (case last-tag
                      (:example-doc
                       (push (concat buf "\n") example-list)
                       (unless (or method-list method-doc-list)
                         (setq global-example-list example-list)
                         (setq example-list nil)))
                      (:method-doc
                       (push buf method-doc-list))
                      (:macro-doc
                       (push buf macro-doc-list))
                      (:global-doc
                       (push buf global-doc-list))
                      (:global
                       (push buf global-doc-list))
                      (:summary-doc (if summary-doc
                                        (error "summary already set")
                                        (setq summary-doc buf)))
                      (:description-doc (push buf description-doc-list)))
                    (when (or is-doc-type (eq tag :example-doc))
                      (setq buf (extract-doc-string line))))))))

      (cond ((member tag '(:method :factory-method))
             (push (parse-method name
                                 phase
                                 (eq tag :factory-method)
                                 (reverse method-doc-list)
                                 (reverse example-list))
                   method-list)
             (setq method-doc-list nil))
            ((eq tag :macro)
             (while (looking-at ".*\\\\\\s-*$")
               (forward-line))
             (push (make-named-object
                    :name macro-sig
                    :description-list macro-doc-list)
                   macro-list)
             (setq macro-doc-list nil))
            ((member tag '(:global :global-break :global-begin :global-end))
             (push (make-named-object
                    :name (concat extern-sig " " extern-name)
                    :description-list global-doc-list)
                   global-list)
             (setq global-doc-list nil)))
      
      (setq last-tag tag))

    (make-protocol
     :module module
     :name name
     :example-list (reverse global-example-list)
     :included-protocol-list included-protocol-list
     :summary summary-doc
     :description-list (reverse description-doc-list)
     :macro-list (reverse macro-list)
     :global-list (reverse global-list)
     :method-list (reverse method-list))))

(defun load-protocols (module)
  (interactive)
  (loop
   while (find-protocol)
   collect (load-protocol module)))
   
(defun create-included-protocol-list (protocol)
  (loop for included-protocol-name in (protocol-included-protocol-list protocol)
        for included-protocol = (get-protocol included-protocol-name)
        unless included-protocol do (error "Could not find protocol %s"
                                           included-protocol-name)
        collect included-protocol))

(defun CREATABLE-protocol ()
  (let ((description "Declare that a defined type supports creation."))
    (make-protocol
     :module 'defobj
     :name "CREATABLE"
     :included-protocol-list nil
     :summary description
     :description-list (list description)
     :method-list nil)))

(defun add-protocol (module protocol)
  (setf (gethash (protocol-name protocol) *protocol-hash-table*) protocol)
  (push protocol (gethash module *module-hash-table*)))

(defun get-module (module-spec)
  (if (consp module-spec) (car module-spec) module-spec))

(defun load-protocols-for-all-modules ()
  (interactive)

  (let ((old-push-mark (symbol-function 'push-mark)))

    (when noninteractive
      (setf (symbol-function 'push-mark)
            #'(lambda () 
                (funcall old-push-mark nil t))))

    (clrhash *protocol-hash-table*)
    (clrhash *module-hash-table*)
    (add-protocol 'defobj (CREATABLE-protocol))
    (loop for module-spec in *swarm-modules*
          for module = (get-module module-spec)
          do
          (if (consp module-spec)
              (find-file-read-only 
               (pathname-for-module module (cdr module-spec)))
              (find-file-read-only (pathname-for-module module)))
          (loop for protocol in (load-protocols module)
                for name = (protocol-name protocol)
                for exist = (gethash name *protocol-hash-table*)
                when exist do (error "Protocol %s already exists" name)
                do (add-protocol module protocol))
          (kill-buffer (current-buffer)))
    
    (when noninteractive
      (setf (symbol-function 'push-mark) old-push-mark))
    
    (loop for protocol being each hash-value of *protocol-hash-table*
          do
          (setf (protocol-included-protocol-list protocol)
                (create-included-protocol-list protocol)))))
  
(defun get-protocol (name)
  (gethash name *protocol-hash-table*))

(defun compare-string-lists (a b)
  (let ((diff
         (loop for a-arg in a
               for b-arg in b
               if (string< a-arg b-arg) return -1
               else if (not (string= a-arg b-arg)) return 1
               finally return 0)))
    (if (zerop diff)
        (< (length a) (length b))
        diff)))

(defun generate-expanded-methodinfo-list (protocol)
  (let ((expanded-protocols-hash-table (make-hash-table))
        (method-hash-table (make-hash-table)))
    (flet ((expand-protocol-level (protocol level)
             (setf (gethash protocol expanded-protocols-hash-table) t)
             (loop for method in (protocol-method-list protocol)
                   do (setf (gethash method method-hash-table) (cons level protocol)))
             (loop for included-protocol in
                   (protocol-included-protocol-list protocol)
                   do
                   (unless (gethash included-protocol expanded-protocols-hash-table)
                     (expand-protocol-level included-protocol (1+ level))))))
      (expand-protocol-level protocol 0))
    (sort 
     (loop for method being each hash-key of method-hash-table using (hash-value level.protocol)
           collect (list (car level.protocol)
                         (cdr level.protocol)
                         method))
     #'(lambda (a b)
         (flet ((phase-pos (phase)
                  (case phase
                    (:creating 0)
                    (:setting 1)
                    (:using 2)))
                (compare-arguments (a b)
                  (flet ((get-key-list (item) (mapcar #'first item)))
                    (compare-string-lists
                     (get-key-list a)
                     (get-key-list b)))))
           (let ((level-diff (- (first a) (first b))))
             (if (zerop level-diff)
                 (let* ((method-a (third a))
                        (method-b (third b))
                        (phase-diff (- (phase-pos (method-phase method-a))
                                       (phase-pos (method-phase method-b)))))
                   (if (zerop phase-diff)
                       (compare-arguments (method-arguments method-a)
                                          (method-arguments method-b))
                       (< phase-diff 0)))
                 (< level-diff 0))))))))

(defun generate-expanded-methodinfo-lists ()
  (interactive)
  (loop for protocol being each hash-value of *protocol-hash-table*
        do
        (setf (protocol-expanded-methodinfo-list protocol)
              (generate-expanded-methodinfo-list protocol))))

(defun external-protocol-name (protocol)
  (let ((raw-protocol-name (protocol-name protocol)))
    (if (internal-protocol-p protocol)
        (substring raw-protocol-name 1)
        raw-protocol-name)))

(defun get-method-signature (method)
  (with-output-to-string (print-method-signature method)))

(defun protocol-index (protocol)
  (position protocol *protocol-list*))

(defun method-signature-index (method-signature)
  (position method-signature *method-signature-list* :test #'string=))

(defun sgml-protocol-id (protocol)
  (let* ((cooked-protocol-name (external-protocol-name protocol)))
    (concat "SWARM."
            (upcase (symbol-name (protocol-module protocol)))
            "."
            (upcase cooked-protocol-name))))

(defun sgml-method-signature-id (protocol phase method-signature)
  (concat (sgml-protocol-id protocol)
          (format ".P%s.M%d" 
                  (case phase
                    (:creating "C")
                    (:setting "S")
                    (:using "U")
                    (otherwise (error "bad phase")))
                  (method-signature-index method-signature))))

(defun sgml-refentry-start (protocol)
  (insert "<REFENTRY ID=\"")
  (insert (sgml-protocol-id protocol))
  (insert "\">\n"))

(defun sgml-refmeta (protocol)
  (insert "<REFMETA>\n")
  (insert "<REFENTRYTITLE>")

  (insert (protocol-name protocol))
  
  (insert "</REFENTRYTITLE>\n")
  (insert "<REFMISCINFO>")
  (insert (symbol-name (protocol-module protocol)))
  (insert "</REFMISCINFO>\n")
  (insert "</REFMETA>\n"))

(defun sgml-namediv (protocol)
  (insert "<REFNAMEDIV>\n")
  (insert "<REFNAME>")
  (insert (protocol-name protocol))
  (insert "</REFNAME>\n")
  (insert "<REFPURPOSE>\n")
  (insert (protocol-summary protocol))
  (insert "\n</REFPURPOSE>\n")
  (insert "</REFNAMEDIV>\n"))

(defun insert-text (text)
  (when text
    (let ((beg (point)))
      (insert text)
      (let ((end (point)))
        (save-excursion
          (save-restriction
            (narrow-to-region beg end)
            (goto-char (point-min))
            (save-excursion
              (while (search-forward "<" nil t)
                (replace-match "&lt;")))
            (save-excursion
              (while (search-forward ">" nil t)
                (replace-match "&gt;")))))))))

(defun sgml-refsect1-text-list (title text-list)
  (when text-list
    (insert "<REFSECT1>\n")
    (insert "<TITLE>")
    (insert-text title)
    (insert "</TITLE>\n")
    (loop for text in text-list
          do 
          (insert "<PARA>\n")
          (insert-text text)
          (insert "\n</PARA>\n"))
    (insert "</REFSECT1>\n")))

(defun sgml-refsect1-description (protocol)
  (sgml-refsect1-text-list "Description"
                           (protocol-description-list protocol)))

(defun sgml-method-description (protocol method)
  (let ((descriptions (method-description-list method)))
    (insert "<FUNCSYNOPSISINFO>\n")
    (insert "<CLASSNAME>")
    (insert (protocol-name protocol))
    (insert "</CLASSNAME>\n")
    (loop for description in descriptions
          do
          (insert-text description)
          (insert "\n"))
    (insert "</FUNCSYNOPSISINFO>\n")))

(defun print-method-signature (method &optional stream)
  (if (method-factory-flag method)
      (princ "+" stream)
      (princ "-" stream))
  (loop for arguments in (method-arguments method)
        for key = (first arguments)
        when key 
        do
        (princ key stream)
        (when (third arguments)
          (princ ":" stream))))

(defun sgml-method-funcsynopsis (owner-protocol method)
  (insert "<FUNCSYNOPSIS ID=\"")
  (insert (sgml-method-signature-id owner-protocol
                                    (method-phase method)
                                    (get-method-signature method)))
  (insert "\">\n")
  (insert "<FUNCPROTOTYPE>\n")
  (insert "<FUNCDEF>")
  (let ((return-type (method-return-type method)))
    (when return-type
      (insert-text return-type)))
  (insert "<FUNCTION>")
  (print-method-signature method (current-buffer))
  (insert "</FUNCTION>")
  (insert "</FUNCDEF>\n")
  (let ((arguments (method-arguments method)))
    (if (and (eql (length arguments) 1)
             (null (third (first arguments))))
        (insert "<VOID>\n")
        (loop for arg in arguments
              do
              (let ((argname (third arg)))
                ;; In the no-argument case, argname will be nil.
                (when argname
                  (insert "<PARAMDEF>")
                  (insert-text (second arg))
                  (insert "<PARAMETER>")
                  (insert argname)
                  (insert "</PARAMETER>")
                  (insert "</PARAMDEF>\n"))))))
  (insert "</FUNCPROTOTYPE>\n")
  (sgml-method-description owner-protocol method)
  (insert "</FUNCSYNOPSIS>\n"))

(defun sgml-link-to-protocol (protocol)
  (insert "<LINK LINKEND=\"")
  (insert (sgml-protocol-id protocol))
  (insert "\">")
  (insert (external-protocol-name protocol))
  (insert "</LINK>"))

(defun methodinfo-list-for-phase (protocol phase)
  (loop for methodinfo in (protocol-expanded-methodinfo-list protocol)
        when (eq (method-phase (third methodinfo)) phase)
        collect methodinfo))

(defun include-p (level protocol owner-protocol)
  (or (zerop level)
      (let ((owner-protocol-name (protocol-name owner-protocol)))
        (when (internal-protocol-p owner-protocol)
          (string=
           (substring (protocol-name owner-protocol) 1)
           (protocol-name protocol))))))

(defun count-included-methodinfo-entries (protocol phase)
  (loop for methodinfo in (methodinfo-list-for-phase protocol phase)
        count (include-p (first methodinfo)
                         protocol
                         (second methodinfo))))

(defun count-included-methodinfo-entries-for-all-phases (protocol)
  (loop for phase in *phases*
        sum (count-included-methodinfo-entries protocol phase)))

(defun sgml-method-definitions (protocol
                                phase
                                &optional protocol-listitem-flag)
  (unless (zerop (count-included-methodinfo-entries protocol phase))
    (let ((methodinfo-list (methodinfo-list-for-phase protocol phase))
          have-list have-item)
      (when protocol-listitem-flag
        (insert "<ITEMIZEDLIST>\n"))
      (loop with last-protocol = nil
            for methodinfo in methodinfo-list
            for level = (first methodinfo)
            for owner-protocol = (second methodinfo)
            for method = (third methodinfo)
            for new-group-flag = (not (eq owner-protocol last-protocol))
            
            when new-group-flag do
            (when have-list
              (insert "</ITEMIZEDLIST>\n")
              (setq have-list nil))
            (when have-item
                (insert "</LISTITEM>\n")
                (setq have-item nil))
            (when protocol-listitem-flag
              (when (include-p level protocol owner-protocol)
                (insert "<LISTITEM>\n")
                (setq have-item t)
                (insert "<PARA>")
                (insert (external-protocol-name owner-protocol))
                (insert "</PARA>\n")))
            (when (include-p level protocol owner-protocol)
              (setq have-list t)
              (insert "<ITEMIZEDLIST>\n"))
              
            do
            (when (include-p level protocol owner-protocol)
              (insert "<LISTITEM>\n")
              (sgml-method-funcsynopsis owner-protocol method)
              (sgml-method-examples owner-protocol method)
              (insert "</LISTITEM>\n"))
            
            for last-protocol = owner-protocol)
      (when have-list
        (insert "</ITEMIZEDLIST>\n"))
      (when protocol-listitem-flag
        (when have-item
          (insert "</LISTITEM>\n"))
        (insert "</ITEMIZEDLIST>\n")))))

(defun sgml-refsect1-named-object-list (title named-object-list)
  (when named-object-list
    (insert "<REFSECT1>\n")
    (insert "<TITLE>")
    (insert-text title)
    (insert "</TITLE>\n")
    (insert "<ITEMIZEDLIST>\n")
    (loop for object in named-object-list
          do
          (insert "<LISTITEM>\n")
          (insert "<PARA>")
          (insert-text (named-object-name object))
          (insert "</PARA>\n")
          (loop for text in (named-object-description-list object)
                do
                (insert "<PARA>")
                (insert-text text)
                (insert "</PARA>\n"))
          (insert "</LISTITEM>\n"))
    (insert "</ITEMIZEDLIST>\n")))

(defun sgml-refsect1-macro-list (protocol)
  (sgml-refsect1-named-object-list "Macros"
                                   (protocol-macro-list protocol)))

(defun sgml-refsect1-global-list (protocol)
  (sgml-refsect1-named-object-list "Global Variables"
                                   (protocol-global-list protocol)))

(defun sgml-examples (protocol)
  (let ((example-list (protocol-example-list protocol)))
    (when example-list
      (insert "<EXAMPLE LABEL=\"")
      (insert (symbol-name (protocol-module protocol)))
      (insert "/")
      (insert (external-protocol-name protocol))
      (insert (format "/%d" (general-example-counter protocol)))
      (insert "\">")
      (insert "<TITLE>\n")
      (insert "</TITLE>\n")
      (loop for example in example-list
            do
            (insert "<PROGRAMLISTING>\n")
            (insert example)
            (insert "</PROGRAMLISTING>\n"))
      (insert "</EXAMPLE>\n"))))

(defun count-method-examples (protocol phase)
  (loop for methodinfo in (methodinfo-list-for-phase protocol phase)
        for method = (third methodinfo)
        count (method-example-list method)))

(defun count-noninternal-protocols (protocol)
  (loop for included-protocol in (protocol-included-protocol-list protocol)
        count (not (internal-protocol-p included-protocol))))

(defun compare-method-signatures (method-a method-b)
  (let* ((method-a-signature (get-method-signature method-a))
         (method-b-signature (get-method-signature method-b)))
    (string< method-a-signature method-b-signature)))

(defun compare-methodinfo (a b)
  (let ((protocol-name-a (protocol-name (second a)))
        (protocol-name-b (protocol-name (second b))))
    (if (string= protocol-name-a protocol-name-b)
        (compare-method-signatures (third a) (third b))
        (string< protocol-name-a protocol-name-b))))

(defun sgml-method-examples (protocol method)
  (when (method-example-list method)
    (insert "<EXAMPLE LABEL=\"")
    (insert (symbol-name (protocol-module protocol)))
    (insert "/")
    (insert (external-protocol-name protocol))
    (insert "/")
    (print-method-signature method (current-buffer))
    (insert (format "/%d" (method-example-counter protocol method)))
    (insert "\">")
    (insert "<TITLE>")
    (insert "</TITLE>\n")

    (insert "<PROGRAMLISTING>\n")

    (loop for example in (method-example-list method)
          do
          (insert example)
          (insert "\n"))
    (insert "</PROGRAMLISTING>\n")
    (insert "</EXAMPLE>\n")))

(defun sgml-methods-for-phase (protocol phase)
  (unless (zerop (count-included-methodinfo-entries protocol phase))
    (insert "<REFSECT2>\n")
    (insert "<TITLE>Phase: ")
    (insert (capitalize (substring (prin1-to-string phase) 1)))
    (insert "</TITLE>\n")
    (sgml-method-definitions protocol phase)
    (insert "</REFSECT2>\n")))

(defun sgml-refsect1-protocol-list (protocol &optional expand-flag)
  (insert "<REFSECT1>\n")
  (insert "<TITLE>Protocols adopted by ")
  (insert (protocol-name protocol))
  (insert "</TITLE>\n")
  (if (zerop (count-noninternal-protocols protocol))
      (insert "<PARA>None</PARA>\n")
      (flet ((print-expanded-protocol-list (protocol)
               (insert "<ITEMIZEDLIST>\n")
               (loop for included-protocol in
                     (protocol-included-protocol-list protocol)
                     do
                     (unless (internal-protocol-p protocol)
                       (insert "<LISTITEM>\n")
                       (insert "<PARA>")
                       (sgml-link-to-protocol included-protocol)
                       (insert "</PARA>\n")
                       (print-expanded-protocol-list included-protocol)
                       (insert "</LISTITEM>\n")))
               (insert "</ITEMIZEDLIST>\n"))
             (print-unexpanded-protocol-list (protocol)
               (insert "<PARA>")
               (loop for included-protocol in
                     (protocol-included-protocol-list protocol)
                     do
                     (unless (internal-protocol-p protocol)
                       (insert " ")
                       (sgml-link-to-protocol included-protocol)))
               (insert "</PARA>\n")))
        (if expand-flag
            (print-expanded-protocol-list protocol)
            (print-unexpanded-protocol-list protocol))))
  (insert "</REFSECT1>\n"))

(defun sgml-refsect1-method-list (protocol)
  (insert "<REFSECT1><TITLE>Methods</TITLE>\n")
  (if (zerop (count-included-methodinfo-entries-for-all-phases protocol))
      (insert "<PARA>None</PARA>\n")
      (loop for phase in *phases*
            do
            (sgml-methods-for-phase protocol phase)))
  (insert "</REFSECT1>\n"))

(defun sgml-refsect1-examples (protocol)
  (when (protocol-example-list protocol)
    (insert "<REFSECT1><TITLE>Examples</TITLE>\n")
    (sgml-examples protocol)
    (insert "</REFSECT1>\n")))

(defun internal-protocol-p (protocol)
  (string= (substring (protocol-name protocol) 0 1) "_"))

(defun generate-refentry-for-protocol (protocol)
  (unless (internal-protocol-p protocol)
    (sgml-refentry-start protocol)
    (sgml-refmeta protocol)
    (sgml-namediv protocol)
    (sgml-refsect1-description protocol)
    (sgml-refsect1-protocol-list protocol)
    (sgml-refsect1-method-list protocol)
    (sgml-refsect1-macro-list protocol)
    (sgml-refsect1-global-list protocol)
    (sgml-refsect1-examples protocol)
    (insert "</REFENTRY>\n")))

(defun sgml-generate-refentries-for-module (module)
  (loop for protocol in (sort 
                         (gethash module *module-hash-table*)
                         (lambda (protocol-a protocol-b)
                           (string< (protocol-name protocol-a)
                                    (protocol-name protocol-b))))
        do
        (generate-refentry-for-protocol protocol)))

(defun sgml-create-refentries-for-module (module)
  (let ((module-name (symbol-name module)))
    (with-temp-file (pathname-for-swarmdocs-pages-output module)
      (sgml-generate-refentries-for-module module))))

(defun sgml-create-refentries-for-all-modules ()
  (interactive)
  (loop for module being each hash-key of *module-hash-table*
        do
        (sgml-create-refentries-for-module module)))

(defun build-method-signature-hash-table ()
  (loop for protocol being each hash-value of *protocol-hash-table*
        do
        (loop for method in (protocol-method-list protocol)
              do
              (push (cons protocol method)
                    (gethash (get-method-signature method)
                             *method-signature-hash-table*)))))

(defun build-protocol-vector ()
  (setq *protocol-list*
        (sort
         (loop for protocol being each hash-value of *protocol-hash-table*
               collect protocol)
         #'(lambda (protocol-a protocol-b)
             (string< (protocol-name protocol-a)
                      (protocol-name protocol-b))))))

(defun build-method-signature-vector ()
  (setq *method-signature-list*
        (sort
         (loop for method-signature being each hash-key of
               *method-signature-hash-table*
               collect method-signature)
         #'string<)))

(defun sgml-protocol-indexentry (protocol)
  (insert "<INDEXENTRY>\n")
  (insert "<PRIMARYIE LINKENDS=\"")
  (insert (sgml-protocol-id protocol))
  (insert "\">")
  (insert (external-protocol-name protocol))
  (insert "</PRIMARYIE>\n")
  (insert "</INDEXENTRY>\n"))

(defun sgml-generate-protocol-index ()
  (insert "<INDEX ID=\"PROTOCOL.INDEX\">\n")
  (insert "<TITLE>Protocol Index</TITLE>\n")
  (loop for protocol in *protocol-list*
        unless (internal-protocol-p protocol)
        do (sgml-protocol-indexentry protocol))
  (insert "</INDEX>\n"))

(defun sgml-method-signature-indexentry (method-signature)
  (insert "<INDEXENTRY>\n")
  (insert "<PRIMARYIE LINKENDS=\"")
  (let ((protocol.method-list (gethash method-signature
                                       *method-signature-hash-table*))
        (space ""))
    (loop for protocol.method in protocol.method-list
          do
          (insert space)
          (insert (sgml-method-signature-id
                   (car protocol.method)
                   (method-phase (cdr protocol.method))
                   method-signature))
          (setq space " ")))
  (insert "\">")
  (insert method-signature)
  (insert "</PRIMARYIE>\n")
  (insert "</INDEXENTRY>\n"))

(defun sgml-generate-method-signature-index ()
  (insert "<INDEX ID=\"METHOD.INDEX\">\n")
  (insert "<TITLE>Method Index</TITLE>\n")
  (loop for method-signature in *method-signature-list*
        do (sgml-method-signature-indexentry method-signature))
  (insert "</INDEX>\n"))

(defun sgml-generate-indices ()
  (with-temp-file (concat (get-swarmdocs-build-area) "src/refindex.sgml")
    (sgml-generate-protocol-index)
    (sgml-generate-method-signature-index)))

(defun load-and-process-protocols ()
  (interactive)
  (load-protocols-for-all-modules)
  (generate-expanded-methodinfo-lists)
  (build-method-signature-hash-table)
  (build-protocol-vector)
  (build-method-signature-vector))

(defun run-all ()
  (interactive)
  (load-and-process-protocols)
  (sgml-create-refentries-for-all-modules)
  (sgml-generate-indices))


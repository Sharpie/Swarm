;; Copyright � 1998-2000 Swarm Development Group
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
(require 'common)
(require 'cc-mode)

(provide 'protocol)

(defvar *protocol-hash-table* (make-hash-table :test #'equal))

(defvar *module-hash-table* (make-hash-table))

(defconst *phases* '(:creating :setting :using))

(defvar *protocol-list*)

(defvar *method-signature-hash-table* (make-hash-table :test #'equal))

(defvar *method-signature-list*)

(defvar *general-example-counter-hash-table* (make-hash-table :test #'eq))

(defvar *method-example-counter-hash-table* (make-hash-table :test #'equal))

(defvar *macro-name-hash-table* (make-hash-table :test #'equal))

(defvar *idl-flag* nil)

(defstruct module
  sym
  summary
  description-list
  function-list
  global-list
  macro-list
  typedef-list
  example-list)

(defstruct protocol
  module
  name
  summary
  description-list
  included-protocol-list
  macro-list
  function-list
  global-list
  typedef-list
  example-list
  method-list
  expanded-methodinfo-list
  deprecated-list)

(defstruct method
  phase
  factory-flag
  return-type
  arguments
  description-list
  example-list
  deprecated-list)

(defstruct methodinfo
  level
  protocol
  method)

(defstruct global
  name
  module
  protocol
  type
  description-list)

(defstruct macro
  name
  module
  protocol
  arguments
  description-list)

(defstruct typedef
  name
  module
  protocol
  type
  description-list)

(defstruct function
  name
  module
  protocol
  return-type
  arguments
  description-list
  example-list)

(defstruct parse-state
  tag
  last-tag
  phase

  line
  buf

  summary-doc
  description-doc-list

  global-type
  global-name
  global-names
  global-list

  function-return-type
  function-name
  function-list

  macro-name
  macro-list

  item-doc-list
  deprecated-doc-list
  method-list

  typedef-list

  scratch-example-list
  example-list)

(defconst *doc-types* '(:method-doc :summary-doc :description-doc
                        :function-doc :macro-doc :typedef-doc
                        :global-doc :global-begin :global-end :global-break
                        :example-doc :deprecated-doc))

(defconst *protocol-regexp*
   (if *disable-gui* 
       "^\\(@protocol\\|#ifndef DISABLE_GUI\\)"
       "^@protocol"))

(defconst *funcptr-regexp* "\\([^;()]*(\\s-*[*]*\\s-*\\([^*);]+\\))[^;]*\\);")

(defun find-protocol ()
  (interactive)
  (let ((ret nil))
    (while (progn
             (setq ret (re-search-forward *protocol-regexp* nil t))
	     (when (save-excursion
		     (beginning-of-line 1)
		     (looking-at "#ifndef DISABLE_GUI"))
	       (beginning-of-line 1)
	       (c-forward-conditional 1)
	       t)))
    (and ret (save-excursion (beginning-of-line 1) (looking-at "@protocol")))))

(defun skip-whitespace ()
  (skip-chars-forward " \t\r\n"))

(defun skip-whitespace-backward ()
  (skip-chars-backward " \t\r\n"))

(defun skip-backward-to-nonwhite ()
  (when (looking-at "\\s-")
    (skip-whitespace-backward))
  (when (looking-at "\\s-")
    (backward-char)))

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

(defun inclusive-phase-p (phase target-phase)
  (if (eq target-phase :using)
      (member phase '(:using :getters))
    (eq phase target-phase)))

(defun parse-method-worker (protocol
                            factory-flag
                            parse-state)
  (let ((phase (parse-state-phase parse-state))
        (method-description-list 
         (reverse (parse-state-item-doc-list parse-state)))
        (method-example-list
         (reverse (parse-state-scratch-example-list parse-state)))
        (method-deprecated-list 
         (reverse (parse-state-deprecated-doc-list parse-state))))
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
       (setq name nil)
       (while (looking-at ",")
         (forward-char)
         (push (cons nil (next-expr)) arguments))
       (when (looking-at "//")
         (beginning-of-line 2))
       until (looking-at ";"))
      (unless phase
        (error "No phase in protocol: %s" (protocol-name protocol)))
      (setq arguments (reverse arguments))
      (when (inclusive-phase-p phase :getters)
        (unless (string-match "^get" (caar arguments))
          (error "Method not a getter: %s" (prin1-to-string arguments))))
      (make-method
       :phase phase
       :factory-flag factory-flag
       :arguments arguments
       :return-type return-type
       :description-list method-description-list
       :example-list method-example-list
       :deprecated-list method-deprecated-list))))

(defun set-first-argument (method value)
  (let ((first-argument (first (method-arguments method))))
    (setf (nth 1 first-argument) value)))

(defun fixup-method (method)
  (let ((sig (get-method-signature method)))
    ;; these fixups are because of problems with protocol self-reference
    (cond ((string= sig "-getZone")
           (setf (method-return-type method) "id <Zone>"))
	  ((string= sig "-getPopulation")
	   (setf (method-return-type method) "id <List>"))
          ((or (string= sig "+create:") (string= sig "+createBegin:"))
           (set-first-argument method "id <Zone>"))
          ((or (string= sig "-getProbeMap")
               (string= sig "-getCompleteProbeMap"))
           (setf (method-return-type method) "id <ProbeMap>"))
          ((string= sig "-getProbeForVariable:")
           (setf (method-return-type method) "id <VarProbe>"))
          ((string= sig "-getProbeForMessage:") 
           (setf (method-return-type method) "id <MessageProbe>"))
          ((string= sig "-getScheduleActivity")
           (setf (method-return-type method) "id <ScheduleActivity>"))
          ((string= sig "-getSwarmActivity")
           (setf (method-return-type method) "id <SwarmActivity>"))
          ((or (string= sig "-getControllingActivity")
               (string= sig "-getOwnerActivity")
               (string= sig "-getTopLevelActivity"))
           (setf (method-return-type method) "id <Activity>"))
          ((string= sig "-getAction")
           (setf (method-return-type method) "id <Action>"))
          ((string= sig "-activateIn:")
           (set-first-argument method "id <Swarm>"))
          ((string= sig "-hdf5OutShallow:")
           (set-first-argument method "id <HDF5>"))
          ((string= sig "-updateArchiver:")
           (set-first-argument method "id <Archiver>"))
          ((or (string= sig "-addProbeMap:")
               (string= sig "-dropProbeMap:"))
           (set-first-argument method "id <ProbeMap>")))
    method))

(defun parse-method (protocol
                     factory-flag
                     parse-state)
  (fixup-method (parse-method-worker protocol factory-flag parse-state)))
  
(defun parse-function (module
                       protocol
                       parse-state)
  (prog1
      (make-function
       :name (parse-state-function-name parse-state)
       :module module
       :protocol protocol
       :return-type (parse-state-function-return-type parse-state)
       :description-list (reverse (parse-state-item-doc-list parse-state))
       :example-list (reverse (parse-state-scratch-example-list parse-state))
       :arguments
       (progn
         (search-forward "(")
         (loop do (skip-whitespace)
               for start-pos = (point)
               do
               (re-search-forward "[),]")
               (backward-char 2)
               (skip-backward-to-nonwhite)
               for arg = (buffer-substring start-pos (1+ (point)))
               collect
               (if (string-match "\\(.*[^a-zA-Z_]\\)\\([a-zA-Z_]+\\)" arg)
                   (cons (match-string 1 arg) (match-string 2 arg))
                   (cons arg nil))
               do
               (re-search-forward "[),]")
               (backward-char)
               until (looking-at ")")
               do
               (forward-char))))
    (search-forward ";")))

(defun handle-function (module protocol parse-state)
  (prog1
      (push (parse-function module protocol parse-state)
            (parse-state-function-list parse-state))
    (setf (parse-state-item-doc-list parse-state) nil)))

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

(defun extract-doc-string (str)
  (if (> (length str) 4)
      (substring str 4)
      ""))

(defun parse-global-using-parse-state (module protocol parse-state)
  (prog1
      (make-global
       :name (parse-state-global-name parse-state)
       :module module
       :protocol protocol
       :type (parse-state-global-type parse-state)
       :description-list
       (if (eq (parse-state-tag parse-state) :global)
           (parse-state-item-doc-list parse-state)
           (list (extract-doc-string (parse-state-line parse-state)))))
    (setf (parse-state-item-doc-list parse-state) nil)))

(defun immediate-global-tag-processed (module protocol parse-state)
  (when (member (parse-state-tag parse-state)
                '(:global-begin :global-end :global-break))
    (parse-global-using-parse-state module protocol parse-state)))
    
(defun is-doc-type (parse-state)
  (member (parse-state-tag parse-state) *doc-types*))

(defun set-buf (parse-state)
  (setf (parse-state-buf parse-state)
        (extract-doc-string (parse-state-line parse-state))))

(defun append-buf (parse-state)
  (let ((tag (parse-state-tag parse-state)))
    (when (is-doc-type parse-state)
      (let ((buf (parse-state-buf parse-state)))
        (setf (parse-state-buf parse-state)
              (let ((line (parse-state-line parse-state)))
                (if (eq tag :example-doc)
                    (concat buf "\n" (extract-doc-string line))
                    (concat
                     (if (string-match " $" buf) buf (concat buf ""))
                     (extract-doc-string line)))))))))

(defun extract-funcptr-name ()
  (save-excursion
    (forward-char)
    (backward-sexp)
    (let ((end (save-excursion
                 (search-backward ")")
                 (point))))
      (backward-sexp)
      (search-forward "*")
      (buffer-substring (point) end))))

(defun extract-funcptr-type ()
  (let ((name (extract-funcptr-name)))
    (save-excursion
        (forward-char)
        (let ((end (point)))
          (backward-sexp 3)
          (strip-regexp (buffer-substring (point) end) name)))))

(defun update-global-state (parse-state)
  (unless (save-excursion
            (beginning-of-line)
            (looking-at "\\s-+//G:"))
    (let ((is-terminated (looking-at ";")))
      (backward-char)
      (skip-backward-to-nonwhite)
      (if (looking-at ")")
          (let ((name (extract-funcptr-type))
                (type (extract-funcptr-name)))
            (setf (parse-state-global-type parse-state) name)
            (setf (parse-state-global-name parse-state) type))
          (progn
            (backward-sexp)
            (let ((name-beg (point)))
              (forward-sexp)
              (let ((name (buffer-substring name-beg (point))))
                (setf (parse-state-global-name parse-state) name))
              (when is-terminated
                (beginning-of-line)
                (forward-sexp)
                (skip-whitespace)
                (let ((type-beg (point)))
                  (goto-char name-beg)
                  (backward-char)
                  (skip-backward-to-nonwhite)
                  (let ((type (buffer-substring type-beg (1+ (point)))))
                    (setf (parse-state-global-type parse-state) type))))))))
    (unless (parse-state-global-name parse-state)
      (error "nil name (non-funcptr)"))
    (unless (parse-state-global-type parse-state)
      (error "nil type (non-funcptr)"))))

    
(defun check-global-doc-post (parse-state)
  (search-forward "//G:")
  (backward-char 4)
  (save-excursion
    (let ((last-extern (parse-state-global-name parse-state)))
      (update-global-state parse-state)
      (cond ((save-excursion
               (beginning-of-line)
               (looking-at ".*;.*//G:"))
             :global-end)
            ((not (string= last-extern
                           (parse-state-global-name parse-state)))
             :global-break)
            (t :global-doc)))))

(defun globaldocp ()
  (save-excursion
    (search-forward "//G:" (end-of-line-position) t)))

(defun check-global (parse-state)
  (let ((tag :global))
    (re-search-forward "\\s-+")
    (let ((not-eol-flag
           (re-search-forward "\\(;\\|//G:\\|,\\)" (end-of-line-position) t)))
      (when not-eol-flag
        (backward-char))
      (cond ((or (not not-eol-flag) (looking-at ":"))
             (save-excursion
               (let ((end (if not-eol-flag
                              (point)
                              (end-of-line-position))))
                 (beginning-of-line)
                 (forward-sexp)
                 (skip-whitespace)
                 (let ((beg (point)))
                   (goto-char end)
                   (skip-whitespace-backward)
                   (setf (parse-state-global-type parse-state)
                         (buffer-substring beg (point))))))
             (if not-eol-flag
                 (progn
                   (setq tag :global-begin)
                   (forward-char 2))
                 (setq tag :global-type-only)))
            ((looking-at ",")
             (if (globaldocp) ; implies only one
                 (update-global-state parse-state)
                 (progn
                   (let ((start (point)))
                     (setf (parse-state-global-name parse-state) nil)
                     (setq tag :global-names)
                     (save-excursion
                       (backward-char)
                       (backward-sexp)
                       (let ((name-beg (point)))
                         (search-forward ";")
                         (backward-char 2)
                         (skip-backward-to-nonwhite)
                         (let ((names (buffer-substring name-beg
                                                        (1+ (point)))))
                           (setf (parse-state-global-names parse-state)
                                 names))
                         (goto-char start)
                         (beginning-of-line)
                         (forward-sexp)
                         (forward-char)
                         (let* ((type-beg (point))
                                (type
                                 (buffer-substring
                                  type-beg
                                  (save-excursion
                                    (goto-char name-beg)
                                    (backward-char)
                                    (skip-backward-to-nonwhite)
                                    (1+ (point))))))
                           (setf (parse-state-global-type parse-state)
                                 type))))))))
            ((looking-at ";")
             (update-global-state parse-state)
             (when (globaldocp)
               (setq tag :global-end))))
      (case tag
        ((:global-begin :global-end) 
         (search-forward "//G: ")
         (backward-char 5))
        (:global-names
         (search-forward ";")
         (setq tag :global))
        ((:global :global-type-only))
        (otherwise
         (error "unknown global tag: %s" tag))))
    tag))
  
(defun skip-c-comment ()
  (interactive)
  (search-forward "*/")
  (beginning-of-line 2))

(defun check-common-tags (parse-state)
  (cond ((looking-at "[ \t]*$") :newline)
        ((looking-at "//S:") :summary-doc)
        ((looking-at "//D:") :description-doc)
        ((looking-at "//#:") :macro-doc)
        ((looking-at "//F:") :function-doc)
        ((looking-at "//T:") :typedef-doc)
        ((looking-at "//x:") :deprecated-doc)

        ((looking-at "externvar.*")
         (check-global parse-state))
        ((looking-at "//G:") :global-doc)
        ((looking-at ".+//G:") (check-global-doc-post parse-state))

        ((looking-at "#if 0")
         (c-forward-conditional 1)
         (beginning-of-line 0)
         :ifdef-comment)
        ((and (looking-at "#ifndef DISABLE_GUI") *disable-gui*)
         (c-forward-conditional 1)
         (beginning-of-line 0)
         :ifdef-comment)
        ((and (looking-at "#ifndef IDL") *idl-flag*)
         (c-forward-conditional 1)
         (beginning-of-line 0)
         :ifdef-comment)
        ((looking-at "#ifdef")
         :ifdef)
        ((looking-at "#ifndef")
         :ifndef)
        ((looking-at "#if [^0]")
         :if)
        ((looking-at "#endif")
         :endif)
        ((looking-at "#else")
         :else)
        ((looking-at "#undef")
         :undef)
        ((looking-at "extern \"C\" {")
         :externC)
        ((looking-at "}")
         :externC-end)
        ((looking-at "/\\*")
         (skip-c-comment)
         :c-comment)
        ((looking-at "#import")
         :import)
        ((looking-at "#include")
         :include)
        ((looking-at "")
         :page-break)
        ((looking-at "typedef\\s-+")
         :typedef)
        ((or (looking-at "// ") (looking-at "//$")) :objc-comment)
        ((looking-at "#define\\s-+\\([^() \t\n]+\\)\\(\\s-+[^()\n]+\\)?$")
         (setf (parse-state-macro-name parse-state) (match-string 1))
         :define)
        ((looking-at "#define\\s-+\\([^(\n]+\\)\\(([^)\n]*)\\)")
         (setf (parse-state-macro-name parse-state) (match-string 1))
         :macro)
        ((looking-at "@class")
         :class)
        ((looking-at "extern\\s-+\\(Class\\s-+\\|int\\s-+\\|void\\s-\\|id\\s-+<.*\\s-\\|id\\s-\\|char\\s-*\\*\\|const\\s-+char\\s-*\\*\\|BOOL\\)\\s-*\\([^ (]+\\)\\s-*(")
         (let ((return-type (match-string 1))
               (function-name (match-string 2)))
           (setf (parse-state-function-return-type parse-state)
                 (strip-regexp return-type "\\s+$"))
           (setf (parse-state-function-name parse-state)
                 function-name))
         :function)
        (t nil)))
  
(defun phase-tag-p (tag)
  (member-if #'(lambda (phase) (inclusive-phase-p tag phase))
             *phases*))

(defun check-protocol-tags (parse-state)
  (let ((tag
         (cond ((looking-at "CREATING") :creating)
               ((looking-at "SETTING") :setting)
               ((looking-at "USING") :using)
               ((looking-at "GETTERS") :getters)
               ((looking-at "-")  :method)
               ((looking-at "+") :factory-method)
               ((looking-at "//M:") :method-doc)
               ((looking-at "@end") :protocol-end)
               ((looking-at "//E:") :example-doc)
               ((looking-at "///M:") :bogus-method)
               (t nil))))
    (when (phase-tag-p tag)
      (setf (parse-state-phase parse-state) tag))
    tag))

(defun protocol-tag-change (parse-state)
  (let ((buf (parse-state-buf parse-state)))
    (case (parse-state-last-tag parse-state)
      (:example-doc
       (push (concat buf "\n") (parse-state-scratch-example-list parse-state))
       (unless (or (parse-state-method-list parse-state)
                   (parse-state-item-doc-list parse-state))
         (setf (parse-state-example-list parse-state)
               (parse-state-scratch-example-list parse-state))
         (setf (parse-state-scratch-example-list parse-state) nil)))
      (:method-doc
       (push buf (parse-state-item-doc-list parse-state))))))

(defun common-tag-change (parse-state)
  (let ((buf (parse-state-buf parse-state)))
    (case (parse-state-last-tag parse-state)
      ((:function-doc :macro-doc :global-doc :typedef-doc)
       (push buf (parse-state-item-doc-list parse-state)))
      (:summary-doc
       (if (parse-state-summary-doc parse-state)
           (error "summary already set")
           (setf (parse-state-summary-doc parse-state) buf)))
      (:description-doc
       (push buf (parse-state-description-doc-list parse-state)))
      (:deprecated-doc
       (push buf (parse-state-deprecated-doc-list parse-state))))))

(defun handle-method (protocol factory-flag parse-state)
  (push (parse-method protocol
                      factory-flag
                      parse-state)
        (parse-state-method-list parse-state))
  (setf (parse-state-scratch-example-list parse-state) nil)
  (setf (parse-state-item-doc-list parse-state) nil)
  (setf (parse-state-deprecated-doc-list parse-state) nil)
  t)
       
(defun parse-typedef (module protocol parse-state)
  (forward-char 7)
  (skip-whitespace)
  (let ((type-beg (point))
        (description-list (reverse (parse-state-item-doc-list parse-state))))
    (if (looking-at ".*\\(union\\|struct\\)")
        (progn
          (search-forward "{")
          (backward-char)
          (forward-sexp)
          (let ((type-end (point)))
            (skip-whitespace)
            (let ((name-beg (point)))
              (search-forward ";")
              (backward-char 2)
              (skip-backward-to-nonwhite)
              (make-typedef
               :name (buffer-substring name-beg (1+ (point)))
               :module module
               :protocol protocol
               :type (buffer-substring type-beg type-end)
               :description-list description-list))))
        (if (looking-at *funcptr-regexp*)
            (let* ((name (match-string 2))
                   (type (strip-regexp (match-string 1) name)))
              (make-typedef
               :name name
               :module module
               :protocol protocol
               :type type
               :description-list description-list))
            (progn
              (search-forward ";")
              (backward-char 2)
              (skip-backward-to-nonwhite)
              (let ((name-end (1+ (point))))
                (backward-sexp)
                (let ((name-beg (point)))
                  (skip-whitespace-backward)
                  (let ((type-end (point)))
                    (make-typedef
                     :name (buffer-substring name-beg name-end)
                     :module module
                     :protocol protocol
                     :type (buffer-substring type-beg type-end)
                     :description-list description-list)))))))))
                                         
(defun handle-typedef (module protocol parse-state)
  (push (parse-typedef module 
                       protocol 
                       parse-state)
        (parse-state-typedef-list parse-state))
  (setf (parse-state-item-doc-list parse-state) nil)
  t)

(defun parse-macro (module protocol parse-state)
  (prog1
      (let ((dl (parse-state-item-doc-list parse-state))
            (name (parse-state-macro-name parse-state)))
        (search-forward "(")
        (let ((arguments
               (loop do (skip-whitespace)
                     for start-pos = (point)
                     do
                     (re-search-forward "[),]")
                     (backward-char 2)
                     (skip-backward-to-nonwhite)
                     for arg = (buffer-substring start-pos (1+ (point)))
                     collect arg
                     do
                     (re-search-forward "[),]")
                     (backward-char)
                     until (looking-at ")")
                     do
                     (forward-char))))
          (make-macro
           :name name
           :module module
           :protocol protocol
           :arguments arguments
           :description-list dl)))
    (while (looking-at ".*\\\\\\s-*$")
      (forward-line))))

(defun parse-define (module protocol parse-state)
  (prog1
      (let ((dl (parse-state-item-doc-list parse-state))
            (name (parse-state-macro-name parse-state)))
        (let ((macro (gethash name *macro-name-hash-table*)))
          (if macro
              (progn
                (message "duplicate #define: %s" name)
                nil)
              (setf (gethash name *macro-name-hash-table*)
                    (make-macro
                     :name name
                     :module module
                     :protocol protocol
                     :arguments :no-arguments
                     :description-list dl)))))
    (while (looking-at ".*\\\\\\s-*$")
      (forward-line))))

(defun handle-macro (module protocol parse-state)
  (push (parse-macro module protocol parse-state)
        (parse-state-macro-list parse-state))
  (setf (parse-state-item-doc-list parse-state) nil)
  t)

(defun handle-define (module protocol parse-state)
  (let ((macro (parse-define module protocol parse-state)))
    (when macro
      (push macro (parse-state-macro-list parse-state))))
  (setf (parse-state-item-doc-list parse-state) nil)
  t)

(defun handle-global (module protocol parse-state)
  (let ((names (parse-state-global-names parse-state)))
    (if names
        (progn
          (loop for name in (split-string names ",")
                for stripped-name = (strip-regexp name "\\s-+")
                do
                (push (make-global
                       :name stripped-name
                       :module module
                       :protocol protocol
                       :type (parse-state-global-type parse-state)
                       :description-list
                       (reverse
                        (parse-state-item-doc-list parse-state)))
                      (parse-state-global-list parse-state)))
          (setf (parse-state-global-names parse-state) nil))
        (push (parse-global-using-parse-state module protocol parse-state)
              (parse-state-global-list parse-state)))
    (setf (parse-state-item-doc-list parse-state) nil))
  t)
 
(defun handle-protocol-tag (protocol parse-state)
  (let ((tag (parse-state-tag parse-state))
        (module (protocol-module protocol)))
    (case tag
      ((:method :factory-method)
       (handle-method protocol
                      (eq (parse-state-tag parse-state) :factory-method)
                      parse-state))
      (:global
       (handle-global module protocol parse-state))
      (:macro
       (handle-macro module protocol parse-state))
      (:define
       (handle-define module protocol parse-state))
      (:typedef
       (handle-typedef module protocol parse-state))
      (:function
       (handle-function module protocol parse-state))
      (:protocol-end t)
      (otherwise nil))))

(defun handle-common-tag (module protocol parse-state)
  (let ((tag (parse-state-tag parse-state)))
    (case tag
      (:global
       (handle-global module protocol parse-state))
      (:macro
       (handle-macro module protocol parse-state))
      (:typedef
       (handle-typedef module protocol parse-state))
      (:define
       (handle-define module protocol parse-state))
      (:function
       (handle-function module protocol parse-state))
      (otherwise nil))))

(defun same-tag-p (parse-state)
  (eq (parse-state-tag parse-state)
      (parse-state-last-tag parse-state)))

(defun end-tag-p (parse-state)
  (eq (parse-state-tag parse-state) :protocol-end))

(defun process-header-file (module protocol)
  (let ((parse-state (make-parse-state)))
    (beginning-of-line 1)
    (while (and (zerop (forward-line 1))
                (not (and protocol (end-tag-p parse-state))))
      (beginning-of-line)
      (let ((tag (check-common-tags parse-state)))
        (if tag  
            (if protocol ;; if a common tag *inside* a protocol do
                ;; special processing if this tag can also be a
                ;; protocol tag (such as deprecated-doc)
                (if (parse-state-deprecated-doc-list parse-state)
                    (progn
                      (setf (protocol-deprecated-list protocol)
                            (parse-state-deprecated-doc-list parse-state))
                      ;; immediately re-set again
                      (setf (parse-state-deprecated-doc-list parse-state) nil))))
          (if protocol
              (progn
                (setq tag (check-protocol-tags parse-state))
                (unless tag
                  (error "Unrecognized text (protocol): [%s]"
                         (line-text))))
            (if (looking-at *protocol-regexp*)
                (progn
                  (re-search-forward "^@end")
                  (setq tag :skipped-protocol))
              (error "Unrecognized text (non-protocol): [%s]"
                     (line-text)))))
        (setf (parse-state-tag parse-state) tag))
      (setf (parse-state-line parse-state) (line-text))
      (let ((immediate-object
             (immediate-global-tag-processed module
                                             protocol
                                             parse-state)))
        (if immediate-object
            (push immediate-object (parse-state-global-list parse-state))
          (progn
            (if (same-tag-p parse-state)
                (append-buf parse-state)
              (progn
                (if protocol
                    (unless (protocol-tag-change parse-state)
                      (common-tag-change parse-state))
                  (common-tag-change parse-state))
                (when (is-doc-type parse-state)
                  (set-buf parse-state)))))))
      (if protocol
          (unless (handle-protocol-tag protocol parse-state)
            (handle-common-tag module protocol parse-state))
        (handle-common-tag module protocol parse-state))
      (setf (parse-state-last-tag parse-state)
            (parse-state-tag parse-state)))
    parse-state))

(defun load-protocol (module)
  (interactive)
  (skip-whitespace)
  (let* ((protocol-name
          (let ((beg (point)))
            (skip-name)
            (buffer-substring beg (point))))
         (included-protocol-list
          (parse-included-protocol-list))
         (protocol (make-protocol
                    :module module
                    :name protocol-name
                    :included-protocol-list included-protocol-list)))
    (let ((parse-state (process-header-file module protocol)))
      (setf (protocol-summary protocol)
            (parse-state-summary-doc parse-state)
      
            (protocol-description-list protocol)
            (reverse (parse-state-description-doc-list parse-state))
            
            (protocol-macro-list protocol)
            (reverse (parse-state-macro-list parse-state))

            (protocol-global-list protocol)
            (reverse (parse-state-global-list parse-state))

            (protocol-method-list protocol)
            (reverse (parse-state-method-list parse-state))

            (protocol-typedef-list protocol)
            (reverse (parse-state-typedef-list parse-state))

            (protocol-example-list protocol)
            (reverse (parse-state-example-list parse-state))))
    protocol))
            
(defun load-protocols (module)
  (interactive)
  (goto-char (point-min))
  (loop
   while (find-protocol)
   collect (load-protocol module)))

(defun load-module (module-sym)
  (goto-char (point-min))
  (let* ((module (make-module :sym module-sym))
         (parse-state (process-header-file module nil)))
    (setf (module-summary module) (parse-state-summary-doc parse-state))
    (setf (module-description-list module)
          (reverse (parse-state-description-doc-list parse-state)))
    (setf (module-example-list module)
          (reverse (parse-state-example-list parse-state)))
    (setf (module-function-list module)
          (reverse (parse-state-function-list parse-state)))
    (setf (module-global-list module)
          (reverse (parse-state-global-list parse-state)))
    (setf (module-macro-list module)
          (reverse (parse-state-macro-list parse-state)))
    (setf (module-typedef-list module)
          (reverse (parse-state-typedef-list parse-state)))
    module))

(defun create-included-protocol-list (protocol)
  (loop for included-protocol-name in (protocol-included-protocol-list protocol)
        for included-protocol = (lookup-protocol included-protocol-name)
        unless included-protocol do (error "Could not find protocol %s"
                                           included-protocol-name)
        collect included-protocol))

(defun lookup-module (module-sym)
  (car (remove-if-not #'module-p (gethash module-sym *module-hash-table*))))

(defun lookup-protocol (name)
  (gethash name *protocol-hash-table*))

(defun CREATABLE-protocol ()
  (let ((description "Declare that a defined type supports creation."))
    (make-protocol
     :name "CREATABLE"
     :module (lookup-module 'defobj)
     :included-protocol-list nil
     :summary description
     :description-list (list description)
     :method-list nil)))

(defun RETURNABLE-protocol ()
  (let ((description "Declare that a defined type may be created as a side-effect"))
    (make-protocol
     :name "RETURNABLE"
     :module (lookup-module 'defobj)
     :included-protocol-list nil
     :summary description
     :description-list (list description)
     :method-list nil)))

(defun add-protocol (module-sym protocol)
  (setf (gethash (protocol-name protocol) *protocol-hash-table*) protocol)
  (push protocol (gethash module-sym *module-hash-table*)))

(defun module-sym-from-spec (module-spec)
  (if (consp module-spec) (car module-spec) module-spec))

(defun ensure-module (module-sym)
  (let ((module (lookup-module module-sym)))
    (if module
        module
        (progn
          (setq module (load-module module-sym))
          (push module (gethash module-sym *module-hash-table*))))
    module))

(defun load-all-modules ()
  (interactive)
  
  (set-verbosity nil)

  (clrhash *protocol-hash-table*)
  (clrhash *module-hash-table*)
  (loop for module-spec in *swarm-modules*
        for module-sym = (module-sym-from-spec module-spec)
        do
        (if (consp module-spec)
            (find-file-read-only 
             (pathname-for-module-sym module-sym (cdr module-spec)))
          (find-file-read-only (pathname-for-module-sym module-sym)))
        (let ((module (ensure-module module-sym)))
          (loop for protocol in (load-protocols module)
                for name = (protocol-name protocol)
                for exist = (gethash name *protocol-hash-table*)
                when exist do (error "Protocol %s already exists" name)
                do (add-protocol module-sym protocol)))
        (kill-buffer (current-buffer)))
  (add-protocol 'defobj (CREATABLE-protocol))
  (add-protocol 'defobj (RETURNABLE-protocol))
  
  (set-verbosity t)
  
  (loop for protocol being each hash-value of *protocol-hash-table*
        do
        (setf (protocol-included-protocol-list protocol)
              (create-included-protocol-list protocol))))

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

(defun generate-complete-protocol-list (protocol)
  (remove-if #'removed-protocol-p
             (let ((seen-protocols (make-hash-table)))
               (flet ((expand (eprotocol)
                              (setf (gethash eprotocol seen-protocols) t)
                              (cons
                               eprotocol
                               (loop for iprotocol in
                                     (protocol-included-protocol-list eprotocol)
                                     unless (gethash iprotocol seen-protocols)
                                     append (expand iprotocol)))))
                 (expand protocol)))))

(defun generate-expanded-methodinfo-list (protocol uniquify-flag)
  (let ((expanded-protocols-hash-table (make-hash-table))
        (method-hash-table
         (if uniquify-flag
             (make-hash-table :test #'equal)
             (make-hash-table))))
    (flet ((expand-protocol-level (protocol level)
             (setf (gethash protocol expanded-protocols-hash-table) t)
             (loop for method in (protocol-method-list protocol)
                   for method-sig = 
                   (if uniquify-flag
                       (get-method-signature method)
                       method)
                   for last-methodinfo = (gethash method-sig method-hash-table)
                   when (or (null last-methodinfo)
                            (> level (methodinfo-level last-methodinfo)))
                   do (setf (gethash method-sig method-hash-table)
                            (make-methodinfo
                             :level level
                             :protocol protocol
                             :method method)))
             (loop for included-protocol in
                   (protocol-included-protocol-list protocol)
                   do
                   (unless (gethash included-protocol expanded-protocols-hash-table)
                     (expand-protocol-level included-protocol (1+ level))))))
      (expand-protocol-level protocol 0))
    (sort 
     (loop for method-list being each hash-value of method-hash-table 
           collect method-list)
     #'(lambda (a b)
         (flet ((phase-pos (phase)
                  (case phase
                    (:creating 0)
                    (:setting 1)
                    (:using 2)
                    (:getters 3)))
                (compare-arguments (a b)
                  (flet ((get-key-list (item) (mapcar #'first item)))
                    (compare-string-lists
                     (get-key-list a)
                     (get-key-list b)))))
           (let ((level-diff (- (methodinfo-level a) (methodinfo-level b))))
             (if (zerop level-diff)
                 (let* ((method-a (methodinfo-method a))
                        (method-b (methodinfo-method b))
                        (phase-diff (- (phase-pos (method-phase method-a))
                                       (phase-pos (method-phase method-b)))))
                   (if (zerop phase-diff)
                       (compare-arguments (method-arguments method-a)
                                          (method-arguments method-b))
                       (< phase-diff 0)))
                 (< level-diff 0))))))))

(defun generate-expanded-methodinfo-lists (uniquify-method-list-flag)
  (interactive)
  (loop for protocol being each hash-value of *protocol-hash-table*
        do
        (setf (protocol-expanded-methodinfo-list protocol)
              (generate-expanded-methodinfo-list protocol
                                                 uniquify-method-list-flag))))

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

(defun module-name (module)
  (symbol-name (module-sym module)))

(defun object-type (object)
  (cond ((protocol-p object) 'protocol)
        ((module-p object) 'module)
        ((global-p object) 'global)
        ((function-p object) 'function)
        ((macro-p object) 'macro)
        ((typedef-p object) 'typedef)
        (t (error "unknown object type"))))

(defun generic-module (object)
  (let ((type (object-type object)))
    (case type
      (protocol (protocol-module object))
      (module object)
      (function (function-module object))
      (global (global-module object))
      (macro (macro-module object))
      (typedef (typedef-module object))
      (otherwise (error "unknown type: %s" type)))))

(defun generic-summary (object)
  (cond ((protocol-p object) (protocol-summary object))
        ((module-p object) (module-summary object))
        (t (error "unknown object"))))

(defun generic-description-list (object)
  (cond ((protocol-p object)
         (protocol-description-list object))
        ((module-p object)
         (module-description-list object))
        (t (error "unknown object"))))

(defun deprecated-p (object)
  (if (and (not (module-p object))
           (not (eq (length (generic-deprecated-list object)) 0)))
      't
    nil))

(defun generic-deprecated-list (object)
  (cond ((protocol-p object)
         (protocol-deprecated-list object))
        ((method-p object)
         (method-deprecated-list object))
        (t (error "unknown object"))))

(defun generic-protocol (object)
  (case (object-type object)
    (function (function-protocol object))
    (global (global-protocol object))
    (macro (macro-protocol object))
    (typedef (typedef-protocol object))
    (otherwise (error "unknown type"))))

(defun generic-name (object)
  (case (object-type object)
    (protocol (protocol-name object))
    (module (module-name object))
    (function (function-name object))
    (global (global-name object))
    (macro (macro-name object))
    (typedef (typedef-name object))
    (otherwise (error "unknown type"))))

(defun generic-macro-list (object)
  (case (object-type object)
    (protocol (protocol-macro-list object))
    (module (module-macro-list object))))

(defun generic-typedef-list (object)
  (case (object-type object)
    (protocol (protocol-typedef-list object))
    (module (module-typedef-list object))))

(defun generic-function-list (object)
  (case (object-type object)
    (protocol (protocol-function-list object))
    (module (module-function-list object))))

(defun generic-global-list (object)
  (case (object-type object)
    (protocol (protocol-global-list object))
    (module (module-global-list object))))

(defun argument-key (argument)
  (first argument))

(defun argument-type (argument)
  (let* ((type-and-varname (cdr argument))
         (varname (cadr type-and-varname)))
    ;; the case of method with no arguments
    (when varname
      (car type-and-varname))))

(defun argument-name (argument)
  (let* ((type-and-varname (cdr argument))
         (name (cadr type-and-varname)))
    (when name
      (if (string= name "class") "class_" name))))

(defun has-arguments-p (method)
  (argument-name (first (method-arguments method))))

(defun print-method-signature (method &optional stream nocolon)
  (if (method-factory-flag method)
      (princ "+" stream)
      (princ "-" stream))
  (loop for argument in (method-arguments method)
        for key = (first argument)
        when key do (princ key stream)
        when (has-arguments-p method) do
        (unless nocolon
            (princ ":" stream))
        ))

(defun methodinfo-list-for-phase (protocol phase)
  (loop for methodinfo in (protocol-expanded-methodinfo-list protocol)
        when (inclusive-phase-p
              (method-phase (methodinfo-method methodinfo))
              phase)
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
        count (include-p (methodinfo-level methodinfo)
                         protocol
                         (methodinfo-protocol methodinfo))))

(defun count-included-methodinfo-entries-for-all-phases (protocol)
  (loop for phase in *phases*
        sum (count-included-methodinfo-entries protocol phase)))

(defun name< (a b)
  (string< (generic-name a) (generic-name b)))


(defun count-method-examples (protocol phase)
  (loop for methodinfo in (methodinfo-list-for-phase protocol phase)
        for method = (methodinfo-method methodinfo)
        count (method-example-list method)))

(defun count-noninternal-protocols (protocol)
  (loop for included-protocol in (protocol-included-protocol-list protocol)
        count (not (internal-protocol-p included-protocol))))

(defun compare-method-signatures (method-a method-b)
  (let* ((method-a-signature (get-method-signature method-a))
         (method-b-signature (get-method-signature method-b)))
    (string< method-a-signature method-b-signature)))

(defun compare-methodinfo (a b)
  (let ((protocol-name-a (protocol-name (methodinfo-protocol a)))
        (protocol-name-b (protocol-name (methodinfo-protocol b))))
    (if (string= protocol-name-a protocol-name-b)
        (compare-method-signatures (methodinfo-method a) (methodinfo-method b))
        (string< protocol-name-a protocol-name-b))))


(defun internal-protocol-p (protocol)
  (string= (substring (protocol-name protocol) 0 1) "_"))

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
         #'name<)))

(defun build-method-signature-vector ()
  (setq *method-signature-list*
        (sort
         (loop for method-signature being each hash-key of
               *method-signature-hash-table*
               collect method-signature)
         #'string<)))

(defun collect-objects-of-type (type)
  (let ((object-accessor
         (case type
           (function #'generic-function-list)
           (global #'generic-global-list)
           (macro #'generic-macro-list)
           (typedef #'generic-typedef-list)
           (otherwise (error "unknown type")))))
    (loop for module-sym being each hash-key of *module-hash-table*
          append
          (loop for object in (gethash module-sym *module-hash-table*)
                append (funcall object-accessor object)))))

(defun load-and-process-modules (&key uniquify-method-lists)
  (interactive)
  (load-all-modules)
  (generate-expanded-methodinfo-lists uniquify-method-lists)
  (build-method-signature-hash-table)
  (build-protocol-vector)
  (build-method-signature-vector))


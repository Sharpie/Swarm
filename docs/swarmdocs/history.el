(require 'cl)
(require 'gnus)
(require 'timezone)
(eval-and-compile
  (push (getenv "TOP_BUILDDIR") load-path))
(require 'common)

(defstruct changelog-item
  filename
  description)

(defstruct changelog
  name
  email
  timestamp
  pos
  item-list)

(defun find-next-changelog ()
  (interactive)
  (when (re-search-forward "^[0-9MTWF]" nil t)
    (backward-char)
    (point)))

(defun narrow-to-next-changelog ()
  (interactive)
  (let ((beg (find-next-changelog)))
    (when beg
      (forward-char)
      (let ((end (find-next-changelog)))
        (if end
            (narrow-to-region beg end)
            (narrow-to-region beg (point-max))))
      (goto-char (point-min))
      t)))

(defun find-next-changelog-item ()
  (interactive)
  (when (re-search-forward "^\\s-+\\*" nil t)
    (point)))
    

(defun narrow-to-next-changelog-item ()
  (interactive)
  (let ((beg (find-next-changelog-item)))
    (when beg
      (skip-chars-forward " \t")
      (setq beg (point))
      (let ((end (find-next-changelog-item)))
        (if end
            (progn
              (beginning-of-line)
              (narrow-to-region beg (point)))
            (narrow-to-region beg (point-max))))
      t)))

(defun parse-date (date)
  (let* ((parse (timezone-parse-date date))
	 (date (mapcar (lambda (d) (and d (string-to-int d))) parse))
	 (time (mapcar 'string-to-int (timezone-parse-time (aref parse 3)))))
    (encode-time (caddr time) (cadr time) (car time)
		 (caddr date) (cadr date) (car date)
		 (* 60 (timezone-zone-to-minute (nth 4 date))))))

(defun parse-changelog-item ()
  (interactive)
  (let ((beg (point)))
    (if (re-search-forward "[:(]" nil t)
        (progn
          (backward-char)
          (if (looking-at "(")
              (progn
                (re-search-backward "[^ ]")
                (forward-char)
                (make-changelog-item
                 :filename (buffer-substring beg (point))
                 :description (progn
                                (skip-chars-forward " ")
                                (buffer-substring (point) (point-max)))))
              (make-changelog-item
               :filename (buffer-substring beg (point))
               :description (progn
                              (forward-char)
                              (skip-chars-forward " ")
                              (buffer-substring (point) (point-max))))))
        (make-changelog-item
         :filename nil
         :description (buffer-substring beg (point-max))))))
          
(defun parse-changelog-entry (pos &optional filename-restriction)
  (interactive)
  (when
      (re-search-forward
       "^\\(.*[0-9]+\\)[ \\t]+\\([A-Za-z. ]+[ \\t]\\)*+<\\(.*\\)>" nil t)
    (let ((date (match-string 1))
          (name (match-string 2))
          (email (match-string 3)))
      (let* ((date+time 
              (if (string-match "^[0-9]" date)
                  (concat date " 0:0:0 GMT")
                  (concat date " GMT")))
             (encoded-time (parse-date date+time))
             changelog-item-list)
        (while (save-restriction
                 (when (narrow-to-next-changelog-item)
                   (goto-char (point-min))
                   (let ((changelog-item (parse-changelog-item)))
                     (when 
                         (or
                          (not filename-restriction)
                          (string= filename-restriction
                                   (changelog-item-filename changelog-item)))
                       (push changelog-item changelog-item-list)))
                   t)))
        (when changelog-item-list
          (make-changelog
           :name name
           :pos pos
           :email email
           :timestamp encoded-time
           :item-list (reverse changelog-item-list)))))))

(defun parse-changelog-buffer (&optional filename-restriction)
  (interactive)
  (let (changelog-list (pos 0))
    (while (save-restriction
             (when (narrow-to-next-changelog)
               (let ((changelog (parse-changelog-entry pos filename-restriction)))
                 (when changelog
                   (push changelog changelog-list)
                   (incf pos)))
               t)))
    (reverse changelog-list)))

(defun parse-changelog (filename &optional filename-restriction)
  (find-file-read-only filename)
  (prog1 (parse-changelog-buffer filename-restriction)
    (kill-buffer (current-buffer))))

(defun sgml-pathname-for-swarmdocs-revision-output (module-sym)
  (let ((module-name (symbol-name module-sym)))
    (case module-sym
      ((refbook set installbook overbook) 
       (concat (get-top-builddir)
               module-name "/" module-name "revhistory.xml"))
      (otherwise (concat (get-top-builddir)
                         "refbook/"
                         module-name
                         "/"
                         module-name
                         "revhistory.xml")))))

(defun process-changelog (&optional module-arg)
  (let* ((module-sym (if module-arg
                         module-arg
                         (intern (car (last command-line-args)))))
         (swarmsrcdir-changelog-list
          (if (member module-sym '(refbook tech set overbook installbook))
              nil
              (parse-changelog (pathname-for-module-sym module-sym "ChangeLog")
                               (header-filename-for-module-sym module-sym))))
         (swarmdocs-changelog-list
          (parse-changelog (pathname-for-swarmdocs module-sym "ChangeLog")))
         (combined-changelog-list 
          (sort (append
                 swarmdocs-changelog-list
                 swarmsrcdir-changelog-list)
                #'(lambda (a b)
                    (let* ((time-a (changelog-timestamp a))
                           (time-b (changelog-timestamp b))
                           (high (- (car time-a) (car time-b))))
                      (if (zerop high)
                          (let ((diff (- (cadr time-a) (cadr time-b))))
                            (if (zerop diff)
                                (not (>= (changelog-pos a) (changelog-pos b)))
                              (>= diff 0)))
                        (>= high 0)))))))
    (with-temp-file (sgml-pathname-for-swarmdocs-revision-output module-sym)
      (sgml-mode)
      (insert "<revhistory id=\"SWARM.")
      (insert (upcase (symbol-name module-sym)))
      (insert ".GENERIC.REVHISTORY\">\n")
      (loop for changelog in combined-changelog-list
            for date-string = (progn
                                (set-time-zone-rule "GMT")
                                (prog1 
                                    (format-time-string 
                                     "%Y-%m-%d"
                                     (changelog-timestamp changelog))
                                  (set-time-zone-rule (getenv "TZ"))))
            for author = (first (split-string
                                 (changelog-email changelog)
                                 "@"))
            do
            (loop for changelog-item in (changelog-item-list changelog)
                  do
                  (insert "<revision>\n")
                  (insert "<revnumber>")
                  (let ((filename (changelog-item-filename changelog-item)))
                    (when filename
                      (insert filename)))
                  (insert "</revnumber>\n")
                  (insert "<date>")
                  (insert date-string)
                  (insert "</date>\n")
                  (insert "<authorinitials>")
                  (insert author)
                  (insert "</authorinitials>\n")
                  (insert "<revremark>\n")
                  (insert-text (changelog-item-description changelog-item))
                  (fill-paragraph nil)
                  (insert "</revremark>\n")
                  (insert "</revision>\n")))
      (insert "</revhistory>\n"))))

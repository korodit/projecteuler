(defun get-file-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun delimiterp (c) (or (char= c #\Space) (char= c #\,) (char= c #\Newline)))

(defun split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defun read-lists-from-stream (stream)
    (loop
        with str and eofp
        until eofp
        do (multiple-value-setq (str eofp) (read-line stream))
        while str
        collect (map 'list #'parse-integer (split str))))

(defun read-matrix-from-stream (el-type stream)
"User must make sure the array is properly structured.
Element type parameter added for future compatibility."
    (let ((contents (loop
        with str and eofp
        until eofp
        do (multiple-value-setq (str eofp) (read-line stream))
        while str
        collect (map 'list #'parse-integer (split str)))))
        (make-array (list (length contents) (length (car contents))) :initial-contents contents)))
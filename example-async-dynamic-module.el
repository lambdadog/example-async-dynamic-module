;;; -*- lexical-binding: t -*-
(require 'example-async-dynamic-module-dyn)

(defvar example-async-dynamic-module--pipe nil)
(defvar example-async-dynamic-module--callback-alist '())

(defun example-async-dynamic-module--filter (_ _)
  (let ((cbs (example-async-dynamic-module-dyn--drain-completions)))
    (dolist (cb cbs)
      (apply (alist-get (car cb) example-async-dynamic-module--callback-alist)
	     `(,(cdr cb))))))

(defun example-async-dynamic-module-sleep-ret (seconds message cb)
  "Asynchronously sleeps for SECONDS seconds, then calls CB with
MESSAGE."
  ;; Initialize if necessary
  (unless example-async-dynamic-module--pipe
    (setq example-async-dynamic-module--pipe
	  (make-pipe-process :name "example-async-dynamic-module-pipe"
			     ;; don't query on exit
			     :noquery t
			     :filter #'example-async-dynamic-module--filter))
    (example-async-dynamic-module-dyn--init example-async-dynamic-module--pipe))
  (let ((cb-num (example-async-dynamic-module-dyn--sleep-ret seconds message)))
    (push `(,cb-num . ,cb) example-async-dynamic-module--callback-alist))
  nil)

(provide 'example-async-dynamic-module)

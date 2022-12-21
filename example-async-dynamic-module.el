;;; -*- lexical-binding: t -*-
(require 'example-async-dynamic-module-dyn)

(defvar example-async-dynamic-module--pipe nil)

;; There's likely a more efficient data structure I can use here
;; rather than an alist. A vector would require more intelligence in
;; generating "callback ids" but a hash table might not be an awful
;; idea.
(defvar example-async-dynamic-module--callback-alist '())

;; This process filter is notified every time the pipe process is
;; written to. Our spawned thread just writes a single byte to the
;; pipe process to notify it that it should drain completions, so we
;; can ignore both the PROC and STRING arguments.
(defun example-async-dynamic-module--filter (_ _)
  (let ((cbs (example-async-dynamic-module-dyn--drain-completions)))
    (dolist (cb cbs)
      (apply (alist-get (car cb) example-async-dynamic-module--callback-alist)
	     `(,(cdr cb))))))

(defun example-async-dynamic-module-sleep-ret (seconds message cb)
  "Asynchronously sleeps for SECONDS seconds, then calls CB with
MESSAGE. MESSAGE must be a string, due to limitations I couldn't
discover a way to work around."
  ;; Initialize pipe if necessary
  (unless example-async-dynamic-module--pipe
    (setq example-async-dynamic-module--pipe
	  (make-pipe-process :name "example-async-dynamic-module-pipe"
			     :noquery t ;; don't query on exit
			     :filter #'example-async-dynamic-module--filter))
    (example-async-dynamic-module-dyn--init example-async-dynamic-module--pipe))
  ;; takes the returned "callback id" from our internal sleep-ret and
  ;; uses it to register our callback in
  ;; `example-async-dynamic-module--callback-alist'.
  (let ((cb-num (example-async-dynamic-module-dyn--sleep-ret seconds message)))
    (push `(,cb-num . ,cb) example-async-dynamic-module--callback-alist))
  nil)

(provide 'example-async-dynamic-module)

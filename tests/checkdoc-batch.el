; Runs checkdoc on files in arguments and prints results
; Copied from https://github.com/leotaku/elisp-check/blob/b73b5a6d41d7a5c0b0698d119a769d7189aa915c/elisp-check.el#L345

; Usage:  Emacs [ARGS] --script checkdoc-batch.el -- FILES...


(setq debug-on-error t)

; Pop off all args until -- is found
(while
  (if argv
    ; This value is the condition for the while loop
    (not (string= (pop argv) "--"))
    ; Ran out of arguments
    (user-error "Error: file names must come after -- argument")))

(unless argv
  (user-error "Error: must pass at least one file"))


(setq checkdoc-autofix-flag 'never)
;(setq checkdoc-diagnostic-buffer )


(dolist (filename argv)
  (with-current-buffer (find-file-noselect filename)
    (checkdoc-current-buffer t))
  (with-current-buffer checkdoc-diagnostic-buffer
    (princ (concat (buffer-string) "\n"))))

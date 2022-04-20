#lang racket
(require wxme)
(require racket/format)
(require racket/cmdline)
(require racket/port)

; The output directory of the script
(define output_dir (make-parameter "./output"))
(define input_dirs (make-parameter null))
(define anonymize? (make-parameter #t))

(command-line 
  #:program "Racket Anonymizer"
  #:once-any 
  [("-o" "--output-dir") out_dir
						 "Specify an output directory, defaults to ./output"
						 (output_dir out_dir)]
  #:multi 
  [("-i" "--input-dir") input_dir 
						"Add an input directory"
						(input_dirs (cons input_dir (input_dirs)))])
; str -> str -> void
; Parameters: 
; rkt-file (str): the filename of the input racket file
; txt-file (str): the filename of the output text file
; 
; Returns:
; void
; Side effects:
; Rights the text version of the racket file to the text file
(define (convert-rkt-to-txt rkt-file txt-file writer-fun)
  (call-with-input-file rkt-file
						(λ (rkt-port)
						   (call-with-output-file txt-file
												  (λ (txt-port) (write-rkt-port-to-txt-port rkt-port txt-port writer-fun)) #:exists 'replace))))

; returns either eof-object or space
(define (skip-spaces txt-port)
  (local [(define char (read-char txt-port))]
		 (cond [(eof-object? char) char]
			   [(char-whitespace? char) (skip-spaces txt-port)]
			   [else char])))

; returns either eof-object or newline
(define (skip-line-comment txt-port char shebang?)
  (cond [(eof-object? char) char]
		[(and shebang? (char=? #\\)) (skip-line-comment txt-port (read-char txt-port) shebang?)]
		[(char=? char #\newline) char]
		[else (skip-line-comment txt-port (read-char txt-port) shebang?)]))

; returns either eof or space
(define (skip-block-comment txt-port char1 char2)
  (cond [(eof-object? char2) char2]
		[(and (char=? char1 #\|)
			  (char=? char2 #\#)) #\space]
		[else (skip-block-comment txt-port char2 (read-char txt-port))]))

(define (get-string txt-port)
  (local [(define char (read-char txt-port))]
		 (cond 
		   [(char=? #\" char) "\""]
		   [(char=? #\\ char) (string-append (string #\\) (string (read-char txt-port)) (get-string txt-port))]
		   [else (string-append (string char) (get-string txt-port))])))

(define (anonymize-txt-port txt-port)
  (local 
	[(define char (read-char txt-port))]
	(cond 
	  [(eof-object? char) ""]
	  [(char=? char #\#) 
	   (local [(define next-char (read-char txt-port))]
			  (cond [(eof-object? next-char) ""]
					[(char=? next-char #\;) 
					 (begin (read txt-port) (anonymize-txt-port txt-port))]
					; (local [(define next-next-char (skip-atom txt-port (skip-spaces txt-port)))]
					;   (if (eof-object? next-next-char) "" 
					;       (string-append (string next-next-char) (anonymize-txt-port txt-port))))]
					[(char=? next-char #\|) (begin (skip-block-comment txt-port (read-char txt-port) (read-char txt-port))
												   (anonymize-txt-port txt-port))]
					[(char=? next-char #\!) 
					 (local [(define next-next-char (skip-line-comment txt-port (read-char txt-port) #t))]
							(if (eof-object? next-next-char) "" 
							  (string-append (string next-next-char) (anonymize-txt-port txt-port))))]
					[else (string-append (string #\# next-char) (anonymize-txt-port txt-port))]))]
	  [(char=? char #\;) 
	   (local [(define next-next-char (skip-line-comment txt-port (read-char txt-port) #f))]
			  (if (eof-object? next-next-char) "" 
				(string-append (string next-next-char) (anonymize-txt-port txt-port))))]
	  [(char=? char #\") (string-append (string char) (get-string txt-port) (anonymize-txt-port txt-port))]
	  [else (string-append (string char) (anonymize-txt-port txt-port))])))

; str -> str -> void
; Parameters:
; rkt-port: the racket port of the input file
; txt-port: the text port of the output file
;
; Returns:
; void
(define (write-rkt-port-to-txt-port rkt-port txt-port writer-fun)
  (local [(define
			(get-remaining-txt-port txt-port)
			(local [(define line (read-line txt-port))]
				   (begin (if (eof-object? line) ""
							(string-append
							  (if (or
									(string-prefix? line ";")
									(string=? line ""))
								""
								(string-append
								  (car (string-split line ";" #:trim? #f #:repeat? #f))
								  "\n"))
							  (get-remaining-txt-port txt-port))))))]
		 (display (writer-fun
					(if (is-wxme-stream? rkt-port)
					  (wxme-port->text-port rkt-port)
					  rkt-port))
				  txt-port)))

(define (translate-file target_dir target_file translation_fun) 
  (local [(define-values (base file must_be_dir) (split-path target_file))]
		 (if (path-has-extension? target_file #".rkt")
		   (convert-rkt-to-txt target_file target_dir translation_fun)
		   (copy-file target_file target_dir #t))))

(define (anonymize-dir input_path output_path)
  ; create the regular and anonymized dirs
  (local [(define regular_path (build-path output_path "regular"))
		  (define anonymized_path (build-path output_path "anonymized"))]
		 (make-directory* regular_path)
		 (make-directory* anonymized_path)
		 (map (lambda (child_path) 
				; If it is a file, move it into a directory of the same name (minus extension)
				(if (file-exists? child_path)
				  (local [(define-values (parent_path child_filename child_mbd) (split-path child_path))]
						 (cond [(string=? (path->string child_filename) ".DS_Store") '()]
							   [else
								 (make-directory* (build-path regular_path (path-replace-extension child_filename "")))
								 (make-directory* (build-path anonymized_path (path-replace-extension child_filename "")))
								 (translate-file (build-path regular_path (path-replace-extension child_filename "") child_filename) child_path port->string)
								 (translate-file (build-path anonymized_path (path-replace-extension child_filename "") child_filename) child_path anonymize-txt-port)
								 ]))
				  (fold-files (lambda (file type cumm)
								(local [(define relative_file (find-relative-path input_path file))]
									   (cond 
										 [(eq? type 'file) 
										  (translate-file (build-path regular_path relative_file)
														  file
														  port->string)
										  (translate-file (build-path anonymized_path relative_file)
														  file
														  anonymize-txt-port)]
										 [(eq? type 'dir) 
										  (make-directory* (build-path regular_path relative_file))
										  (make-directory* (build-path anonymized_path relative_file))]
										 [else '()])))
							  '() child_path #f)))
			  (directory-list input_path #:build? #t))))

(if (null? (input_dirs))
  (local [(define input_port (open-input-string (read-line (current-input-port) 'return)))]
		 (anonymize-txt-port (if (is-wxme-stream? input_port)
							   (wxme-port->text-port input_port)
							   input_port)))
  (map (lambda (dir) (anonymize-dir dir (output_dir))) (input_dirs)))

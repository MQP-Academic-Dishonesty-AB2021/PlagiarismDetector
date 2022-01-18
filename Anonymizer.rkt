#lang racket
(require wxme)
(require racket/format)
(require racket/cmdline)

; The output directory of the script
(define output_dir (make-parameter "./output"))
(define input_dirs (make-parameter null))

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
(define (convert-rkt-to-txt rkt-file txt-file)
  (call-with-input-file rkt-file
						(λ (rkt-port)
						   (call-with-output-file txt-file
												  (λ (txt-port) (write-rkt-port-to-txt-port rkt-port txt-port)) #:exists 'replace))))

(define (corresponding-closing-brace opening-brace)
  (cond [(char=? opening-brace #\{) #\}]
		[(char=? opening-brace #\() #\)]
		[(char=? opening-brace #\[) #\]]
		[else #f]))

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
	  [else (string-append (string char) (anonymize-txt-port txt-port))])))

; str -> str -> void
; Parameters:
; rkt-port: the racket port of the input file
; txt-port: the text port of the output file
;
; Returns:
; void
(define (write-rkt-port-to-txt-port rkt-port txt-port)
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
		 (display (anonymize-txt-port
					(if (is-wxme-stream? rkt-port)
					  (wxme-port->text-port rkt-port)
					  rkt-port))
				  txt-port)))


(define
  (generate_filename)
  (local
	[(define random-number (random 4294967087))]
	(begin
	  (if (link-exists? (string-append output_dir "/" (~v random-number) ".txt"))
		(generate_filename)
		(string-append output_dir "/" (~v random-number) ".txt")))))

; (define (anonymize-dir input_dir) 
;   (local 
; 	[(define
; 	   ; path -> list(str)
; 	   (get-all-rkt-files path)
; 	   (foldl
; 		 ; list(path) -> list(str)
; 		 ; for each path,
; 		 ; if its a racket file, append,
; 		 ; if its a directory, recursively call
; 		 ; else don't do anything
; 		 (λ (new cumm)
; 			(local [(define filename (some-system-path->string new))]
; 				   (begin
; 					 (cond
; 					   [(string-suffix? filename ".rkt") (cons
; 														   (string-append
; 															 (some-system-path->string path)
; 															 "/"
; 															 filename)
; 														   cumm)]
; 					   [else cumm]))))
; 		 '() (directory-list path)))
; 	 (define input_paths
; 	   (foldl
; 		 (λ (new cumm)
; 			(append cumm
; 					(get-all-rkt-files (string->some-system-path new 'unix))))
; 		 '() input_dir))]
; 	(map (lambda (filename) (convert-rkt-to-txt filename (generate_filename)))
; 		 input_paths)))

(define (anonymize-dir input_path output_path)
  ; For each file in input_path recursively anonymize the directory
  (local 
	[(define (anon-file child-path)
	   (if (path-has-extension? child-path #".rkt")
		 (convert-rkt-to-txt (build-path input_path child-path) (build-path output_path child-path))
		 (copy-file (build-path input_path child-path) (build-path output_path child-path) #t)))
	 (define (anonymize-dir-helper current-path child-paths)
	   (if (null? child-paths)
		 (void)
		 (begin
		   (local [(define child-path (build-path current-path (car child-paths)))]
				  (cond [(file-exists? (build-path input_path child-path)) (anon-file child-path)]
						[(directory-exists? (build-path input_path child-path)) 
						 (begin 
						   (if (directory-exists? (build-path output_path child-path))
							 (void)
							 (make-directory (simplify-path (build-path output_path child-path))))
						   (anonymize-dir-helper child-path (directory-list (build-path input_path child-path))))]))
		   (anonymize-dir-helper current-path (cdr child-paths)))))]
	(anonymize-dir-helper (build-path 'same) (directory-list input_path))))

; (display (input_dirs))
(if (null? (input_dirs))
  (local [(define input_port (open-input-string (read-line (current-input-port) 'return)))]
		 (anonymize-txt-port (if (is-wxme-stream? input_port)
							   (wxme-port->text-port input_port)
							   input_port)))
  (map (lambda (dir) (anonymize-dir dir (output_dir))) (input_dirs)))

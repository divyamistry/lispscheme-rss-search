;; Author: Divya Mistry
;;         CSI 3331.01 Fall 2007
;; Exam 2 Part 2
;; Scheme implementation of the RSS Aggregator

;; required libraries for processing files and data
(require (lib "pregexp.ss"))
(require (lib "list.ss"))

;; These are all the hash-tables I'll need to 
;;   store words associated to title and description
;;   Links are stored in a separate table
;;   Titles are stored in separate table
(define titleHT (make-hash-table 'equal)) ;maps feed numbers to feed titles
(define linkHT (make-hash-table 'equal))  ;maps feed numbers to feed links
(define wordHT (make-hash-table 'equal))  ;maps words to list of feeds they appear in


;; keep a global counter for number of items
(define numItems 0)

;; process ALL items on current feed
;;   currFile: input-port of file currently being processed
(define (processFeed currFile)
  (if (not (null? (getAll currFile)))
      (begin
        ;(display "processing again\n")
        (processFeed currFile)
        )
      ;else
      (begin
        () ; return null once done processing
        )
      )
  )

;; process all files one after the other
;;   currFile: input-port of file currently being processed
(define (processFiles currFile)
  (if (null? (processFeed currFile))
      (begin
        ;(display "done with processFeed\n")
        (close-input-port currFile)
        )
      ;else
      (begin
        "THIS SHOULD NEVER HAPPEN\n";this cant happen. processFeed must ALWAYS return null
        )
      )
  )

;; GET next Item in the Feed of current file
;;   currFile: input-port of file currently being processed
(define (getItem currFile) 
  (if (regexp-match-peek "<item>(.*?)</item>" currFile)
      (begin
        ;get the whole item as string
        (bytes->string/locale (car (cdr (regexp-match "<item>(.*?)</item>" currFile))))
        )
      ;else
      () ;return null if no more items left
      )
  )


;; Put title, link, and description for an item in their corresponding hash-tables
;;   currFile: input-port of file currently being processed
(define (getAll currFile)
  (let ((tstr (getItem currFile)))
    (if (not (null? tstr))
        (begin
          ;capture title
          (if (not (regexp-match "<title>(.*?)</title>" tstr))
              (begin
                (hash-table-put! titleHT numItems "no title provided")
                )
              ;else
              (begin
                (hash-table-put! titleHT numItems (car (cdr (regexp-match "<title>(.*?)</title>" tstr))))
                (procDesc (car (cdr(regexp-match "<title>(.*?)</title>" tstr))))
                )
              )
          ; capture link
          (if (not (regexp-match "<link>(.*?)</link>" tstr))
              (hash-table-put! linkHT numItems "no link provided")
              ;else
              (hash-table-put! linkHT numItems (car (cdr (regexp-match "<link>(.*?)</link>" tstr))))
              )
          ;capture description
          (if (not (regexp-match "<description>(.*?)</description>" tstr))
              "no description available"
              ;else
              (procDesc (car (cdr (regexp-match "<description>(.*?)</description>" tstr))))
              )
          ;increment numItems once the item is processed
          (set! numItems (+ 1 numItems))
          )
        ;else
        () ; return null for further detection
        )
    )
  )

;; ADD word to the hash table with corresponding item number
;;   word: string that needs to be added to the hash-table
(define (addWord word)
  (let ((exo (hash-table-get wordHT word ())))
    (if (null? exo) 
        (begin
          ;(display "word was null")
          (hash-table-put! wordHT word (list numItems))
          )
        ;else
        (hash-table-put! wordHT word (cons numItems exo))
        )
    )
  )


;; PROCESS each word in the Description of item
;;    desc: description of the feed item currently being processed by the calling function
(define (procDesc desc)
  (let ((l (remove* `("") (pregexp-split "[, ;:`~!?.\"()]" desc)))) 
    (define (addAll)
      (if (null? (cdr l))
          ;we're last element of the list
          (begin
            (addWord (car l))
            )
          ;else // more elements left, so process them
          (begin
            (addWord (car l))
            ;get rid of first element
            (set! l (cdr l))
            ;process next one
            (addAll)
            )
          )
      )
    (addAll)
    )
  )


;; get individual file names
;;    global lastRead: boolean to flag that ALL filenames have been acquired from main rss source file
(define lastRead #f)
(define (getFiles)
  (let ((exo (regexp-match-peek ".*?: (.*?)\n" mainsrc))) ;check if filename can be read as expected
    (if exo
        (begin
          (let ((filename (bytes->string/locale (car (cdr (regexp-match ".*?: (.*?)\n" mainsrc)))))) ;get string version of the filename
            (define currfile (open-input-file filename)) ;open the file
            (processFiles currfile) ;process the file for its items
            (close-input-port currfile) ;close the file
            )
          (getFiles) ;start working on next files
          )
        ;else //reading last item which probably isn't ending with a \n. Could be #eof
        (begin
          (if (and (not lastRead) (regexp-match-peek ".*?: (.*?)$" mainsrc))
              (let ((filename (bytes->string/locale (car (cdr (regexp-match ".*?: (.*?)$" mainsrc))))))
                (define currfile (open-input-file filename))
                (processFiles currfile)
                (close-input-port currfile)
                )
              ;else
              )
          (set! lastRead #t) ;last filename was captured
          )
        )
    )
  )


;; main funciton to run the program
;;   srcfile: file that contains location of the feeds
;;   global mainsrc: input-port for rss.txt main source file
(define mainsrc #f)
(define (SchemeRSS srcfile)
  (if (and (> (string-length srcfile) 0) (file-exists? srcfile))
    (begin
      (set! mainsrc (open-input-file srcfile))
      (getFiles)
      (querydb)
    )
    ;
    (display "file not found. please try again.\n")
  )
  )


;; query function
;;   asks user to enter a search term, and calls appropriate method to display the result
(define (querydb)
  (display "\nEnter a search term (length between 3-13): <return to break>")
  (let ((qry (read-line)))
    (if (> (string-length qry) 2)
        ;string is long enough to show search
        (begin
          (showFeeds (hash-table-get wordHT qry ()))
          (querydb)
          )
        ;else
        (begin
          ;if empty string was given, exit.
          (if (= 0 (string-length qry))
              exit;
              ;otherwise display short-string message
              (begin
                (display "Search term too short for searching. (min length = 3)\n")
                (querydb)
                )
              )
          )
        )
    )
  )


;; show results based on given list of item ids
;;   itemsList: list of feed items where the search term was found
(define (showFeeds itemsList)
  (if (not (null? itemsList))
      (begin
        (if (< 30 (length itemsList)) ;if word appear more than 30 times, it is too common
            ;word is too common
            (display "\nGiven word is too common. Please try again with something more specific")
            ;else
            (begin
              (display (hash-table-get titleHT (car itemsList) ""))
              (display "\n")
              (display (hash-table-get linkHT (car itemsList) ""))
              (display "\n\n")
              (if (not (null? (cdr itemsList)))
                  (begin
                    ;don't print anything, but call the recursion again
                    (showFeeds (cdr itemsList))
                    )
                  ;else do nothing
                  )
              )
            )
        )
      ;else // itemsList is null, then there were no results found
      (display "No results found for your search term. Try again\n\n")
      )
  )


;; this function prints key-value pair has key=>value
;;   used as the for-each parameter to hash-table to print
;;   all the k-v pairs in hash table
(define (hash-table-toString k v)
  (display k)
  (display "=>")
  (display v)
  (display "\n")
  )

;final turn in
(display "Enter absolute path to access rss.txt: ")
(SchemeRSS (read-line))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; paul's parser for tom's own minimal language
;;;;
;;;; (C) 2013 Paul Nathan
;;;; License: LLGPL (http://opensource.franz.com/preamble.html)


;; Aim is to implement TOML v0.1
;;
;; https://github.com/mojombo/toml/blob/master/versions/toml-v0.1.0.md

(defpackage :pp-toml
  (:use
   :common-lisp)
  (:export
   ;; entry point for world
   :parse-string

   ;; testing entry points

   :extract-lisp-structure

   :not-special-case
   :datetime
   :whitespace
   :alphanumericp
   :string-char
   :keygroup-char
   :normal-key
   :string
   :number
   :bool
   :array-contents
   :array
   :value
   :end-of-information
   :keyvalue
   :keygroup
   :preamble
   :file-grammar
   :strip-comments))
(in-package :pp-toml)


(ql:quickload :esrap)
(ql:quickload '(:parse-number
                :alexandria
                :cl-ppcre
                :local-time))
(use-package :esrap)

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-bracket (char)
  (not (eql #\] char)))

(defun not-integer (string)
  (when (find-if-not #'digit-char-p string)
    t))

(defun not-special-case (char)
  (not (member char
               '(#\[
                 #\]
                 #\"
                 #\Space
                 #\Newline
                 #\tab
                 #\=
                 #\.))))

(defrule integer (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(defrule 4-integers (and integer integer integer integer))

(defrule 2-integers (and integer integer ) )

;;1979-05-27T07:32:00Z
(defrule datetime (and 4-integers #\- 2-integers #\- 2-integers #\T
                       2-integers #\: 2-integers #\: 2-integers #\Z)
  (:lambda (list)
    (list
     :datetime
     (local-time:parse-timestring
      (format nil "~{~a~}" (alexandria:flatten list))))))


(defrule whitespace
    (+ (or #\space #\tab #\newline))
  (:constant :ws))

(defrule alphanumeric
    (alphanumericp character))

(defrule string-char
    (or (not-doublequote character) (and #\\ #\')))

(defrule keygroup-char
    (or (not-bracket character) #\. ))

(defrule normal-key
    (+ (not-special-case character))
  (:text
    list))


(defun transliterate-unicode (string)
  (cl-ppcre:regex-replace-all
   "\\\\u(\\d{4})" string
   #'(lambda
         ;; Interface expected by regexp-replace-all
         (target-string start end match-start match-end reg-starts reg-ends)

       (declare (ignore start end match-start match-end))

       (format t "~a; ~a~&" reg-starts reg-ends)

       (let ((matched-code
              (subseq target-string
                      (aref reg-starts 0)
                      (aref reg-ends 0))))
         ;; convert the char into a string
         (string
         ;;convert the integer to the code character
          (code-char
           ;; convert the string to an integer
           (parse-integer matched-code)))))))

(defun transliterate-to-specials (string)

  (flet ((tr (target repl)
           (setf string (cl-ppcre:regex-replace-all
                         (cl-ppcre:quote-meta-chars
                          target)
                         string
                         (string repl)))))
    ;; alpha sorted
    (tr "\\b" #\Backspace)
    (tr "\\f" #\Form)
    (tr "\\n" #\Linefeed)
    (tr "\\r" #\Return)
    (tr "\\t" #\Tab)

    ;; todo: determine why this is commented out
    ;;   (tr "\\\"" #\")
    (tr "\/" #\/)
    (tr "\\\\" #\\)))

(defrule string-contents (* (or (and "\\" "\"")
                                string-char
                                ))
   (:lambda (s)
     (format nil "~{~c~}"
             (loop for var in s collect
                                (if (listp var)
                                    #\"
                                    var)))))

(defrule string (and #\" string-contents #\")
  (:destructure (q1 string q2)
    (declare (ignore q1 q2))
    (list
     :string
     (transliterate-to-specials
      (transliterate-unicode
       (text string))))))


(defrule number (and (? "-" ) (and
                               (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
                               (?
                                (and
                                 #\.
                                 (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))))))
  (:destructure (sign list)
    (list
     :number
     (if sign
         (parse-number:parse-number (text (push sign list)) :radix 10)
         (parse-number:parse-number (text list) :radix 10)))))

(defrule bool (or "true" "false")
  (:lambda (thing)
    (list
     :bool
     (if (string= thing "true")
         ;; Return keywords to allow a semantic walker ignore them when stripping out foos
         :true
         :false))))

(defrule array-contents (and value
                             (* (and (? whitespace )
                                     #\,
                                     (? whitespace) value))
                             (? whitespace)
                             (? #\,)
                             (? whitespace))
  (:lambda (thing)
    ;; Drop the whitespace captures
    (let ((array-list
            (butlast (butlast (butlast thing)))))
      (append
       (list (car array-list))
       ;; strip the whitespace and comma info
       (loop for group in (cadr array-list)
             collect
             (fourth group))))))

(defrule array (and #\[
                    (? whitespace)
                    array-contents
                    (? whitespace) #\])
  (:lambda (thing)
    (list
     :array
     (third thing))))

(defrule value
    (or
     array
     datetime
     bool
     number
     string
     ))

(defrule end-of-information (and (* (or #\Space #\tab))
                                 #\Newline)
  (:constant :ws))

(defrule keyvalue
    (and (? whitespace)
         normal-key
         (? whitespace)
         #\=
         (? whitespace)
         value
         end-of-information)
  (:destructure (w1 key w2 e1 w3 value w4)
    (declare (ignore w1 w2 e1 w3 w4))
    (list
     :keyvalue
     key value)))

(defrule keygroup
    (and (? whitespace) #\[ (+ keygroup-char) #\] (? whitespace))
  (:destructure (_1 _2 name _3 _4)
    (declare (ignore _1 _2 _3 _4))
    (list :header
          (text name))))


(defparameter *comment-scanner*
  (cl-ppcre:create-scanner
   ;; initial regex kindly contributed by geekosaur@irc.freenode.net
   "^(([^#\"]|\"(([^\\\"]*\\.)*[^\\\"]*\"))+)?#(.*)$"
   :single-line-mode nil
   :multi-line-mode t)
  "Scanner for # comments. Handles \"#\" strings")

(defun strip-comments (string)
  "Strips the comments from the string"
  (cl-ppcre:regex-replace-all
   *comment-scanner*
   string
   "\\1"))

(defrule preamble (* keyvalue))

(defrule file-grammar (and
                       preamble
                       ;; interleaving
                       (* (and
                           keygroup
                           (* keyvalue)
                           ))))

(defun parse-string (string)
  "Returns the toml parsed structure from `string` or :parse-error"
  (parse 'file-grammar string))

(defun extract-lisp-structure (parsed-structure)
  ;; Expecting parsed-structure to be two lists of lists. List 1
  ;; will be the keys not belonging to a top-level value. List 2 is
  ;; teh list of values belonging to top-level keys.
  (let ((table (make-hash-table
                ;; Will be comparing strings for the keys
                :test #'equal)))


    ;;pass 1: get all unheadered keys into a hash table
    (loop for keyvalue in (first parsed-structure)
       do (setf (gethash (second keyvalue) table)
                (process-value-data (third keyvalue))))

    ;; pass 2: normalize headers into keyvalues.
    ;;
    ;; as partof this pass, duplicate keys are detected and an error
    ;; is thrown
    ;;
    ;; [h1.h2] key1 = t => h1.h2.key1
    (loop for header in (second parsed-structure)
       do

         (assert (eq (caar header) :header))
         (let ((keygroup-header (cadar header))
               (section (cadr header)))
           (format t "header: ~a~&" keygroup-header)
           (unless section
             (setf (gethash keygroup-header table) nil))
           (loop for keyvalue in section do
                (format t "keyvalue: ~a~&" keyvalue)
                (assert (eq (first keyvalue) :keyvalue))
                (let ((key
                       ;; Format the key
                       (format nil "~a.~a"
                               keygroup-header
                               (second keyvalue))))
                  (multiple-value-bind (value gotten)
                      (gethash key table)
                    (declare (ignore value))
                    ;; Duplicate value detection!
                    (assert (not gotten) ))
                  (setf (gethash key table)
                       ;; collapse values from the (:type <stuff>) information.
                       (process-value-data (third keyvalue)))))))

    ;; break!  at this point: we have a flat common lisp hash table,
    ;; no duplicate keys. each value is a lisp value correctly
    ;; transcribed from the toml.

    ;; pass 3. place the h1.h2.key2 into nested hash tables h1 => h2>
    ;; key2 => value and remove the h1.h2.key2 key.

    table))

(defun process-value-data (value)
  "`value` may be any of the standard toml value types: array,
datetime, bool, number, or string. Of those, arrays are a special case:
Supposing an array is encountered, process-value-data recurses upon the array.

Toml does not support references  as of v0.1, and there for we can traverse arrays without cyles of references."
  (alexandria:switch ((first value) :test #'eq )
    (:array
     (loop for ele in (second value)
       collect (process-value-data ele)))
    (:bool
     (alexandria:switch ((second value))
      (:true t)
      (:false nil)
      (t
       (error "Unable to understand: ~a was thought to be a :BOOL must be :true or :false" (second value)))))
    (:datetime
     ;; The datetime is already parsed
     (second value))
    (:string
     ;; Text is already parsed as well
     (second value))
    (:number
     (second value))
    (t
     (error "Unable to understand: ~a; must be a legit pp-toml type" (first value)))))

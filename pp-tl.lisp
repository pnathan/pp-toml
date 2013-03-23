;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; paul's parser for tom's own minimal language
;;;;
;;;; (C) 2013 Paul Nathan
;;;; License: LLGPL (http://opensource.franz.com/preamble.html)

(defpackage :pp-toml
  (:use
   :common-lisp)
  (:export
   ;; entry point for world
   :parse-file

   ;; testing entry points
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
    (or (not-doublequote character) (and #\\ #\")))

(defrule keygroup-char
    (or (not-bracket character) #\. ))

(defrule normal-key
    (+ (not-special-case character))
  (:text
    list))

(defrule string (and #\" (* string-char) #\")
  (:destructure (q1 string q2)
    (declare (ignore q1 q2))
    (list
     :string
     (text string))))


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
     bool
     number
     string
     datetime
     array
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


(defun strip-comments (string)
  ;; regex contributed by geekosaur
  (cl-ppcre:regex-replace
   "^(([^#\"]|\"(([^\\\"]*\\.)*[^\\\"]*\"))+)#.*$"
   string
   "\\1"))

(defrule preamble (* keyvalue))

(defrule file-grammar (and
                       preamble
                       ;; interleaving
                       (* (and
                           (+ keygroup)
                           (+ keyvalue)
                           ))))

(defun parse-file (string)
  (parse 'file-grammar string))

(ql:quickload :esrap)
(ql:quickload :parse-number)
(ql:quickload :alexandria)
(ql:quickload :local-time)

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
    (local-time:parse-timestring
     (format nil "~{~a~}" (alexandria:flatten list)))))


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
    (text string)))


(defrule number (and (? "-" ) (and
                              (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
                              (?
                               (and
                                #\.
                                (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))))))
  (:destructure (sign list)
    (if sign
        (parse-number:parse-number (text (push sign list)) :radix 10)
        (parse-number:parse-number (text list) :radix 10))))

(defrule bool (or "true" "false")
  (:lambda (thing)
    (if (string= thing "true")
        t
        nil)))

(defrule keygroup
    (and (? whitespace) #\[ (+ keygroup-char) #\] (? whitespace)))


(defrule array-contents (and value (* (and (? whitespace )#\, (? whitespace) value)))
  )

(defrule trailing-array-contents (and (? whitespace) value (? whitespace) (? #\,))
  (:destructure (w1 v w2 c1)
    v))

(defrule array (and #\[
                    (? whitespace)
                    array-contents
                    (? whitespace)
                    trailing-array-contents
                    (? whitespace) #\]))

(defun recursively-remove-member-from-list (bag list test)
  (set-difference bag list :test test)
  (when list
    (cond
      ((listp (car list))
       (cons
        (recursively-remove-element-from-list bag (car list) test)
        (recursively-remove-element-from-list bag (cdr list) test)))
        ((funcall test (car list) bag)
         (recursively-remove-element-from-list bag (cdr list) test))
        (t
         (cons (car list)
               (recursively-remove-element-from-list bag (cdr list) test))))))

(defun remove-special-characters (tree)
  (recursively-remove-element-from-list
   '("," "[" :WS "]")
   tree
   #'(lambda (bag element)
       (format t "~a ~~> ~a => ~a~&" element bag (position element bag :test #'equal))
       (position element bag :test #'equal))))


(defrule value
    (or
     bool
     number
     ;string
     ;datetime
     array
     ))

(defrule keyvalue
    (and (? whitespace)
         normal-key
         (? whitespace)
         #\=
         (? whitespace)
         value)
  (:destructure (w1 key w2 e1 w3 value)
    (declare (ignore w1 w2 e1 w3))
    (list key value)))

(parse 'keygroup "[aaah]")

(parse 'keyvalue "aaa = \"bbb\"")

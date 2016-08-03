
(asdf:defsystem #:chennai
  :serial t
  :description "CLOS Based Chess Programm"
  :author "Patrick Krusenotto <patrick.krusenotto@gmail.com>"
  :license ":-)"
  :depends-on (#:marshal #:cl-ppcre)
  :components ((:file "package")
               (:file "chennai")))

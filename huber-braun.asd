;;;; huber-braun.asd

(asdf:defsystem #:huber-braun
  :description "Describe huber-braun here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:defmodel)
  :components ((:file "package")
	       (:file "coupling-current")
               (:file "huber-braun")))

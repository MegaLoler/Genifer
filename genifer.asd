(defsystem #:genifer
  :description "A framework for defining S-expression syntax for languages."
  :author "MegaLoler"
  :serial t
  :components ((:module "src"
			:serial t
			:components
			((:file "util")
			 (:file "genifer")))
	       (:module "lang"
			:components
			((:file "javascript")))))

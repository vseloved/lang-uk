;;; (c) 2013-2016 Vsevolod Dyomkin

(in-package #:asdf-user)


(defsystem #:lang-uk
  :version (:read-file-line "version.txt" :at 0)
  :description "CL-NLP based microservices API for Ukrainian language."
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :maintainer "Vsevolod Dyomkin <vseloved@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:rutilsx #:cl-nlp #:woo #:local-time #:yason #:babel #:flexi-streams
               #+dev #:should-test)
  :serial t
  :components
  ((:module #:src
            :serial t
            :components
            ((:file "packages")
             (:file "core")
             (:file "lexics")))
   #+dev
   (:module #:test
            :components
            ())))

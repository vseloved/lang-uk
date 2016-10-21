;;; (c) 2016 Vsevolod Dyomkin

(in-package #:lang-uk)
(named-readtables:in-readtable rutilsx-readtable)

;;; Test API definition:
;;; http://online.swagger.io/validator/debug?url=http://uk.lisp.kiev.ua/swagger


(defparameter *api-paths* #h(equal)
  "Supported API endpoints.")

(defgeneric api-process (path data)
  (:documentation
   "Specific processing of an API PATH request containing the payload DATA."))


(defun run-api (req)
  (let ((path (getf req :path-info))
        (method (getf req :request-method)))
    (format t "~A ~A ~A~%" (local-time:now) method path)
    (finish-output)
    (cond
      ((starts-with "/swagger" path)
       (list 200 '(:content-type "application/json")
             (list (with-output-to-string (out)
                     (yason:encode
                      #h("swagger" "2.0"
                         "basePath" "/"
                         "paths" *api-paths*
                         "x-microservice-taxonomy" '("test")
                         "info" #h("version" "0.1.0"
                                   "contact" #h("name" "Vsevolod Dyomkin"
                                                "email" "vseloved@gmail.com")
                                   "description" "Ukrainian language processing using CL-NLP"
                                   "license" #h("name" "Apache")
                                   "title" "wiki-lang-detect")
                         "schemes" '("http")
                         "host" "uk.lisp.kiev.ua")
                      out)))))
      ((and (get# path *api-paths*)
            (eql :POST method))
       (handler-case
           (with ((buf (make-array (getf req :content-length)
                                   :element-type 'flex:octet))
                  (data (yason:parse
                         (babel:octets-to-string
                          (progn (read-sequence buf (getf req :raw-body))
                                 buf)
                          :encoding :utf-8))))
             (api-process (mkeyw (slice path 1)) data))
         (error (e)
           (format *error-output* "~A" e)
           '(500 nil nil))))
      (t '(404 nil nil)))))

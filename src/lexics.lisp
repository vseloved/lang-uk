;;; (c) 2016 Vsevolod Dyomkin

(in-package #:lang-uk)
(named-readtables:in-readtable rutilsx-readtable)


(defparameter *lemma-api-limit* 1000)

(defvar *vesum*
  (nlex:load-mem-dict (merge-pathnames "data/dict-uk.txt"
                                       (asdf:component-pathname
                                        (asdf:find-system :lang-uk))))
  "Velykyi Slovnyk Suchasnoyi Ukrayinskoyi Movy
   (Big dictionary of contemporary Ukrainian Language).")

(:= (nlex::dict-pos-precedence *vesum*)
    '(tag::conj tag::prep tag::excl tag::insert tag::numr
      tag::noun tag::verb tag::adj tag::adjp tag::adv tag::advp))


(defmethod api-process ((path (eql :lemma)) data)
  (with ((((words "words") (tags "tags")) ? data))
    (cond ((> (length words) *lemma-api-limit*)
           (list 400 '(:content-type "text/plain")
                 (list (fmt "Number of requested words exceeds maxLength (~A): ~A"
                            *lemma-api-limit* (length words)))))
          ((and tags (/= (length words) (length tags)))
           (list 400 '(:content-type "text/plain")
                 (list (fmt "Number of requested words doesn't agree with requested tags: ~A vs ~A"
                            (length words) (length tags)))))
          (t
           (list 200 '(:content-type "application/json;charset=utf-8")
                 (let ((rez (mapcar ^(with ((word tag (nlex:lemmatize
                                                       *vesum* %
                                                       (mksym %% :package :tag))))
                                       (pair word tag))
                                    words
                                    (or tags (loop :repeat (length words)
                                                   :collect nil)))))
                   (list (with-output-to-string (out)
                           (yason:encode
                            (list (mapcar 'lt rez)
                                  (mapcar ^(fmt "~{~(~A~)~^:~}" (rt %))
                                          rez))
                            out)))))))))

(:= (? *api-paths* "/lemma")
    #h("post" #h("consumes" '("application/json")
                 "produces" '("application/json")
                 "tags" '("lemmatization" "lang-uk" "vesum" "brown-uk")
                 "x-taskClass" "lemmatize"
                 "x-taskAlgo" "vseloved"
                 "x-taskModel" "default"
                 "parameters"
                 (list
                  #h("name" "words"
                     "in" "body"
                     "description" "Words to lemmatize"
                     "required" t
                     "schema" #h("type" "array"
                                 "items" #h("type" "string")
                                 "maxLength" 1000))
                  #h("name" "pos"
                     "in" "body"
                     "description" "Optional target POS tags for each word"
                     "schema" #h("type" "array"
                                 "items" #h("type" "string")
                                 "maxLength" 1000)))
                 "responses"
                 #h("500" #h("description" "Internal server error")
                    "400" #h("description" "Bad request")
                    "200" #h("examples"
                             #h("application/json"
                                '(("людина" "любити" ",")
                                  (("noun")
                                   ("verb")
                                   ("unk"))
                                  (t t nil)))
                             "description" "Lemmas for each word,
                                            their tags and information
                                            if they were found"
                             "schema" #h("type" "array"
                                         "items" (list #h("type" "array"
                                                          "items" #h("type" "string"))
                                                       #h("type" "array"
                                                          "items" #h("type" "string"))
                                                       #h("type" "array"
                                                          "items" #h("type" "boolean")))))))))

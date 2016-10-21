(push "../rutils/" asdf:*central-registry*)
(push "../cl-nlp/" asdf:*central-registry*)
(push "./" asdf:*central-registry*)

(ql:quickload :lang-uk)

(woo:run 'lang-uk::run-api :port 7777)

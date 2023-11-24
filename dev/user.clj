(ns user)

(comment
  (require '[portal.api :as p])
  (def p (p/open))
  (add-tap #'p/submit))

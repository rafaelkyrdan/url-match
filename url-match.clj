;;
;; Matcher should recognize and destruct URL by:
;; host: domain
;; path: parts, splitted with "/"
;; queryparam: name/value pairs of query
;; (see examples below)
;; 
;; Each string that is started from "?" is a "bind"
;; (recognize matcher) should return nil or seq of binds
;;
 
(defn host "host pattern" [input] (str "^" "https?://" input ""))

(defn path "parts pattern" [part] (clojure.string/replace part #"\?[^&\/\?]+" "([^&\\/?]+)"))

(defn queryparam "query paattern" [query] (clojure.string/replace query #"\?[\S]+" "([^&\\/?]+)"))

(defn trim-and-split "sanitize pattern before apply it" [input] (clojure.string/split (clojure.string/trim input) #"(\))|\("))

(defn new-pattern
  "build new pattern"
  [input]
  (loop [patterns (clojure.string/split input #";") result ()]
    (if (empty? patterns)
      result
      (let [[head & remaining] patterns]
        (recur remaining
          (into result
            (do (let [[pattern param] (trim-and-split head)]
                  (let [output (@(resolve (symbol pattern)) param)] [output])
                )
            )
          )
        )
      )
    )
  ))

(defn check
  "apply pattern to input string"
  [pattern input]
  (let [output (re-find (re-pattern pattern) input)]
    (if (string? output)
        [output]
        (rest output)))
  )

(defn in? "true if coll contains elm" [coll elm] (some #(= elm %) coll))

(defn recognize-failed?
  "check if pattern failed"
  [input]
  (let [[host-check & remaining] input]
    (if (empty? host-check)
      nil
      (if (in? remaining ())
        nil
        remaining)))
  )

(defn recognize
  "recognize and destruct URL"
  [patterns input]
  (loop [checks patterns result ()]
    (if (empty? checks)
      (recognize-failed? result)
      (let [[head & remaining] checks]
        (recur remaining
            (into result (do (let [x (check head input)] [x])))
          )
        )
      )
    )
  )


(def twitter (new-pattern "host(twitter.com); path(?user/status/?id);"))
(recognize twitter "http://twitter.com/bradfitz/status/562360748727611392")
; (("bradfitz" "562360748727611392"))

(def dribbble (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);"))
(recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
; (("1905065-Travel-Icons-pack") ("1"))

(recognize dribbble "https://twitter.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
; nil ;; host mismatch

(recognize dribbble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users")
; nil ;; offset queryparam missing

(def dribbble2 (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset); queryparam(list=?type);"))

(recognize dribbble2 "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
; (("1905065-Travel-Icons-pack") ("1") ("users"))




(ns speaknoteasy.core
  (:require [instaparse.core :as insta]
            )
  (:use hiccup.core
        clojure.test)
  )

;; (use '[cemerick.pomegranate :only (add-dependencies)])
;; (cemerick.pomegranate/add-dependencies 
;;  :coordinates '[[somelibrary "0.0.1"]]
;;  :repositories {"clojars" "http://clojars.org/repo" } )

(insta/set-default-output-format! :hiccup)


(def mrenderer
  {:heading (fn [heading-bullet & x]
              (let [nstar (count (rest heading-bullet))
                ]

                [(keyword (str "h" nstar))
                 (apply
                  str
                  (map #(second %) (filter (fn [[tag content]] (= tag :text)) x)))]
              ))
   }
  )

(defn org-to-html [org-string]
  (html (rest (insta/transform mrenderer (orgmode org-string)))))
(defn- o2h [& arg] (apply org-to-html arg))


(defn decommented [s]
  (clojure.string/join
   "\n"
   (filter
    #(and (> (count %) 0) (not= \# (.charAt % 0)))
    (.split
     s
     "\n"))))

(do
  (def orgmode
    ;; (insta/parser "file:./src/speaknoteasy/orgmode.instaparse")
    ;; filter out commented lines
    (insta/parser
     (decommented (slurp "src/speaknoteasy/orgmode.instaparse"))))
)
(let [t (orgmode "* helo\n") ;; (orgmode (slurp "src/speaknoteasy/example.org"))
      ]

  (insta/transform mrenderer t)
  )

(deftest heading
  (is (= "<h1>hello</h1>" (o2h "* hello\n")))
  (is (= "<h2>hello</h2>" (o2h "** hello\n")))
  )

(deftest formatting
  (is (= "<h1>hello</h1>" (o2h "* hello\n")))
  (is (= "<h2>hello</h2>" (o2h "** hello\n")))
  )

(orgmode "*bold*")
(orgmode "*bold* inline *bold* with text *bold*")
(orgmode "inline _underline_ with text")
(orgmode "inline /italic!/ with text")
(orgmode "inline =code=  with text")
(orgmode "inline ~verbatim~ with text")

(orgmode
 "Here are 2 kinds of links: [[link-no-description]] does not contain a description but [[link-path][description of the link-path]] does.")

(let [grammar (decommented (slurp "src/speaknoteasy/test.instaparse"))
      ]
  ((insta/parser (decommented grammar))
   ;; "[[link-path][description of the link-path]]"
   "[[link-no-description]]"
   )
  )


(orgmode "[[link-path][description of the link-path]] *asdf*")
(orgmode "a a")

(defn retrievebytag [ls-orig tagname]
  ((fn myfn [ls]
     (if (not (coll? ls))
       nil
       (let [remain (rest ls)]
         (if (= (first ls) tagname)

           remain
           (do
             (mapcat myfn remain))
           )))
     
     ) ls-orig))

(let [
      expected "bold"

      text "
*bold* inline *bold* with text *bold*
inline _underline_ with text
inline /italic!/ with text
inline =code=  with text
inline ~verbatim~ with text


"
      ]

  ;; (= expected (apply str (retrievebytag ((insta/parser grammar) text) :any)))
  ;; (apply str (retrievebytag ((insta/parser grammar) text) :any))
  (orgmode text)
  )

(orgmode "*bold* inline *bold* with text *bold*")
(orgmode "inline _underline_ with text")
(orgmode "inline /italic!/ with text")
(orgmode "inline =code=  with text")
(orgmode "inline ~verbatim~ with text")



(run-tests)



((insta/parser
  "S = (a | b)+; a = 'a'; b = 'b'")
 "ab")
((insta/parser
  "S ='*' (!'z' #'.') '*'")
 "*x*")
((insta/parser
  "S = !'ab' ('a' | 'b')+")
 "baaaaab")


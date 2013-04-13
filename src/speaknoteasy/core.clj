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

(do
  (def orgmode
    (insta/parser "file:./src/speaknoteasy/orgmode.instaparse"))
  (let [t (orgmode "* helo\n") ;; (orgmode (slurp "src/speaknoteasy/example.org"))

        ]

    (insta/transform mrenderer t)
    ))

(deftest heading
  (is (= "<h1>hello</h1>" (o2h "* hello\n")))
  (is (= "<h2>hello</h2>" (o2h "** hello\n")))
  )

(run-tests)





document = headline | specialblock | comment | list | mixedtext | word | paragraphbreak | multiwhitespace | line
EOL = "\n" | "\r\n"
paragraphbreak = EOL EOL+

multiwhitespace = [\s]+
asterisk = [*]
nonasterisk = [^*]
mixedLineUntilEOL = multiwhitespace (mixedtext / (!EOL .))+
headline = asterisk+ mixedLineUntilEOL EOL
list = listunordered / listordered
listunordered = multiwhitespace ("-" / "+") mixedLineUntilEOL
    
listordered = multiwhitespace [0-9]+ ("." / ")") mixedLineUntilEOL
    
line = (!EOL .)* EOL

mixedtext = (hyperlink / codetext / boldtext / underlinedtext / italicstext / verbatimtext / horizontalrule / word)

boldtext = "*" (!"*" .)+ "*"

codetext = "=" (!"=" .)+ "="

underlinedtext = "_" (!"_" .)+ "_"

italicstext = "/" (!"/" .)+ "/"

verbatimtext = "~" (!"~" .)+ "~"

word = (!EOL ![\s] .)+

plaintext = (!EOL .)+

horizontalrule = "-----" "-"* EOL

hyperlink = "[[" (!"]" .)+ ("][" (!"]" .)+)? "]]"

specialblock = (specialblocksource / specialblockquote / specialblockcatchall)
rule specialblockcatchall = "#+" ("begin" / "BEGIN") (!EOL .)* EOL (!"#+end" !"#+END" .)* ("#+end" / "#+END") (!EOL .)* EOL
specialblockquote = "#+" ("begin_quote" / "BEGIN_QUOTE") (!EOL .)* EOL (!"#+end_quote" !"#+END_QUOTE" .)* ("#+end_quote" / "#+END_QUOTE") (!EOL [\s])* EOL
specialblocksource = "#+" ("begin_src" / "BEGIN_SRC") (!EOL .)* EOL (!"#+end_src" !"#+END_SRC" .)* ("#+end_src" / "#+END_SRC") (!EOL [\s])* EOL
comment = EOL? "#" (!EOL .)+ EOL

# defm

A Clojure library designed to provide a pattern matching defn-like construct called defm.

## Usage

````clojure
(defm file-or-string-fn []
  ([File] (println "It's a file"))
  ([String] (println "It's a string"))
  (:else (println "It's a " (type _1)))
````

## License

Copyright © 2014 Colin Taylor

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

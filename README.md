# defm

A Clojure library designed to provide a simple pattern matching defn-like construct called defm.

Defm dispatches on parameter type and value as well as number of parameters

## Usage

````clojure
(defm file-or-string-fn []
  ([File] (println "It's a file"))
  ([s :- String] (println "It's a string " s))
  (["magic"] (println "It's magic"))
  ([_] (println "It's a " (type _1))))
````

## License

Copyright © 2014 Colin Taylor

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

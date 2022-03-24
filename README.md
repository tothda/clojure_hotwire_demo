# Hotwire Demo with Clojure Backend

This is a small (and silly) example of integrating Hotwire with a Clojure backend.

## To start the demo application

* Start nrepl server with `clojure -M:nrepl`
* Connect your editor's nrepl client to localhost:21155
* Evaluate the whole `hotwire.clj`
* Start the system:
  * Go to the end of `hotwire.clj`
  * evaluate: `(bootstrap-db!)` to generate some test data
  * evalutate: `(start-jetty!)` to start the web server
* Visit http://localhost:21156/

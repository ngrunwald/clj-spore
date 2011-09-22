# clj-spore

This is a Clojure implementation of the [SPORE](https://github.com/SPORE/specifications) specification for RESTful clients. It uses spec files to generate functions calling http routes on a server. See [this post](http://lumberjaph.net/misc/2010/09/17/spore.html) and [this one](http://lumberjaph.net/misc/2010/10/20/spore-update.html) for more information or read the [specification](https://github.com/SPORE/specifications) for even greater details. This SPORE implementation is build upon the existing [clj-http](https://github.com/mmcgrana/clj-http) library and is middleware-compatible with it.

## Usage

For the most basic case:

```clojure
(use 'clj-spore)
(use 'clj-spore.middleware)

(let [client (load-spec-from-file "test/ihackernews.json"
                                  :middlewares [wrap-json-format])
      res ((client :askhn_posts) :nextid "FiNf744LLx")]
  (if (= (res :status) 200)
    (doseq [item (get-in [:decoded-body "items"] res)]
      (println (item "title")))))
```

_load-spec-from-file_ takes a file path to the spec and optionally a list of middlewares and an :overload param that can be used to overload spec params, like this:

```clojure
(def client (load-spec-from-file "test/ihackernews.json"
                                  :middlewares [wrap-json-format]
                                  :overload {:base_url "http://localhost:8765"}))
```

The body of the request is given by the :payload arg and :middlewares can contain more complex instanciation, like so:

```clojure
(def client (load-spec-from-file "test/ihackernews.json" :middlewares [[wrap-clojure-response
                                                                        :enabled-if #(not= (:path %) "/login")
                                                                        :args [:type "application/x-clojure"]]]))
```

You can check the tests to see more use cases and read the docstrings of the functions to go further.

## Thanks

- [Linkfluence](http://us.linkfluence.net/) for sponsoring this work.

## License

Copyright (C) 2011 Nils Grunwald

Distributed under the Eclipse Public License, the same as Clojure.

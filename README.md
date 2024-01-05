# re-frame TodoMVC with Reitit and Malli

Single-file (see `src/todomvc/main.cljs`) implementation of the 
[re-frame](https://day8.github.io/re-frame/re-frame/) 
[TodoMVC](https://todomvc.com/) 
[sample application](https://github.com/day8/re-frame/tree/master/examples/todomvc)
using
[Reitit](https://cljdoc.org/d/metosin/reitit/0.7.0-alpha7/doc/introduction)
and
[Malli](https://cljdoc.org/d/metosin/malli/0.13.0/doc/readme)
(instead of 
[Secretary](https://github.com/clj-commons/secretary) 
and 
[`clojure.spec`](https://clojure.org/about/spec),
respectively).

Furthermore,
this implementation makes use of 
[ULIDs](https://github.com/ulid/spec)
as todo IDs,
as opposed to integer IDs.

As mentioned in the reference codebase linked above,
the re-frame TodoMVC application is not a minimal implementation.
Instead,
it is meant to be something of a showcase of re-frame functionality.

Recent versions of Node and Clojure are required.

## Explore the implementation

Install dependencies: `npm install`

Start Shadow CLJS: `npm run dev`

View the application by visiting `localhost:8000` in your web browser.

Open and connect from an editor of your choice to explore the implementation. 
([Calva](https://calva.io/) for VSCode is an accessible option.)

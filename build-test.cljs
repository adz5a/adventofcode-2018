(require 'lumo.build.api)
(lumo.build.api/build
  "src"
  {:output-to "test.js"
   :output-dir "out"
   :main 'advent.test
   :target :nodejs})


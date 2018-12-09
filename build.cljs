(require 'lumo.build.api)
(lumo.build.api/build
  "src"
  {:output-to "main.js"
   :output-dir "out"
   :main 'advent.core
   :target :nodejs})

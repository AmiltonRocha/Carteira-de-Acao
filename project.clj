(defproject gilsontrade "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"] ;; depedencia que faz a programação em clojure
                 [cheshire "5.11.0"] ;; dependencia para parsear JSON
                 [clj-http "3.12.3"] ;; dependencia para fazer requisições HTTP
                 [org.clojure/tools.cli "1.0.214"]];; dependencia para leitura de argumentos
                  
  :main ^:skip-aot gilsontrade.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})

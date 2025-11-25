(ns back.handler
  {:clj-kondo/ignore [:unresolved-symbol :unused-referred-var :refer-all]} ;; aqui e paradesabilitar o linter
  (:require [compojure.core :refer :all] ; no-linter
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]))

(defroutes app-routes
  (GET "/" [] "Hello World")
  (POST "/compra" [] "Hello World")
  (route/not-found "Not Found"))

(def app
  (wrap-defaults app-routes site-defaults))



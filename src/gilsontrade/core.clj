;; Nesta pagina e a onde colocamos o codigo principal do projeto
(ns gilsontrade.core
  (:require [clj-http.client :as http-client] ;; biblioteca para fazer requisicoes HTTP
            [cheshire.core :as json]) ;; biblioteca para parsear JSON
  (:gen-class))

(def api-local-url "http://localhost:3000") ;; URL da API local













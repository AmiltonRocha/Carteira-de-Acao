(ns back.handler
  {:clj-kondo/ignore [:unresolved-symbol :unused-referred-var :refer-all]} ;; aqui e paradesabilitar o linter
  (:require [compojure.core :refer :all] ; no-linter
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [clj-http.client :as http-client] ;; biblioteca para fazer requisicoes HTTP
            [cheshire.core :as json] ;; biblioteca para parsear JSON
            [ring.util.response :as response])) ;; biblioteca para respostas HTTP

(def key-api "g967zghLxWPwMp72fdAaFU") ;; 
(def api-url "https://brapi.dev/api/quote/") ;;brapi

(defn buscar-dados-acao
  "Busca os dados de uma acao na API brapi.dev e retorna o resultado"
  [codigo-acao]
  (let [codigo (.toUpperCase codigo-acao) ;; isso deixa o codigo da acao em maiuscula
        url-completa (str api-url codigo) ;; isso completa a url com o codigo da acao
        response (http-client/get url-completa {:headers {"Authorization" (str "Bearer " key-api)} ;; adiciona o token no header
                                                :throw-exceptions false}) ;; aqui e a onde faz o get la na api
        dados-json (json/parse-string (:body response) true) ;; Traducao do formato da api para o formato do clojure
        resultado (first (:results dados-json))] ;; pega o primeiro resultado do array de resultados
    resultado)) ;; retorna o resultado (sem imprimir)

(defroutes app-routes 
  "Rotas da aplicacao"

  (GET "/acao/:codigo" [codigo] 
    (let [resultado (buscar-dados-acao codigo)
          codigo (:symbol resultado)
          nome (:longName resultado)
          ultimo-preco (:regularMarketPrice resultado)
          preco-maximo-do-dia (:regularMarketDayHigh resultado)
          preco-minimo-do-dia (:regularMarketDayLow resultado)
          preco-de-abertura (:regularMarketOpen resultado)
          preco-de-fechamento (:regularMarketPreviousClose resultado)
          hora (:regularMarketTime resultado)]
          (-> {:codigo codigo
               :nome nome
               :ultimo-preco ultimo-preco
               :preco-maximo-do-dia preco-maximo-do-dia
               :preco-minimo-do-dia preco-minimo-do-dia
               :preco-de-abertura preco-de-abertura
               :preco-de-fechamento preco-de-fechamento
               :hora hora}
              json/generate-string ;; aqui esta gerando uma string com o json
              response/response ;; com a resposta da api pegamos o body e transformamos em json
              (response/content-type "application/json; charset=utf-8")))) ;; ele manda a requisicao para o a brapi e retorna o json
  (POST "/compra" [] "Hello World")
  (route/not-found "Not Found"))


(def app
  (wrap-defaults app-routes site-defaults))

(ns back.handler
  {:clj-kondo/ignore [:unresolved-symbol :unused-referred-var :refer-all]} ;; aqui e paradesabilitar o linter
  (:require [compojure.core :refer :all] ; no-linter
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults api-defaults]]
            [clj-http.client :as http-client] ;; biblioteca para fazer requisicoes HTTP
            [cheshire.core :as json] ;; biblioteca para parsear JSON
            [ring.util.response :as response])) ;; biblioteca para respostas HTTP

(def key-api "g967zghLxWPwMp72fdAaFU") ;; 
(def api-url "https://brapi.dev/api/quote/") ;;brapi

;; Atom para armazenar as transacoes em memoria
(def transacoes (atom [])) ;; lista vazia inicialmente
;;criar uma lista de atons para poder fazer uma lista 

(defn buscar-dados-acao
  "Busca os dados de uma acao na API brapi.dev e retorna o resultado ou mapa com erro"
  [codigo-acao]
  (let [codigo (.toUpperCase codigo-acao) ;; isso deixa o codigo da acao em maiuscula
        url-completa (str api-url codigo) ;; isso completa a url com o codigo da acao
        response (http-client/get url-completa {:headers {"Authorization" (str "Bearer " key-api)} ;; adiciona o token no header
                                                :throw-exceptions false}) ;; aqui e a onde faz o get la na api
        status (:status response)
        body (:body response)]
    (if (= status 200)
      (try
        (let [dados-json (json/parse-string body true)
              results (:results dados-json)]
          (if (and results (seq results))
            (first results) ;; pega o primeiro resultado do array
            {:erro-api (str "Acao " codigo " nao encontrada na API brapi.dev")}))
        (catch Exception e
          {:erro-api (str "Erro ao processar resposta da API: " (.getMessage e))}))
      {:erro-api (str "Erro HTTP " status " ao acessar API brapi.dev. Verifique se o codigo " codigo " esta correto.")})))

(defn registra-compra
  "Registra uma compra de acao e armazena no atom de transacoes"
  [codigo quantidade preco]
  (let [codigo-upper (.toUpperCase codigo) ;; converte o codigo para maiuscula
        valor-total (* quantidade preco) ;; calcula o valor total da compra
        data-atual (str (java.time.LocalDate/now)) ;; obtem a data atual
        transacao {:tipo "compra"
                   :codigo codigo-upper
                   :quantidade quantidade
                   :preco-unitario preco
                   :valor-total valor-total
                   :data data-atual}]
    (swap! transacoes conj transacao) ;; adiciona a transacao ao atom
    transacao)) ;; retorna a transacao criada

(defn calcular-saldo-acoes
  "Calcula quantas acoes de um codigo especifico o usuario possui"
  [codigo]
  (let [codigo-upper (.toUpperCase codigo)
        compras (filter #(and (= (:tipo %) "compra")
                              (= (:codigo %) codigo-upper))
                        @transacoes)
        vendas (filter #(and (= (:tipo %) "venda")
                             (= (:codigo %) codigo-upper))
                       @transacoes)
        total-compras (reduce + 0 (map :quantidade compras))
        total-vendas (reduce + 0 (map :quantidade vendas))]
    (- total-compras total-vendas)))

(defn registra-venda
  "Registra uma venda de acao e armazena no atom de transacoes"
  [codigo quantidade]
  (let [codigo-upper (.toUpperCase codigo)
        saldo-atual (calcular-saldo-acoes codigo-upper)
        dados-acao (buscar-dados-acao codigo-upper)
        preco-atual (:regularMarketPrice dados-acao)
        valor-total (* quantidade preco-atual)
        data-atual (str (java.time.LocalDate/now))
        transacao {:tipo "venda"
                   :codigo codigo-upper
                   :quantidade quantidade
                   :preco-unitario preco-atual
                   :valor-total valor-total
                   :data data-atual}]
    (if (>= saldo-atual quantidade)
      (do
        (swap! transacoes conj transacao)
        transacao)
      {:erro (str "Saldo insuficiente. Voce possui " saldo-atual " acoes de " codigo-upper " e tentou vender " quantidade)})))

(defroutes app-routes 
  "Rotas da aplicacao"

  (GET "/acao/:codigo" [codigo] 
    (let [resultado (buscar-dados-acao codigo)]
      (if-let [erro (:erro-api resultado)]
        (-> {:erro erro}
            json/generate-string
            response/response
            (response/content-type "application/json; charset=utf-8")
            (response/status 404))
        (let [codigo-symbol (:symbol resultado)
              nome (:longName resultado)
              ultimo-preco (:regularMarketPrice resultado)
              preco-maximo-do-dia (:regularMarketDayHigh resultado)
              preco-minimo-do-dia (:regularMarketDayLow resultado)
              preco-de-abertura (:regularMarketOpen resultado)
              preco-de-fechamento (:regularMarketPreviousClose resultado)
              hora (:regularMarketTime resultado)]
          (-> {:codigo codigo-symbol
               :nome nome
               :ultimo-preco ultimo-preco
               :preco-maximo-do-dia preco-maximo-do-dia
               :preco-minimo-do-dia preco-minimo-do-dia
               :preco-de-abertura preco-de-abertura
               :preco-de-fechamento preco-de-fechamento
               :hora hora}
              json/generate-string
              response/response
              (response/content-type "application/json; charset=utf-8"))))))
  (POST "/compra" {body :body}
    (try
      (let [dados (json/parse-string (slurp body) true)
            codigo (:codigo dados)
           quantidade (:quantidade dados)
            preco (:preco dados)
            transacao (registra-compra codigo quantidade preco)]
        (-> transacao
            json/generate-string
            response/response
            (response/content-type "application/json; charset=utf-8")
            (response/status 201)))
      (catch Exception _
        (-> {:erro "Dados invalidos. Envie: codigo, quantidade e preco"}
            json/generate-string
            response/response
            (response/content-type "application/json; charset=utf-8")
            (response/status 400)))))
  (GET "/transacoes" []
    (-> @transacoes
        json/generate-string
        response/response
        (response/content-type "application/json; charset=utf-8")))
  (route/not-found "Not Found"))

;;(defn extrato-por-periudo [] vai precisar de uma data inicial e uma final)


(def app
  (wrap-defaults app-routes api-defaults))

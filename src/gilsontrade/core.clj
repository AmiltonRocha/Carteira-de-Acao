;; Nesta pagina e a onde colocamos o codigo principal do projeto
(ns gilsontrade.core
  (:require [clj-http.client :as http-client] ;; biblioteca para fazer requisicoes HTTP
            [cheshire.core :as json] ;; biblioteca para parsear JSON
            [clojure.string :as str]) ;; biblioteca para manipular strings
  (:gen-class)) ;;
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

(defn -main
  "Inicio do projeto."
  [& args]
  (if (empty? args)
    (println "Uso: lein run <CODIGO_ACAO>\nExemplo: lein run PETR4")
    (let [codigo-acao (.toUpperCase (first args)) ;; isso deixa o codigo da acao em maiuscula
          resultado (buscar-dados-acao codigo-acao) ;; busca os dados usando a funcao base
          codigo (:symbol resultado) ;; pega o codigo da acao
          nome (:longName resultado) ;; pega o nome completo da acao
          preco (:regularMarketPrice resultado) ;; pega o preco atual da acao
          variacao (:regularMarketChange resultado) ;; pega a variacao do preco
          variacao-percent (:regularMarketChangePercent resultado) ;; pega a variacao percentual
          preco-de-abertura (:regularMarketOpen resultado) ;; pega o preco de abertura
          preco-de-fechamento (:regularMarketPreviousClose resultado) ;; pega o preco de fechamento anterior
          hora (:regularMarketTime resultado)] ;; pega a hora da ultima atualizacao
      (println "\n=== Cotacao da Acao ===")
      (println "Codigo:" codigo)
      (println "Nome:" nome)
      (println "Preco Atual: R$" preco)
      (println "Variacao:" variacao "(" variacao-percent "%)") 
      (println "Maxima do Dia: R$" (:regularMarketDayHigh resultado))
      (println "Minima do Dia: R$" (:regularMarketDayLow resultado))
      (println "Volume:" (:regularMarketVolume resultado))
      (println "Preco de Abertura: R$" preco-de-abertura)
      (println "Preco de Fechamento: R$" preco-de-fechamento)
      (println "Hora: " hora)))) 



;;# Consultar uma ação específica
;;lein run PETR4






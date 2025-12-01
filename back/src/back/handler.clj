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

(defn converter-para-numero
  "Converte um valor para numero (double), aceita string, number ou tipos numericos do Java"
  [valor]
  (cond
    (nil? valor)
    nil
    (string? valor)
    (try
      (Double/parseDouble valor)
      (catch Exception _
        nil))
    (number? valor)
    (double valor)
    (instance? Number valor)
    (double valor)
    :else
    nil))

(defn calcular-valor-total
  "Calcula o valor total de uma transacao "
  [quantidade preco-unitario]
  (* quantidade preco-unitario))

(defn filtrar-transacoes
  "Filtra transacoes por codigo e tipo "
  [transacoes codigo tipo]
  (let [codigo-upper (.toUpperCase codigo)]
    (filter #(and (= (:tipo %) tipo)
                  (= (:codigo %) codigo-upper))
            transacoes)))

(defn data-posterior-ou-igual?
  "Verifica se data1 e posterior ou igual a data2 "
  [data1 data2]
  (>= (compare data1 data2) 0))

(defn data-entre?
  "Verifica se data esta entre data-inicial e data-final (inclusive) "
  [data data-inicial data-final]
  (and (data-posterior-ou-igual? data data-inicial)
       (data-posterior-ou-igual? data-final data)))

(defn data-venda-valida?
  "Valida se a data da venda e valida em relacao a data da compra mais antiga "
  [data-venda data-compra-antiga]
  (if data-compra-antiga
    (data-posterior-ou-igual? data-venda data-compra-antiga)
    true))  ; Se nao ha compra, permite vender

(defn calcular-saldo-acoes-puro
  "Calcula o saldo de acoes a partir de uma lista de transacoes "
  [transacoes codigo]
  (let [codigo-upper (.toUpperCase codigo)
        compras (filtrar-transacoes transacoes codigo-upper "compra")
        vendas (filtrar-transacoes transacoes codigo-upper "venda")
        total-compras (reduce + 0 (map :quantidade compras))
        total-vendas (reduce + 0 (map :quantidade vendas))]
    (- total-compras total-vendas)))

(defn obter-codigos-unicos
  "Retorna uma lista de codigos unicos de acoes a partir das transacoes (funcao pura)"
  [transacoes]
  (distinct (map :codigo transacoes)))

(defn calcular-saldo-todas-acoes-puro
  "Calcula o saldo de todas as acoes a partir de uma lista de transacoes (funcao pura)"
  [transacoes]
  (let [codigos-unicos (obter-codigos-unicos transacoes)]
    (map (fn [codigo]
           {:codigo codigo
            :quantidade (calcular-saldo-acoes-puro transacoes codigo)})
         codigos-unicos)))

(defn data-compra-mais-antiga
  "Retorna a data da compra mais antiga de uma acao a partir de uma lista de transacoes "
  [transacoes codigo]
  (let [codigo-upper (.toUpperCase codigo)
        compras (filtrar-transacoes transacoes codigo-upper "compra")]
    (if (seq compras)
      (apply min (map :data compras))
      nil)))

(defn registra-compra
  "Registra uma compra de acao e armazena no atom de transacoes"
  [codigo quantidade preco data]
  (let [codigo-upper (.toUpperCase codigo) ;; converte o codigo para maiuscula
        valor-total (calcular-valor-total quantidade preco) ;; calcula o valor total da compra
        ;; Usa a data fornecida, ou data atual se nao fornecida
        data-compra (if data data (str (java.time.LocalDate/now)))
        transacao {:tipo "compra"
                   :codigo codigo-upper
                   :quantidade quantidade
                   :preco-unitario preco
                   :valor-total valor-total
                   :data data-compra}]
    (swap! transacoes conj transacao) ;; adiciona a transacao ao atom
    transacao)) ;; retorna a transacao criada

(defn calcular-saldo-acoes
  "Calcula quantas acoes de um codigo especifico o usuario possui"
  [codigo]
  (calcular-saldo-acoes-puro @transacoes codigo))

(defn calcular-saldo-todas-acoes
  "Calcula o saldo de todas as acoes na carteira"
  []
  (calcular-saldo-todas-acoes-puro @transacoes))

(defn data-compra-mais-antiga-wrapper
  "Retorna a data da compra mais antiga de uma acao especifica"
  [codigo]
  (data-compra-mais-antiga @transacoes codigo))

(defn registra-venda
  "Registra uma venda de acao e armazena no atom de transacoes"
  [codigo quantidade data]
  ;; Valida se quantidade e um numero valido
  (if (not (and (number? quantidade) (> quantidade 0)))
    {:erro (str "Quantidade invalida. Deve ser um numero maior que zero. Recebido: " quantidade " (tipo: " (type quantidade) ")")}
    (let [codigo-upper (.toUpperCase codigo)
          ;; Garante que quantidade seja double
          quantidade-num (double quantidade)
          ;; Usa a data fornecida, ou data atual se nao fornecida
          data-venda (if data data (str (java.time.LocalDate/now)))
          ;; Valida se a data da venda nao e anterior a data da compra mais antiga DA ACAO ESPECIFICA
          data-compra-antiga (data-compra-mais-antiga-wrapper codigo-upper)]
    ;; Primeiro valida se existe compra da acao especifica
    (if (nil? data-compra-antiga)
      {:erro (str "Nao e possivel vender " codigo-upper " sem ter comprado antes. Registre uma compra primeiro.")}
      ;; Valida se a data da venda nao e anterior a data da compra mais antiga DA ACAO ESPECIFICA
      (if (not (data-venda-valida? data-venda data-compra-antiga))
        {:erro (str "Nao e possivel vender " codigo-upper " antes da data de compra. Data da compra mais antiga de " codigo-upper ": " data-compra-antiga "")}
        (let [saldo-atual (calcular-saldo-acoes codigo-upper)
              dados-acao (buscar-dados-acao codigo-upper)]
        (if (or (nil? dados-acao) (:erro-api dados-acao))
          {:erro (str "Erro ao buscar dados da acao " codigo-upper ". Tente novamente.")}
          (let [preco-atual (:regularMarketPrice dados-acao)]
            (if (nil? preco-atual)
              {:erro (str "Preco da acao " codigo-upper " nao disponivel. Tente novamente.")}
              (let [valor-total (calcular-valor-total quantidade-num preco-atual)
                    transacao {:tipo "venda"
                               :codigo codigo-upper
                               :quantidade quantidade-num
                               :preco-unitario preco-atual
                               :valor-total valor-total
                               :data data-venda}]
                (if (>= saldo-atual quantidade-num)
                  (do
                    (swap! transacoes conj transacao)
                    transacao)
                  {:erro (str "Saldo insuficiente. Voce possui " saldo-atual " acoes de " codigo-upper " e tentou vender " quantidade-num)})))))))))))

(defn extrato-por-periodo
  "Filtra transacoes entre uma data inicial e uma data final (formato: YYYY-MM-DD)"
  [data-inicial data-final]
  (let [todas-transacoes @transacoes
        ;; Filtra transacoes cuja data esta entre data-inicial e data-final (inclusive)
        transacoes-filtradas (filter (fn [transacao]
                                       (data-entre? (:data transacao) data-inicial data-final))
                                     todas-transacoes)]
    transacoes-filtradas))

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
            data (:data dados)  ;; Extrai a data do body (opcional)
            transacao (registra-compra codigo quantidade preco data)]
        (-> transacao
            json/generate-string
            response/response
            (response/content-type "application/json; charset=utf-8")
            (response/status 201)))
      (catch Exception _
        (-> {:erro "Dados invalidos. Envie: codigo, quantidade, preco e data (formato: YYYY-MM-DD)"}
            json/generate-string
            response/response
            (response/content-type "application/json; charset=utf-8")
            (response/status 400)))))
  (POST "/venda" {body :body}
    (try
      (let [body-str (if (string? body) body (slurp body))
            dados (json/parse-string body-str true)
            codigo (:codigo dados)
            quantidade-raw (:quantidade dados)
            data (:data dados)
            ;; Converte quantidade para numero usando funcao auxiliar
            quantidade (converter-para-numero quantidade-raw)]  ;; Extrai a data do body (opcional)
        (if (and codigo quantidade data (number? quantidade) (> quantidade 0))
          (let [transacao (registra-venda codigo quantidade data)]
            (if-let [erro (:erro transacao)]
              (-> {:erro erro}
                  json/generate-string
                  response/response
                  (response/content-type "application/json; charset=utf-8")
                  (response/status 400))
              (-> transacao
                  json/generate-string
                  response/response
                  (response/content-type "application/json; charset=utf-8")
                  (response/status 201))))
          (-> {:erro (str "Dados invalidos. Envie: codigo, quantidade (numero > 0) e data (formato: YYYY-MM-DD). Recebido - codigo: " codigo ", quantidade: " quantidade-raw " (" (type quantidade-raw) "), data: " data)}
              json/generate-string
              response/response
              (response/content-type "application/json; charset=utf-8")
              (response/status 400))))
      (catch Exception e
        (-> {:erro (str "Dados invalidos. Envie: codigo, quantidade e data (formato: YYYY-MM-DD). Erro: " (.getMessage e))}
            json/generate-string
            response/response
            (response/content-type "application/json; charset=utf-8")
            (response/status 400)))))
  (GET "/transacoes" []
    (-> @transacoes
        json/generate-string
        response/response
        (response/content-type "application/json; charset=utf-8")))
  (GET "/saldo" []
    (-> (calcular-saldo-todas-acoes)
        json/generate-string
        response/response
        (response/content-type "application/json; charset=utf-8")))
  (GET "/transacoes/periodo" {query-params :query-params}
    (let [data-inicial (get query-params "data-inicial")
          data-final (get query-params "data-final")]
      (if (and data-inicial data-final)
        (let [transacoes-filtradas (extrato-por-periodo data-inicial data-final)]
          (-> transacoes-filtradas
              json/generate-string
              response/response
              (response/content-type "application/json; charset=utf-8")))
        (-> {:erro "Parametros obrigatorios: data-inicial e data-final (formato: YYYY-MM-DD)"}
            json/generate-string
            response/response
            (response/content-type "application/json; charset=utf-8")
            (response/status 400)))))
  (route/not-found "Not Found"))

(def app
  (wrap-defaults app-routes api-defaults))

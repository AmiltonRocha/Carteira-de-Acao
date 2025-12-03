(ns back.handler
     {:clj-kondo/ignore [:unresolved-symbol :unused-referred-var :refer-all]}
     (:require [compojure.core :refer :all]
               [compojure.route :as route]
               [ring.middleware.defaults :refer [wrap-defaults site-defaults api-defaults]]
               [clj-http.client :as http-client]
               [cheshire.core :as json]
               [ring.util.response :as response]
               [clojure.string :as str]))

   (def key-api "g967zghLxWPwMp72fdAaFU")
   (def api-url "https://brapi.dev/api/quote/")

   ;; Atom para armazenar as transacoes em memoria
   (def transacoes (atom []))

   ;; ===== FUNÇÕES DE CONVERSÃO DE DATA =====

   (defn timestamp-para-data
     "Converte timestamp Unix (em SEGUNDOS) para string YYYY-MM-DD"
     [timestamp]
     (try
       (let [timestamp-long (long timestamp)
             ;; A API brapi retorna timestamp em SEGUNDOS, não milissegundos
             ;; Verificamos se é menor que um valor típico em milissegundos
             ;; Se for menor que 10000000000 (ano 2286 em segundos), está em segundos
             timestamp-millis (if (< timestamp-long 10000000000)
                                (* timestamp-long 1000)
                                timestamp-long)
             resultado (-> timestamp-millis
                           java.time.Instant/ofEpochMilli
                           (.atZone (java.time.ZoneId/of "America/Sao_Paulo"))
                           .toLocalDate
                           str)]
         resultado)
       (catch Exception e
         (println "[ERRO] Erro ao converter timestamp:" timestamp "Erro:" (.getMessage e))
         nil)))

   (defn data-para-timestamp
     "Converte string YYYY-MM-DD para timestamp Unix (inicio do dia)"
     [data-str]
     (try
       (-> (java.time.LocalDate/parse data-str)
           (.atStartOfDay (java.time.ZoneId/of "America/Sao_Paulo"))
           .toInstant
           .getEpochSecond)
       (catch Exception e
         (println "Erro ao converter data:" data-str (.getMessage e))
         nil)))

   (defn data-e-hoje?
     "Verifica se uma data (string no formato YYYY-MM-DD) e igual a data de hoje"
     [data]
     (= data (str (java.time.LocalDate/now))))

   (defn data-no-passado-ou-hoje?
     "Verifica se a data não é futura"
     [data-str]
     (try
       (let [data-informada (java.time.LocalDate/parse data-str)
             data-hoje (java.time.LocalDate/now)]
         (not (.isAfter data-informada data-hoje)))
       (catch Exception _
         false)))

   ;; ===== FUNÇÕES DE BUSCA DE DADOS =====

   (defn buscar-dados-historicos
     "Busca dados historicos de uma acao na API brapi.dev usando range e interval"
     [codigo-acao range interval]
     (let [codigo (.toUpperCase codigo-acao)
           url-completa (str api-url codigo "?range=" range "&interval=" interval)
           response (http-client/get url-completa {:headers {"Authorization" (str "Bearer " key-api)}
                                                   :throw-exceptions false})
           status (:status response)
           body (:body response)]
       (if (= status 200)
         (try
           (let [dados-json (json/parse-string body true)
                 results (:results dados-json)]
             (if (and results (seq results))
               (first results)
               {:erro-api (str "Acao " codigo " nao encontrada na API brapi.dev")}))
           (catch Exception e
             {:erro-api (str "Erro ao processar resposta da API: " (.getMessage e))}))
         {:erro-api (str "Erro HTTP " status " ao acessar API brapi.dev. Verifique se o codigo " codigo " esta correto.")})))

   (defn filtrar-preco-por-data
     "Filtra os dados historicos para encontrar o preco de uma data especifica ou a mais proxima"
     [dados-historicos data-desejada]
     (if (:erro-api dados-historicos)
       dados-historicos
       (let [historico (:historicalDataPrice dados-historicos)]
         (if (nil? historico)
           {:erro-api "Dados historicos nao encontrados na resposta da API"}
           (let [historico-com-datas (map (fn [item]
                                            (let [data-conv (timestamp-para-data (:date item))]
                                              (assoc item :data-formatada data-conv)))
                                          historico)
                 historico-valido (filter #(:data-formatada %) historico-com-datas)
                 preco-exato (first (filter #(= data-desejada (:data-formatada %))
                                            historico-valido))]
             (if preco-exato
               {:regularMarketPrice (:close preco-exato)
                :symbol (:symbol dados-historicos)
                :longName (:longName dados-historicos)}
               ;; Se não encontrou exata, busca a mais próxima ANTERIOR
               (let [datas-anteriores (filter #(neg? (compare (:data-formatada %) data-desejada))
                                              historico-valido)
                     mais-proxima (first (sort-by :data-formatada #(compare %2 %1)
                                                  datas-anteriores))]
                 (if mais-proxima
                   {:regularMarketPrice (:close mais-proxima)
                    :symbol (:symbol dados-historicos)
                    :longName (:longName dados-historicos)
                    :aviso (str "Data " data-desejada " nao disponivel (fim de semana/feriado). Usando "
                                (:data-formatada mais-proxima))}
                   {:erro-api (str "Nenhum dado historico disponivel para " data-desejada)}))))))))

(defn buscar-dados-acao
  "Busca os dados de uma acao na API brapi.dev. Se data for fornecida, busca preco historico daquela data"
  [codigo-acao & {:keys [data]}]
  (let [codigo (.toUpperCase codigo-acao)]
    (if (and data (not (data-e-hoje? data)))
      ;; Se data foi fornecida E nao e hoje, busca preco historico
      (let [dados-historicos (buscar-dados-historicos codigo "1y" "1d")]
        (filtrar-preco-por-data dados-historicos data))
      ;; Se nao tem data OU e hoje, busca preco atual
      (let [url-completa (str api-url codigo)
            response (http-client/get url-completa {:headers {"Authorization" (str "Bearer " key-api)}
                                                    :throw-exceptions false})
            status (:status response)
            body (:body response)]
        (if (= status 200)
          (try
            (let [dados-json (json/parse-string body true)
                  results (:results dados-json)]
              (if (and results (seq results))
                (first results)
                {:erro-api (str "Acao " codigo " nao encontrada na API brapi.dev")}))
            (catch Exception e
              {:erro-api (str "Erro ao processar resposta da API: " (.getMessage e))}))
          {:erro-api (str "Erro HTTP " status " ao acessar API brapi.dev. Verifique se o codigo " codigo " esta correto.")})))))

;; ===== FUNÇÕES AUXILIARES =====

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
  "Calcula o valor total de uma transacao"
  [quantidade preco-unitario]
  (* quantidade preco-unitario))

(defn filtrar-transacoes
  "Filtra transacoes por codigo e tipo"
  [transacoes codigo tipo]
  (let [codigo-upper (.toUpperCase codigo)]
    (filter #(and (= (:tipo %) tipo)
                  (= (:codigo %) codigo-upper))
            transacoes)))

(defn data-posterior-ou-igual?
  "Verifica se data1 e posterior ou igual a data2"
  [data1 data2]
  (>= (compare data1 data2) 0))

(defn data-entre?
  "Verifica se data esta entre data-inicial e data-final (inclusive)"
  [data data-inicial data-final]
  (and (data-posterior-ou-igual? data data-inicial)
       (data-posterior-ou-igual? data-final data)))

(defn data-venda-valida?
  "Valida se a data da venda e valida em relacao a data da compra mais antiga"
  [data-venda data-compra-antiga]
  (if data-compra-antiga
    (data-posterior-ou-igual? data-venda data-compra-antiga)
    true))

;; ===== FUNÇÕES DE CÁLCULO DE SALDO =====

(defn calcular-saldo-acoes-puro
  "Calcula o saldo de acoes a partir de uma lista de transacoes"
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
  "Retorna a data da compra mais antiga de uma acao a partir de uma lista de transacoes"
  [transacoes codigo]
  (let [codigo-upper (.toUpperCase codigo)
        compras (filtrar-transacoes transacoes codigo-upper "compra")]
    (if (seq compras)
      (first (sort (map :data compras)))
      nil)))

;; ===== FUNÇÕES DE REGISTRO =====

(defn registra-compra
  "Registra uma compra de acao e armazena no atom de transacoes"
  [codigo quantidade preco data]
  (let [codigo-upper (.toUpperCase codigo)
        valor-total (calcular-valor-total quantidade preco)
        data-compra (if data data (str (java.time.LocalDate/now)))
        transacao {:tipo "compra"
                   :codigo codigo-upper
                   :quantidade quantidade
                   :preco-unitario preco
                   :valor-total valor-total
                   :data data-compra}]
    (swap! transacoes conj transacao)
    transacao))

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
  (if (not (and (number? quantidade) (> quantidade 0)))
    {:erro (str "Quantidade invalida. Deve ser um numero maior que zero. Recebido: " quantidade " (tipo: " (type quantidade) ")")}
    (let [codigo-upper (.toUpperCase codigo)
          quantidade-num (double quantidade)
          data-venda (if data data (str (java.time.LocalDate/now)))
          data-compra-antiga (data-compra-mais-antiga-wrapper codigo-upper)]
      (if (nil? data-compra-antiga)
        {:erro (str "Nao e possivel vender " codigo-upper " sem ter comprado antes. Registre uma compra primeiro.")}
        (if (not (data-venda-valida? data-venda data-compra-antiga))
          {:erro (str "Nao e possivel vender " codigo-upper " antes da data de compra. Data da compra mais antiga de " codigo-upper ": " data-compra-antiga)}
          (let [saldo-atual (calcular-saldo-acoes codigo-upper)
                dados-acao (buscar-dados-acao codigo-upper :data data-venda)]
            (if (or (nil? dados-acao) (:erro-api dados-acao))
              {:erro (str "Erro ao buscar dados da acao " codigo-upper ". " (or (:erro-api dados-acao) "Tente novamente."))}
              (let [preco-atual (:regularMarketPrice dados-acao)
                    aviso (:aviso dados-acao)]
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
                        (if aviso
                          (assoc transacao :aviso aviso)
                          transacao))
                      {:erro (str "Saldo insuficiente. Voce possui " saldo-atual " acoes de " codigo-upper " e tentou vender " quantidade-num)})))))))))))

(defn extrato-por-periodo
  "Filtra transacoes entre uma data inicial e uma data final (formato: YYYY-MM-DD)"
  [data-inicial data-final]
  (let [todas-transacoes @transacoes
        transacoes-filtradas (filter (fn [transacao]
                                       (data-entre? (:data transacao) data-inicial data-final))
                                     todas-transacoes)]
    transacoes-filtradas))

;; ===== ROTAS DA API =====

(defroutes app-routes
  "Rotas da API da aplicacao"

  (GET "/acao/:codigo" {params :params query-params :query-params}
    (let [codigo (:codigo params)
          data-param (get query-params "data")
          resultado (if data-param
                      (buscar-dados-acao codigo :data data-param)
                      (buscar-dados-acao codigo))]
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
              hora (:regularMarketTime resultado)
              aviso (:aviso resultado)]
          (-> (merge {:codigo codigo-symbol
                      :nome nome
                      :ultimo-preco ultimo-preco
                      :preco-maximo-do-dia preco-maximo-do-dia
                      :preco-minimo-do-dia preco-minimo-do-dia
                      :preco-de-abertura preco-de-abertura
                      :preco-de-fechamento preco-de-fechamento
                      :hora hora}
                     (when aviso {:aviso aviso}))
              json/generate-string
              response/response
              (response/content-type "application/json; charset=utf-8"))))))

  (POST "/compra" {body :body}
    (try
      (let [body-str (if (string? body) body (slurp body))
            dados (json/parse-string body-str true)
            codigo (:codigo dados)
            quantidade-raw (:quantidade dados)
            preco-raw (:preco dados)
            data (:data dados)
            quantidade (converter-para-numero quantidade-raw)
            preco (converter-para-numero preco-raw)]

        (if (and data (not (data-no-passado-ou-hoje? data)))
          (-> {:erro "Data invalida. Nao e possivel registrar transacoes em datas futuras."}
              json/generate-string
              response/response
              (response/content-type "application/json; charset=utf-8")
              (response/status 400))

          (if (and codigo quantidade preco (number? quantidade) (> quantidade 0) (number? preco) (> preco 0))
            (let [transacao (registra-compra codigo quantidade preco data)]
              (-> transacao
                  json/generate-string
                  response/response
                  (response/content-type "application/json; charset=utf-8")
                  (response/status 201)))
            (-> {:erro (str "Dados invalidos. Envie: codigo, quantidade (numero > 0), preco (numero > 0) e data (formato: YYYY-MM-DD). Recebido - codigo: " codigo ", quantidade: " quantidade-raw ", preco: " preco-raw)}
                json/generate-string
                response/response
                (response/content-type "application/json; charset=utf-8")
                (response/status 400)))))
      (catch Exception e
        (-> {:erro (str "Dados invalidos. Envie: codigo, quantidade, preco e data (formato: YYYY-MM-DD). Erro: " (.getMessage e))}
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
            quantidade (converter-para-numero quantidade-raw)]

        (if (and data (not (data-no-passado-ou-hoje? data)))
          (-> {:erro "Data invalida. Nao e possivel registrar transacoes em datas futuras."}
              json/generate-string
              response/response
              (response/content-type "application/json; charset=utf-8")
              (response/status 400))

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
                (response/status 400)))))
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
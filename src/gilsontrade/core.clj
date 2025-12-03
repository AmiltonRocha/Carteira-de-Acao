(ns gilsontrade.core
   (:require [clj-http.client :as http-client]
             [cheshire.core :as json]
             [clojure.string :as str])
   (:gen-class))

 (def api-local-url "http://localhost:3000")

 ;; ===== FUNÇÕES DE VALIDAÇÃO =====

 (defn validar-formato-data
   "Valida se a string está no formato YYYY-MM-DD"
   [data-str]
   (try
     (java.time.LocalDate/parse data-str)
     true
     (catch Exception _
       false)))

 (defn data-valida?
   "Verifica se a data é válida e não é futura"
   [data-str]
   (and (validar-formato-data data-str)
        (try
          (let [data-informada (java.time.LocalDate/parse data-str)
                data-hoje (java.time.LocalDate/now)]
            (not (.isAfter data-informada data-hoje)))
          (catch Exception _
            false))))

 ;; ===== FUNÇÕES DE COMUNICAÇÃO COM API =====

 (defn buscar-dados-acao
   "Consulta os dados de uma acao na API local. Se data for fornecida, busca preco historico"
   [codigo & {:keys [data]}]
   (let [url (if data
               (str api-local-url "/acao/" codigo "?data=" data)
               (str api-local-url "/acao/" codigo))
         response (http-client/get url {:throw-exceptions false})
         status (:status response)]
     (if (= status 200)
       (if-let [dados (json/parse-string (:body response) true)]
         dados
         (do
           (println "Erro ao processar resposta da API")
           nil))
       (do
         (println "Erro ao consultar acao. Status HTTP:" status)
         nil))))

 (defn registrar-compra
   "Registra uma compra de acao"
   [codigo quantidade data]
   (try
     (let [dados-acao (buscar-dados-acao codigo :data data)]
       (if (and dados-acao (not (:erro dados-acao)))
         (if-let [preco-atual (:ultimo-preco dados-acao)]
           (let [url (str api-local-url "/compra")
                 dados-json (json/generate-string {:codigo codigo
                                                   :quantidade quantidade
                                                   :preco preco-atual
                                                   :data data})
                 response (http-client/post url {:body dados-json
                                                 :content-type "application/json"
                                                 :accept :json
                                                 :throw-exceptions false})
                 status (:status response)]
             (if (= status 201)
               (try
                 (let [resultado (json/parse-string (:body response) true)]
                   resultado)
                 (catch Exception e
                   {:erro (str "Erro ao processar resposta do servidor: " (.getMessage e))}))
               (if-let [body (:body response)]
                 (try
                   (let [erro (json/parse-string body true)]
                     erro)
                   (catch Exception e
                     {:erro (str "Erro HTTP " status ". Resposta: " (subs body 0 (min 200 (count body))))}))
                 {:erro (str "Erro HTTP " status ". Sem resposta do servidor.")})))
           {:erro (str "Preco da acao nao disponivel. Dados recebidos: " (pr-str dados-acao))})
         (if-let [erro (:erro dados-acao)]
           {:erro erro}
           {:erro "Erro ao buscar dados da acao. Tente novamente."})))
     (catch Exception e
       {:erro (str "Erro ao conectar com o servidor: " (.getMessage e))})))

 (defn registrar-venda
   "Registra uma venda de acao"
   [codigo quantidade data]
   (try
     (let [dados-acao (buscar-dados-acao codigo)]
       (if (and dados-acao (not (:erro dados-acao)))
         (if-let [preco-atual (:ultimo-preco dados-acao)]
           (let [url (str api-local-url "/venda")
                 dados-json (json/generate-string {:codigo codigo
                                                   :quantidade quantidade
                                                   :data data})
                 response (http-client/post url {:body dados-json
                                                 :content-type "application/json"
                                                 :accept :json
                                                 :throw-exceptions false})
                 status (:status response)]
             (if (= status 201)
               (try
                 (let [resultado (json/parse-string (:body response) true)]
                   resultado)
                 (catch Exception e
                   {:erro (str "Erro ao processar resposta do servidor: " (.getMessage e))}))
               (if-let [body (:body response)]
                 (try
                   (let [erro (json/parse-string body true)]
                     erro)
                   (catch Exception e
                     {:erro (str "Erro HTTP " status ". Resposta: " (subs body 0 (min 200 (count body))))}))
                 {:erro (str "Erro HTTP " status ". Sem resposta do servidor.")})))
           {:erro (str "Preco da acao nao disponivel. Dados recebidos: " (pr-str dados-acao))})
         (if-let [erro (:erro dados-acao)]
           {:erro erro}
           {:erro "Erro ao buscar dados da acao. Tente novamente."})))
     (catch Exception e
       {:erro (str "Erro ao conectar com o servidor: " (.getMessage e))})))

 (defn buscar-transacoes
   "Busca todas as transacoes"
   []
   (try
     (let [url (str api-local-url "/transacoes")
           response (http-client/get url {:throw-exceptions false})
           status (:status response)]
       (if (= status 200)
         (try
           (let [transacoes (json/parse-string (:body response) true)]
             transacoes)
           (catch Exception e
             {:erro (str "Erro ao processar resposta do servidor: " (.getMessage e))}))
         {:erro (str "Erro HTTP " status " ao buscar transacoes.")}))
     (catch Exception e
       {:erro (str "Erro ao conectar com o servidor: " (.getMessage e))})))

 (defn buscar-transacoes-por-periodo
   "Busca transacoes entre uma data inicial e final"
   [data-inicial data-final]
   (try
     (let [url (str api-local-url "/transacoes/periodo?data-inicial=" data-inicial "&data-final=" data-final)
           response (http-client/get url {:throw-exceptions false})
           status (:status response)]
       (if (= status 200)
         (try
           (let [transacoes (json/parse-string (:body response) true)]
             transacoes)
           (catch Exception e
             {:erro (str "Erro ao processar resposta do servidor: " (.getMessage e))}))
         (if-let [body (:body response)]
           (try
             (let [erro (json/parse-string body true)]
               erro)
             (catch Exception e
               {:erro (str "Erro HTTP " status ". Resposta: " (subs body 0 (min 200 (count body))))}))
           {:erro (str "Erro HTTP " status ". Sem resposta do servidor.")})))
     (catch Exception e
       {:erro (str "Erro ao conectar com o servidor: " (.getMessage e))})))

 (defn buscar-saldo-carteira
   "Busca o saldo de todas as acoes na carteira"
   []
   (try
     (let [url (str api-local-url "/saldo")
           response (http-client/get url {:throw-exceptions false})
           status (:status response)]
       (if (= status 200)
         (try
           (let [saldos (json/parse-string (:body response) true)]
             saldos)
           (catch Exception e
             {:erro (str "Erro ao processar resposta do servidor: " (.getMessage e))}))
         {:erro (str "Erro HTTP " status " ao buscar saldo da carteira.")}))
     (catch Exception e
       {:erro (str "Erro ao conectar com o servidor: " (.getMessage e))})))

 ;; ===== FUNÇÕES PURAS =====

 (defn calcular-valor-total-acao
   "Calcula o valor total de uma acao (quantidade * preco atual)"
   [quantidade preco-atual]
   (* quantidade preco-atual))

 ;; ===== FUNÇÕES DE EXIBIÇÃO =====

 (defn exibir-transacao
   "Exibe os dados de uma transacao"
   [transacao]
   (println "\nTipo:" (:tipo transacao))
   (println "Codigo:" (:codigo transacao))
   (println "Quantidade:" (:quantidade transacao))
   (println (format "Preco Unitario: R$ %.2f" (:preco-unitario transacao)))
   (println (format "Valor Total: R$ %.2f" (:valor-total transacao)))
   (println "Data:" (:data transacao))
   (when-let [aviso (:aviso transacao)]
     (println "Aviso:" aviso))
   (println "---"))

 (defn exibir-transacoes
   "Exibe uma lista de transacoes usando recursao"
   ([transacoes]
    (when (seq transacoes)
      (exibir-transacao (first transacoes))
      (recur (rest transacoes)))))

 (defn exibir-saldo-acao
   "Exibe o saldo de uma acao com preco atual e valor total"
   [saldo-acao]
   (let [codigo (:codigo saldo-acao)
         quantidade (:quantidade saldo-acao)]
     (if (> quantidade 0)
       (let [dados-acao (buscar-dados-acao codigo)]
         (if (and dados-acao (not (:erro dados-acao)))
           (if-let [preco-atual (:ultimo-preco dados-acao)]
             (let [valor-total (calcular-valor-total-acao quantidade preco-atual)]
               (println "\nCodigo:" codigo)
               (println "Quantidade:" quantidade)
               (println (format "Preco Atual: R$ %.2f" preco-atual))
               (println (format "Valor Total: R$ %.2f" valor-total))
               (println "---"))
             (println "\nCodigo:" codigo " - Erro ao buscar preco atual"))
           (println "\nCodigo:" codigo " - Erro ao buscar dados da acao"))))))

 ;; ===== MENU =====

 (defn exibir-menu
   "Exibe o menu principal"
   []
   (println "\n=== Gerenciador de Carteira de Acoes ===")
   (println "1. Consultar dados de uma acao")
   (println "2. Registrar compra de acao")
   (println "3. Registrar venda de acao")
   (println "4. Exibir extrato de transacoes")
   (println "5. Exibir saldo da carteira")
   (println "0. Sair")
   (print "\nEscolha uma opcao: "))

 (defn processar-opcao
   "Processa a opcao escolhida pelo usuario"
   [opcao]
   (cond
     (= opcao "1") (do
                     (print "Digite o codigo da acao (ex: PETR4): ")
                     (flush)
                     (let [codigo (read-line)
                           dados (buscar-dados-acao codigo)]
                       (if-let [dados-validos dados]
                         (do
                           (println "\n=== Dados da Acao ===")
                           (println "Codigo:" (:codigo dados-validos))
                           (println "Nome:" (:nome dados-validos))
                           (println (format "Ultimo Preco: R$ %.2f" (:ultimo-preco dados-validos)))
                           (println (format "Preco Maximo do Dia: R$ %.2f" (or (:preco-maximo-do-dia dados-validos) 0.0)))
                           (println (format "Preco Minimo do Dia: R$ %.2f" (or (:preco-minimo-do-dia dados-validos) 0.0)))
                           (println (format "Preco de Abertura: R$ %.2f" (or (:preco-de-abertura dados-validos) 0.0)))
                           (println (format "Preco de Fechamento: R$ %.2f" (or (:preco-de-fechamento dados-validos) 0.0)))
                           (println "Hora:" (:hora dados-validos))
                           (when-let [aviso (:aviso dados-validos)]
                             (println "\n*** AVISO:" aviso)))
                         (println "Erro ao consultar acao."))
                       true))

     (= opcao "2") (do
                     (print "Digite o codigo da acao: ")
                     (flush)
                     (let [codigo (read-line)]
                       (print "Digite a quantidade: ")
                       (flush)
                       (try
                         (let [quantidade-str (read-line)
                               quantidade (Double/parseDouble quantidade-str)]
                           (print "Digite a data da compra (formato: YYYY-MM-DD, ex: 2024-01-15): ")
                           (flush)
                           (let [data (read-line)]
                             (if (not (data-valida? data))
                               (println "\nErro: Data invalida ou futura. Use uma data no formato YYYY-MM-DD que não seja futura.")
                               (let [resultado (registrar-compra codigo quantidade data)]
                                 (if-let [erro (:erro resultado)]
                                   (println "\nErro:" erro)
                                   (do
                                     (println "\nCompra registrada com sucesso!")
                                     (println "Codigo:" (:codigo resultado))
                                     (println "Quantidade:" (:quantidade resultado))
                                     (println (format "Preco Unitario: R$ %.2f" (:preco-unitario resultado)))
                                     (println (format "Valor Total: R$ %.2f" (:valor-total resultado)))
                                     (println "Data:" (:data resultado))
                                     (when-let [aviso (:aviso resultado)]
                                       (println "\n*** AVISO:" aviso))))))))
                         (catch NumberFormatException _
                           (println "\nErro: Quantidade invalida. Digite um numero valido."))
                         (catch Exception ex
                           (println "\nErro inesperado:" (.getMessage ex))))
                       true))

     (= opcao "3") (do
                     (print "Digite o codigo da acao: ")
                     (flush)
                     (let [codigo (read-line)]
                       (print "Digite a quantidade: ")
                       (flush)
                       (try
                         (let [quantidade-str (read-line)
                               quantidade (Double/parseDouble quantidade-str)]
                           (print "Digite a data da venda (formato: YYYY-MM-DD, ex: 2024-01-15): ")
                           (flush)
                           (let [data (read-line)]
                             (if (not (data-valida? data))
                               (println "\nErro: Data invalida ou futura. Use uma data no formato YYYY-MM-DD que não seja futura.")
                               (let [resultado (registrar-venda codigo quantidade data)]
                                 (if-let [erro (:erro resultado)]
                                   (println "\nErro:" erro)
                                   (do
                                     (println "\nVenda registrada com sucesso!")
                                     (println "Codigo:" (:codigo resultado))
                                     (println "Quantidade:" (:quantidade resultado))
                                     (println (format "Preco Unitario: R$ %.2f" (:preco-unitario resultado)))
                                     (println (format "Valor Total: R$ %.2f" (:valor-total resultado)))
                                     (println "Data:" (:data resultado))
                                     (when-let [aviso (:aviso resultado)]
                                       (println "\n*** AVISO:" aviso))))))))
                         (catch NumberFormatException _
                           (println "\nErro: Quantidade invalida. Digite um numero valido."))
                         (catch Exception ex
                           (println "\nErro inesperado:" (.getMessage ex))))
                       true))

     (= opcao "4") (do
                     (print "Deseja filtrar por periodo? (s/n): ")
                     (flush)
                     (let [filtrar (read-line)]
                       (if (= (str/lower-case filtrar) "s")
                         (do
                           (print "Digite a data inicial (formato: YYYY-MM-DD, ex: 2024-01-01): ")
                           (flush)
                           (let [data-inicial (read-line)]
                             (print "Digite a data final (formato: YYYY-MM-DD, ex: 2024-12-31): ")
                             (flush)
                             (let [data-final (read-line)
                                   resultado (buscar-transacoes-por-periodo data-inicial data-final)]
                               (if-let [erro (:erro resultado)]
                                 (println "\nErro:" erro)
                                 (if (seq resultado)
                                   (do
                                     (println "\n=== Extrato de Transacoes (Periodo: " data-inicial " a " data-final ") ===")
                                     (exibir-transacoes resultado))
                                   (println "\nNenhuma transacao encontrada no periodo especificado."))))))
                         (let [resultado (buscar-transacoes)]
                           (if-let [erro (:erro resultado)]
                             (println "\nErro:" erro)
                             (if (seq resultado)
                               (do
                                 (println "\n=== Extrato de Transacoes ===")
                                 (exibir-transacoes resultado))
                               (println "\nNenhuma transacao encontrada.")))))
                       true))

     (= opcao "5") (do
                     (println "\n=== Saldo da Carteira ===")
                     (let [saldos (buscar-saldo-carteira)]
                       (if-let [erro (:erro saldos)]
                         (println "\nErro:" erro)
                         (if (seq saldos)
                           (do
                             (println "\nAcoes na carteira:")
                             (run! exibir-saldo-acao saldos)
                             (let [valor-total-carteira (reduce + 0 (map (fn [saldo]
                                                                           (let [codigo (:codigo saldo)
                                                                                 quantidade (:quantidade saldo)
                                                                                 dados-acao (buscar-dados-acao codigo)]
                                                                             (if (and dados-acao (not (:erro dados-acao)) (> quantidade 0))
                                                                               (if-let [preco-atual (:ultimo-preco dados-acao)]
                                                                                 (calcular-valor-total-acao quantidade preco-atual)
                                                                                 0)
                                                                               0)))
                                                                         saldos))]
                               (println (format "\n=== Valor Total da Carteira: R$ %.2f ===" valor-total-carteira))))
                           (println "\nNenhuma acao na carteira."))))
                     true))

   (= opcao "0") (do
                   (println "\nSaindo...")
                   nil)

   :else (do 
           true))

(defn executar-menu
  "Executa o menu usando recursao"
  ([]
   (executar-menu true))
  ([continuar]
   (if continuar
     (do
       (exibir-menu)
       (flush)
       (let [opcao (read-line)
             continuar-novo (processar-opcao opcao)]
         (recur continuar-novo)))
     nil)))

(defn -main
  "Funcao principal do programa"
  [& _]
  (println "Bem-vindo ao Gerenciador de Carteira de Acoes!")
  (println "Certifique-se de que a API esta rodando em" api-local-url)
  (executar-menu))
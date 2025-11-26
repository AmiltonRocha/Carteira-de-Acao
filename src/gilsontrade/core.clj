;; Nesta pagina e a onde colocamos o codigo principal do projeto
(ns gilsontrade.core
  (:require [clj-http.client :as http-client] ;; biblioteca para fazer requisicoes HTTP
            [cheshire.core :as json]) ;; biblioteca para parsear JSON
  (:gen-class))

(def api-local-url "http://localhost:3000") ;; URL da API local

(defn buscar-dados-acao
  "Consulta os dados de uma acao na API local"
  [codigo]
  (let [url (str api-local-url "/acao/" codigo)
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
  [codigo quantidade]
  (try
    (let [dados-acao (buscar-dados-acao codigo)]
      (if (and dados-acao (not (:erro dados-acao)))
        (if-let [preco-atual (:ultimo-preco dados-acao)]
          (let [url (str api-local-url "/compra")
                dados-json (json/generate-string {:codigo codigo
                                                  :quantidade quantidade
                                                  :preco preco-atual})
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
                          (println "Ultimo Preco: R$" (:ultimo-preco dados-validos))
                          (println "Preco Maximo do Dia: R$" (:preco-maximo-do-dia dados-validos))
                          (println "Preco Minimo do Dia: R$" (:preco-minimo-do-dia dados-validos))
                          (println "Preco de Abertura: R$" (:preco-de-abertura dados-validos))
                          (println "Preco de Fechamento: R$" (:preco-de-fechamento dados-validos))
                          (println "Hora:" (:hora dados-validos)))
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
                              quantidade (Double/parseDouble quantidade-str)
                              resultado (registrar-compra codigo quantidade)]
                          (if-let [erro (:erro resultado)]
                            (println "\nErro:" erro)
                            (do
                              (println "\nCompra registrada com sucesso!")
                              (println "Codigo:" (:codigo resultado))
                              (println "Quantidade:" (:quantidade resultado))
                              (println "Preco Unitario: R$" (:preco-unitario resultado))
                              (println "Valor Total: R$" (:valor-total resultado))
                              (println "Data:" (:data resultado)))))
                        (catch NumberFormatException _
                          (println "\nErro: Quantidade invalida. Digite um numero valido."))
                        (catch Exception ex
                          (println "\nErro inesperado:" (.getMessage ex))))
                      true))
    (= opcao "0") (do
                    (println "\nSaindo...")
                    nil)
    :else (do
            (println "\nOpcao invalida! Tente novamente.")
            true)))

(defn executar-menu
  "Executa o menu usando "
  ([]
   (executar-menu true))  ; <- inicia com true para começar o loop
  ([continuar]
   (if continuar
     (do
       (exibir-menu)
       (flush)
       (let [opcao (read-line)
             continuar-novo (processar-opcao opcao)]
         (recur continuar-novo)))  ; <- recur volta para o inicio da funcao com novo valor
     nil)))  ; <- retorna nil quando nao continuar

(defn -main
  "Funcao principal do programa"
  [& _]
  (println "Bem-vindo ao Gerenciador de Carteira de Acoes!")
  (println "Certifique-se de que a API esta rodando em" api-local-url)
  (executar-menu)) 











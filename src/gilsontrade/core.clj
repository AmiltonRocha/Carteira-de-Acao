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











# Instala o pacote 'dados', que contém diversos conjuntos de dados úteis para análise
install.packages("dados")

# Instala o pacote 'corrplot', usado para gerar gráficos de correlação
install.packages("corrplot")

# Instala o pacote 'report', que facilita a criação de relatórios de modelos estatísticos
install.packages("report")

# Instala o pacote skimr se ainda não estiver instalado
install.packages("skimr")

# Instala o pacote GGally, necessário para a função ggpairs
install.packages("GGally")

# Carrega o pacote 'dados' para utilizar os conjuntos de dados contidos nele (ex: pinguins)
library("dados")

# Carrega o pacote 'corrplot' para gerar gráficos de correlação entre variáveis
library(corrplot)

# Anexa o conjunto de dados 'pinguins' ao espaço de nomes, permitindo o acesso direto às variáveis do dataset
attach(pinguins)

# Carrega o pacote 'report'
library(report)

# Carrega o pacote skimr para uso da função skim
library(skimr)

# Carrega o pacote dplyr para manipulação de dados
library(dplyr)

# Carrega o pacote GGally para criar gráficos de pares
library(GGally)

# Cria uma função de paleta de cores que faz a transição entre as cores 'white', 'lightblue' e 'darkblue'
paleta_azul <- colorRampPalette(c("white", "lightblue", "darkblue"))

# Gera um vetor de 10 cores usando a função 'paleta_azul', criando uma transição suave de 'white' a 'darkblue'
cores_azuis <- paleta_azul(10)

# Remove todas as linhas que têm pelo menos um valor ausente
pinguins_limpos <- na.omit(pinguins)

# Gera um resumo estatístico básico do dataset
summary(pinguins_limpos)

# Exibe a estrutura e as primeiras observações do dataset 'pinguins'
glimpse(pinguins)

# Exibe a estrutura e as primeiras observações do dataset 'pinguins_limpos'
glimpse(pinguins_limpos)

# Gera um resumo detalhado do dataset 'pinguins_limpos'
skim(pinguins_limpos)

#PLOT DA CORRELAÇÃO ENTRE TODAS AS VARIÁVEIS
plot(pinguins_limpos , pch=10 , cex=1.5 , col="darkblue")

#PLOT DA CORRELAÇÃO ENTRE AS VARIAVEIS EXPLICATIVAS
# Seleciona as variáveis explicativas e a resposta
variaveis_explicativas <- pinguins_limpos[, c("comprimento_bico", "comprimento_nadadeira", "massa_corporal")]

# Adiciona a variável resposta à seleção
dados <- cbind(variaveis_explicativas, profundidade_bico = pinguins_limpos$profundidade_bico)

# Gera o gráfico de correlação (scatterplot matrix)
pairs(dados, pch = 16, cex = 1.5, col = "darkblue", 
      main = "Correlação entre Profundidade do Bico e Variáveis Explicativas")

# CORRELAÇÃO ENTRE PROFUNDIDADE DO BICO E COMPRIMENTO DO BICO
plot(profundidade_bico, comprimento_bico, col="darkblue")

# CORRELAÇÃO ENTRE PROFUNDIDADE DO BICO E COMPRIMENTO DA NADADEIRA
plot(profundidade_bico, comprimento_nadadeira, col="darkblue")

# CORRELAÇÃO ENTRE PROFUNDIDADE DO BICO E COMPRIMENTO DA NADADEIRA
plot(profundidade_bico, massa_corporal, col="darkblue")

#TABELA COM MATRIZ DE CORRELAÇÃO
tab_correlacao = cor(pinguins_limpos[, c("profundidade_bico", "comprimento_bico", "comprimento_nadadeira", "massa_corporal", "ano")])
tab_correlacao

#MATRIZ DE CORRELAÇÃO GRÁFICA
corrplot(tab_correlacao,method = "number",col = cores_azuis)

# Cria uma matriz de gráficos de pares (ggpairs) para as colunas 3 a 6, colorindo os pontos pela espécie
graf1 <- ggpairs(pinguins_limpos, columns = 3:6, ggplot2::aes(colour=especie))

# Exibe o gráfico de pares gerado
graf1

#MODELO 1 - Ajuste do modelo de regressão linear múltipla com a profundidade do bico como variável resposta
modelo_1 = lm(profundidade_bico~ . -especie -ilha -sexo -ano, data = pinguins_limpos)
summary(modelo_1)

#MODELO 2 - Sem massa_corporal
modelo_2 <- update(modelo_1, ~ . -massa_corporal)
summary(modelo_2)

#MODELO 3 - Sem comprimento da nadadeira
modelo_3 = update(modelo_1, ~ . -comprimento_nadadeira)
summary(modelo_3)

#AIC - MODELO 2
paste("AIC do modelo 2: ", AIC(modelo_2))
#AIC - MODELO 3
paste("AIC do modelo 3: ", AIC(modelo_3))
#BIC - MODELO 2
paste("BIC do modelo 2: ", BIC(modelo_2))
#BIC - MODELO 3
paste("BIC do modelo 3: ", BIC(modelo_3))

# Gera gráficos de diagnóstico do modelo 2
plot(modelo_2)

# Gera relatório detalhado do modelo 2
report(modelo_2)

#PREVISOES
# Previsões com intervalo de confiança usando novas variáveis preditoras
novas.preditoras <- data.frame(comprimento_bico = c(45, 50), comprimento_nadadeira = c(190, 210))
previsoes <- predict(modelo_2, newdata = novas.preditoras, interval = "confidence")
previsoes

#Previsões com intervalo de predição usando novas variáveis preditoras
novas.preditoras <- data.frame(comprimento_bico = c(45, 50), comprimento_nadadeira = c(190, 210))
previsoes <- predict(modelo_2, newdata = novas.preditoras, interval = "prediction")
previsoes




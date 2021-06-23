library(tidyverse)
library(AER)
library(wooldridge)
library(plm)
library(magrittr)
library(readxl)
library(lmtest)
library(sandwich)

#1
dados1 <- tibble(
  Y = c(10, 11.6, 8, 20.1, 9),
  X2 = c(15, 20, 18, 35, 22),
  X3 = c(20, 30, 25, 40, 35),
  Z2 = c(20, 25, 23, 37, 18))

#1A
lm(Y ~ X2 + X3, dados1) %>%
  summary()

#1B
ivreg(Y ~ X2 + X3 | Z2 + X3, data = dados1) %>%
  summary()

#1C
dados1 %$%
cor(X2, Z2)
# Z2 não é um intrumento fraco. Um instrumento fraco tem pouca correlação com 
# variável que instrumenta, o que faz com que ele cause pouca "variação  exógena",
# causada pelo intrumento. Isso faz com que vieses que ele possa ter "explodam
# em valor. Nesse caso, a estimativa trazida pelo intrumento poderia ser de pouca
# utilidade.

#1D
lm(Y ~ X2 + X3, dados1) %>%
  summary()[["r.squared"]]

ivreg(Y ~ X2 + X3 | Z2 + X3, data = dados1) %>%
  summary()[["r.squared"]]

#1E
# A situação do problema coloca o modelo de variavel instrumental como ideal.
# Temos, por hipótese, correlação entre X2 e o não-observado, o que torna o OLS 
# viesado, e temos Z2 independente do não-observado e com correlação alta com X2,
# a primeira necessária para Z2 ser um bom estimador, e a segunda tornando ele 
# mais confiável

#2
data("mathpnl")

#2A
pdata.frame(mathpnl, index = 3850) %>%
  lm(math4 ~ y94 + y95 + y96 + y97 + y98 + log(rexpp) + 
        log(enrol) + lunch, data = ., model = "pooling") %>%
  coeftest(vcov. = vcovHC)

#2B
# O sinal é o que eu esperava, pois indica que a cada 1% de alunos que qualificam 
# para o programa de refeições gratuitas, 0.4% a menos de alunos passam em 
# matemática, o que é em linha com o fato de que alunos mais ricos vão melhor na
# escola. A variavel "lunch", portanto, é uma proxy para a pobreza no distrito,
# que impacta as aprovações.

#2C
pdata.frame(mathpnl, index = 3850) %>%
  plm(math4 ~ y94 + y95 + y96 + y97 + y98 + log(rexpp) + 
        log(enrol) + lunch, data = ., model = "pooling")

#3
data("bwght")

#3A
# Esse modelo superestimaria o valor de beta populacional, pois o consumo de 
# cigarros provavelmente está correlacionado com outros habitos que podem fazer 
# mal para o desenvolvimento do bebê.

#3B
# Embora a taxa sobre cigarros aparente ser um bom intrumento para esse modelo, 
# a justificativa dada é incompleta, pois é necessário dizer também que a taxa 
# não é correlacionada com fatores não-observados. Além disso, há o risco de que 
# o instrumento seja fraco, o que pode criar um viés significativo, ou uma grande
# variância

#3C

bwght %>%
  summarise(
    first_stage = coef(lm(packs ~ cigtax))[2],
    reduced_form = coef(lm(bwght ~ cigtax))[2],
    IV_estimate = reduced_form/first_stage)

#4
data4 <- pdata.frame(read_xlsx("dados questão 4.xlsx"))

#4A
data4 %>%
  plm(indiceremun ~ desemp, data = ., model = "pooling") %>%
  coef()[2]

#4B
data4 %>%
  lm(indiceremun ~ desemp + factor(year), data = .) %>%
  coef()[2]

#4C
# O uso de efeitos fixos para tempo se justifica pelo fato de podem ter 
# características específicas de um ano, como uma recessão, que poderiam viesar 
# a estimativa ( por exemplo diminuindo o emprego especialmente entre os de 
# menor remuneração), assim como poderiam haver tendências temporais, que também
# poderiam trazer vieses

#5
data5 <- read_xlsx("dadosquestao5.xlsx")

#5A
data5%>%
  m

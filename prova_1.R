library(tidyverse)
library(sandwich)
library(lmtest)
library(haven)
#1
Y<-matrix(c(1.5, 6.5, 10, 11, 11.5, 16.5), ncol=1)
X<-matrix(c(1,0,0,1,1,2,1,1,4,1,2,2,1,2,4,1,3,6), ncol=3, byrow = TRUE)
#A
Xt<-t(X)
XtX<-Xt%*%X
XtX_inv<-solve(XtX)
XtX_inv
#B
XtY<-Xt%*%Y
B_hat<-XtX_inv%*%XtY
#Y= 2 + 3X_1 + 1X_2 +u
#C
k<-nrow(B_hat)
n<-nrow(Y)
u_hat<-Y-X%*%B_hat
u_hat_t<-t(u_hat)
SSR<-u_hat_t%*%u_hat
sigma2_hat<-as.numeric(SSR/(n-k))
mavcov<-sigma2_hat*XtX_inv
ep<-sqrt(diag(mavcov))
gl<-(n-k)
sig<-qt(0.025, gl, lower.tail = F)
beta2t<-B_hat[2]/ep[2]
#hipótese nula foi rejeitada, uma vez que beta2/var(beta2) é maior que a 
#estatistica T que é o criterio
#D
IC<-c(B_hat[3]+ep[3]*sig, B_hat[3]-ep[3]*sig)
#o zero está contido no intervalo de confiança, o que faz co que nã rejeitemos a
#hipótese nula em 95%. Aumenta o intervalo de confiança ter muita variação dos
#resíduos, e diminui ter muita variação em X ou ter muitas observações, tudo mais constante
#E
Ym <- Y - mean(Y)
Ytm <- t(Ym)
SST <- Ytm%*%Ym
R2<-1-SSR/SST
#Praticamente toda a variação é explicada pela regressão, e portanto podemos 
#prever muito bem o valor de Y com os observaveis X

#2
install.packages("wooldridge")
library(wooldridge)
data("wagepan")
#A
wage_reg1<-lm(lwage~married + educ+ hours, wagepan)
summary(wage_reg1)
#B
wage_reg2<-lm(lwage~married + factor(year)*educ + hours, wagepan)
summary(wage_reg2)
#C
coeftest(wage_reg2, vcov.=vcovHC)
#Erros padrões robustos são adequados para inferencia em mais casos, tendo 
#flexibilidade para lidar com casos de heteroscedasticidade

#3
expec<-tibble(
  paciente=1:15,
  Y1=c(9, 10, 9, 3, 9, 2, 7, 9, 6, 3, 6, 11, 2, 12, 4),
  Y0=c(6, 5, 2, 3, 2, 1, 5, 6, 2, 9, 1, 4, 2, 7, 0))
#A
ATE<-mean(expec$Y1)-mean(expec$Y0)
#B
tratamento<-subset(expec, paciente%%2==0)
ATT<-mean(tratamento$Y1)- mean(tratamento$Y0)
#C
controle<-subset(expec, paciente%%2==1)
ATU<-mean(controle$Y1)- mean(controle$Y0)
#D
obs<-mean(tratamento$Y1) - mean(controle$Y0)
#podemos concluir que o tratamento é benéfico, dado que ele traria beneficios 
#para a população em geral (ATE). Além disso, seu efeito também seria visto 
#nas observações efetivas

#4
read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

titanic <- read_data("titanic.dta")%>%
  mutate(d = case_when(class == 1 ~ 1, TRUE ~ 0))
#A
VIP<-subset(titanic, d==1)
normal<-subset(titanic, d==0)
SDO<-mean(VIP$survived)-mean(normal$survived)
#B
#não há razão para crer que gere, uma vez que o os nossos dados podem ser viesados
#por haver mais mulheres ou criançasentre os VIPs, e estes seriam já privilegiados.
#Teriamos, assim, três efeitos misturados: o efeito que queremos estimar, o efeito
#de ser criança e o efeito de ser mulher
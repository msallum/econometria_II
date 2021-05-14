library(tidyverse)

#geração dos dados
B<-matrix(runif(10, max=10),ncol = 1)
X<-matrix(c(rep(1, 100),rnorm(900)), ncol=10)
Y<-X%*%B + matrix(rnorm(100), ncol=1)

#estimação dos Betas
Xt<-t(X)
XtX<-Xt%*%X
XtX_inv<-solve(XtX)
XtY<-Xt%*%Y
B_hat<-XtX_inv%*%XtY

#estimando variancia
k<-nrow(B)
n<-nrow(Y)
u_hat<-Y-X%*%B_hat
u_hat_t<-t(u_hat)
SSR<-u_hat_t%*%u_hat
sigma_hat<-as.numeric(SSR/(n-k))

#estimando variancia dos Betas
var_cov<-sigma_hat*XtX_inv

#R^2
Yt<-t(Y)
SST<-Yt%*%Y
r2<-1-SSR/SST

#estimando variancia robusta dos Betas
S<-diag(c(u_hat))
S2<-S%*%S
var_cov_rob<-(n/n-k)*XtX_inv%*%(Xt%*%S2%*%X)%*%XtX_inv

#IC 95%
i<-1
sig<-qt(0.025, n-k, lower.tail = TRUE)
top<-B_hat[i]+var_cov_rob[i,i]*sig
low<-B_hat[i]-var_cov_rob[i,i]*sig

#Potencial outcomes
pessoas<-tibble(
  Y1 = sample(5:20, 15),
  Y0 = sample(0:15, 15),
  D = as.numeric(runif(15)>.5)
)
ATE<-pessoas%>%
  mutate(TE=
           Y1-Y0)%>%
  summarise(mean(TE))
AT_<-pessoas%>%
  mutate(
    TE=Y1-Y0)%>%
  group_by(D)%>%
  summarise(efeito=mean(TE))

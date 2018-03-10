####################

####### CPP ########

####################

 

######################

## Importando dados ##

 

path = "C:\\Users\\marci\\Desktop\\R\\" # Atenção ao uso de "\\" ou "/"

setwd(path)

 

require(readxl) # função para carregar o pacote, também é possível usar "library"

dados = read_excel("Matriz 1.xlsx", sheet = 1, col_names = FALSE)

as.matrix(dados)

 

#############

## 1 ETAPA ##

#############

 

# Aleatorização com Distr Normal

 

sd1 = sd(dados[,1])

sd2 = sd(dados[,2])

 

 

#############

## 2 ETAPA ##

#############

 

## Critério 1 ##

################

 

## Alternativa 1

 

fun1 = function(x) dnorm(x,dados[1,1],sd1)*pnorm(x,dados[2,1],sd1)*pnorm(x,dados[3,1],sd1)

PMax.Alt.11 = integrate(fun1,-Inf,Inf)$value

 

fun2 = function(x) dnorm(x,dados[1,1],sd1)*(1-pnorm(x,dados[2,1],sd1))*(1-pnorm(x,dados[3,1],sd1))

PMin.Alt.11 = integrate(fun2,-Inf,Inf)$value

 

## Alternativa 2

 

fun1 = function(x) dnorm(x,dados[2,1],sd1)*pnorm(x,dados[1,1],sd1)*pnorm(x,dados[3,1],sd1)

PMax.Alt.21 = integrate(fun1,-Inf,Inf)$value

 

fun2 = function(x) dnorm(x,dados[2,1],sd1)*(1-pnorm(x,dados[1,1],sd1))*(1-pnorm(x,dados[3,1],sd1))

PMin.Alt.21 = integrate(fun2,-Inf,Inf)$value

 

## Alternativa 3

 

fun1 = function(x) dnorm(x,dados[3,1],sd1)*pnorm(x,dados[1,1],sd1)*pnorm(x,dados[2,1],sd1)

PMax.Alt.31 = integrate(fun1,-Inf,Inf)$value

 

fun2 = function(x) dnorm(x,dados[3,1],sd1)*(1-pnorm(x,dados[1,1],sd1))*(1-pnorm(x,dados[2,1],sd1))

PMin.Alt.31 = integrate(fun2,-Inf,Inf)$value

 

 

## Critério 2 ##

################

 

## Alternativa 1

 

fun1 = function(x) dnorm(x,dados[1,2],sd2)*pnorm(x,dados[2,2],sd2)*pnorm(x,dados[3,2],sd2)

PMax.Alt.12 = integrate(fun1,-Inf,Inf)$value

 

fun2 = function(x) dnorm(x,dados[1,2],sd2)*(1-pnorm(x,dados[2,2],sd2))*(1-pnorm(x,dados[3,2],sd2))

PMin.Alt.12 = integrate(fun2,-Inf,Inf)$value

 

## Alternativa 2

 

fun1 = function(x) dnorm(x,dados[2,2],sd2)*pnorm(x,dados[1,2],sd2)*pnorm(x,dados[3,2],sd2)

PMax.Alt.22 = integrate(fun1,-Inf,Inf)$value

 

fun2 = function(x) dnorm(x,dados[2,2],sd2)*(1-pnorm(x,dados[1,2],sd2))*(1-pnorm(x,dados[3,2],sd2))

PMin.Alt.22 = integrate(fun2,-Inf,Inf)$value

 

## Alternativa 3

 

fun1 = function(x) dnorm(x,dados[3,2],sd2)*pnorm(x,dados[1,2],sd2)*pnorm(x,dados[2,2],sd2)

PMax.Alt.32 = integrate(fun1,-Inf,Inf)$value

 

fun2 = function(x) dnorm(x,dados[3,2],sd2)*(1-pnorm(x,dados[1,2],sd2))*(1-pnorm(x,dados[2,2],sd2))

PMin.Alt.32 = integrate(fun2,-Inf,Inf)$value

 

 

#############

## 3 ETAPA ##

#############

 

## Ponto de Vista PP

 

PP.A1 = PMax.Alt.11 * PMax.Alt.12

PP.A2 = PMax.Alt.21 * PMax.Alt.22

PP.A3 = PMax.Alt.31 * PMax.Alt.32

 

PP = rbind (PP.A1,PP.A2,PP.A3)

rownames(PP) = c("Alt 1","Alt 2","Alt 3")

PP = cbind (PP,rank(-PP))

colnames(PP) = c("PP","Rank")

PP

 

## Ponto de Vista PO

 

PO.A1 = 1-((1-PMax.Alt.11) * (1-PMax.Alt.12))

PO.A2 = 1-((1-PMax.Alt.21) * (1-PMax.Alt.22))

PO.A3 = 1-((1-PMax.Alt.31) * (1-PMax.Alt.32))

 

PO = rbind (PO.A1,PO.A2,PO.A3)

rownames(PO) = c("Alt 1","Alt 2","Alt 3")

PO = cbind (PO,rank(-PO))

colnames(PO) = c("PO","Rank")

PO

 

## Ponto de Vista CP

 

CP.A1 = (1-PMin.Alt.11) * (1-PMin.Alt.12)

CP.A2 = (1-PMin.Alt.21) * (1-PMin.Alt.22)

CP.A3 = (1-PMin.Alt.31) * (1-PMin.Alt.32)

 

CP = rbind (CP.A1,CP.A2,CP.A3)

rownames(CP) = c("Alt 1","Alt 2","Alt 3")

CP = cbind (CP,rank(-CP))

colnames(CP) = c("CP","Rank")

CP

 

## Ponto de Vista CO

 

CO.A1 = 1-(PMin.Alt.11 * PMin.Alt.12)

CO.A2 = 1-(PMin.Alt.21 * PMin.Alt.22)

CO.A3 = 1-(PMin.Alt.31 * PMin.Alt.32)

 

CO = rbind (CO.A1,CO.A2,CO.A3)

rownames(CO) = c("Alt 1","Alt 2","Alt 3")

CO = cbind (CO,rank(-CO))

colnames(CO) = c("CO","Rank")

CO

 

 

#################

## RESULTADOS ###

#################

 

cbind(PP,PO,CP,CO)

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 
####################

####### CPP ########

####################

 

######################

## Importando dados ##

 

path = "C:\\Users\\marci\\Desktop\\R\\" # Atenção ao uso de "\\" ou "/"

setwd(path)

 

require(readxl) # função para carregar o pacote, também é possível usar "library"

dados = read_excel("Matriz 1.xlsx", sheet = 1, col_names = FALSE)

as.matrix(dados)

 

#############

## 1 ETAPA ##

#############

 

# Aleatorização com Distr Normal

 

sd1 = sd(dados[,1])

sd2 = sd(dados[,2])

 

 

#############

## 2 ETAPA ##

#############

 

## Critério 1 ##

################

 

## Alternativa 1

 

fun1 = function(x) dnorm(x,dados[1,1],sd1)*pnorm(x,dados[2,1],sd1)*pnorm(x,dados[3,1],sd1)

PMax.Alt.11 = integrate(fun1,-Inf,Inf)$value

 

fun2 = function(x) dnorm(x,dados[1,1],sd1)*(1-pnorm(x,dados[2,1],sd1))*(1-pnorm(x,dados[3,1],sd1))

PMin.Alt.11 = integrate(fun2,-Inf,Inf)$value

 

## Alternativa 2

 

fun1 = function(x) dnorm(x,dados[2,1],sd1)*pnorm(x,dados[1,1],sd1)*pnorm(x,dados[3,1],sd1)

PMax.Alt.21 = integrate(fun1,-Inf,Inf)$value

 

fun2 = function(x) dnorm(x,dados[2,1],sd1)*(1-pnorm(x,dados[1,1],sd1))*(1-pnorm(x,dados[3,1],sd1))

PMin.Alt.21 = integrate(fun2,-Inf,Inf)$value

 

## Alternativa 3

 

fun1 = function(x) dnorm(x,dados[3,1],sd1)*pnorm(x,dados[1,1],sd1)*pnorm(x,dados[2,1],sd1)

PMax.Alt.31 = integrate(fun1,-Inf,Inf)$value

 

fun2 = function(x) dnorm(x,dados[3,1],sd1)*(1-pnorm(x,dados[1,1],sd1))*(1-pnorm(x,dados[2,1],sd1))

PMin.Alt.31 = integrate(fun2,-Inf,Inf)$value

 

 

## Critério 2 ##

################

 

## Alternativa 1

 

fun1 = function(x) dnorm(x,dados[1,2],sd2)*pnorm(x,dados[2,2],sd2)*pnorm(x,dados[3,2],sd2)

PMax.Alt.12 = integrate(fun1,-Inf,Inf)$value

 

fun2 = function(x) dnorm(x,dados[1,2],sd2)*(1-pnorm(x,dados[2,2],sd2))*(1-pnorm(x,dados[3,2],sd2))

PMin.Alt.12 = integrate(fun2,-Inf,Inf)$value

 

## Alternativa 2

 

fun1 = function(x) dnorm(x,dados[2,2],sd2)*pnorm(x,dados[1,2],sd2)*pnorm(x,dados[3,2],sd2)

PMax.Alt.22 = integrate(fun1,-Inf,Inf)$value

 

fun2 = function(x) dnorm(x,dados[2,2],sd2)*(1-pnorm(x,dados[1,2],sd2))*(1-pnorm(x,dados[3,2],sd2))

PMin.Alt.22 = integrate(fun2,-Inf,Inf)$value

 

## Alternativa 3

 

fun1 = function(x) dnorm(x,dados[3,2],sd2)*pnorm(x,dados[1,2],sd2)*pnorm(x,dados[2,2],sd2)

PMax.Alt.32 = integrate(fun1,-Inf,Inf)$value

 

fun2 = function(x) dnorm(x,dados[3,2],sd2)*(1-pnorm(x,dados[1,2],sd2))*(1-pnorm(x,dados[2,2],sd2))

PMin.Alt.32 = integrate(fun2,-Inf,Inf)$value

 

 

#############

## 3 ETAPA ##

#############

 

## Ponto de Vista PP

 

PP.A1 = PMax.Alt.11 * PMax.Alt.12

PP.A2 = PMax.Alt.21 * PMax.Alt.22

PP.A3 = PMax.Alt.31 * PMax.Alt.32

 

PP = rbind (PP.A1,PP.A2,PP.A3)

rownames(PP) = c("Alt 1","Alt 2","Alt 3")

PP = cbind (PP,rank(-PP))

colnames(PP) = c("PP","Rank")

PP

 

## Ponto de Vista PO

 

PO.A1 = 1-((1-PMax.Alt.11) * (1-PMax.Alt.12))

PO.A2 = 1-((1-PMax.Alt.21) * (1-PMax.Alt.22))

PO.A3 = 1-((1-PMax.Alt.31) * (1-PMax.Alt.32))

 

PO = rbind (PO.A1,PO.A2,PO.A3)

rownames(PO) = c("Alt 1","Alt 2","Alt 3")

PO = cbind (PO,rank(-PO))

colnames(PO) = c("PO","Rank")

PO

 

## Ponto de Vista CP

 

CP.A1 = (1-PMin.Alt.11) * (1-PMin.Alt.12)

CP.A2 = (1-PMin.Alt.21) * (1-PMin.Alt.22)

CP.A3 = (1-PMin.Alt.31) * (1-PMin.Alt.32)

 

CP = rbind (CP.A1,CP.A2,CP.A3)

rownames(CP) = c("Alt 1","Alt 2","Alt 3")

CP = cbind (CP,rank(-CP))

colnames(CP) = c("CP","Rank")

CP

 

## Ponto de Vista CO

 

CO.A1 = 1-(PMin.Alt.11 * PMin.Alt.12)

CO.A2 = 1-(PMin.Alt.21 * PMin.Alt.22)

CO.A3 = 1-(PMin.Alt.31 * PMin.Alt.32)

 

CO = rbind (CO.A1,CO.A2,CO.A3)

rownames(CO) = c("Alt 1","Alt 2","Alt 3")

CO = cbind (CO,rank(-CO))

colnames(CO) = c("CO","Rank")

CO

 

 

#################

## RESULTADOS ###

#################

 

cbind(PP,PO,CP,CO)

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 

 


 

 

 

 

 

 


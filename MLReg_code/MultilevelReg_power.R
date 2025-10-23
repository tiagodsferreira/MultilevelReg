# title: "Modelos de Regressão Multinível: Laboratório"
# author: "Joana Cadima & Tiago Ferreira"
# date: "Maio, 2021"


## Estudo pilot ou literatura existente
## Neste caso, para simplificar, estimamos uma modelo apenas com random-intercept

Model.1 <- lmer(Happiness ~ GPA.C + (1 | SchoolID), data=SchoolsData)
summary(Model.1)
library(effectsize)

icc(Model.1)

b <- fixef(Model.1)
v1 <- VarCorr(Model.1)
s <- sigma(Model.1)

## Sintaxe para cálculo do poder para modelos multinível

##########################
#criar base de dados
##########################
# 5 clusters com 20 observações individuais cada

set.seed(2021)
x1 <- 1:20
cluster <- 1:5
design <- expand.grid(GPA.C=x1, SchoolID=cluster)
nrow(design)
simulated.df <- cbind(design, Happinessy=rnorm(nrow(design)))

#########################
#Definir parametros alvo do modelo a testar
#########################
# Usar o a função makeLmer do pacote simr para ajustar o valor dos parâmetros a estimar no modelo
model.of.interest <- makeLmer(Happiness ~ GPA.C + (1|SchoolID), fixef=b,
                       VarCorr=v1, sigma=s, data=simulated.df)
model.of.interest
summary(model.of.interest)
icc(model.of.interest)
standardize_parameters(model.of.interest) 

library(ggplot2)
library(GGally)
GGally::ggpairs(simulated.df)

# usamos as funções do pacote simr para alterar os parâmetros de acordo com o queremos
library(simr)
help(package="simr")
fixef(model.of.interest)["GPA.C"] <-.15 # efeito fixo a detetar
summary(model.of.interest) 
#simulamos agora o número de vezes que este efeito é detetado numa amostra com n=100
powerSim(model.of.interest, nsim=20) 


#########################
# Usar powerSim para simular o modelo:
# Criar base de dados usando os parâmetros do modelo (i.e. simula amostra aleatórias de y de acordo com o modelo definido).
# Corre novamente o modelo na amostra simulada, repetindo o processo várias (>1000) vezes
# Conta o número de vezes (i.e., proporção) que é atingido o valor p de corte  
#########################


fixef(model.of.interest)["x"] <-.3
summary(model.of.interest)
powerSim(model.of.interest, nsim=20)
?powerSim

# Mudar o parametro g para simular efeito do grupo. VarCorr() permite retirar os parâmetros de variância do modelo
library(performance)
summary(model.of.interest)
VarCorr(model.of.interest)['SchoolID'] <- .3 
summary(model.of.interest)
icc(model.of.interest)
?icc
#Aumentando o ICC o poder diminui

powerSim(model.of.interest, nsim=20)
pc1 <- powerCurve(model.of.interest, nsim=10, breaks=c(2,4,8,12,16,20))
summary(pc1) 
ls(pc1)
plot(pc1, xlab="Level 1 Sample Size")

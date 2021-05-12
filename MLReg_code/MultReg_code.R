# title: "Modelos de Regressão Multinível: Laboratório"
# author: "Joana Cadima & Tiago Ferreira"
# date: "Maio, 2021"


library(pacman)
p_load(datasets, lattice, psych, ggplot2, knitr, png, kableExtra, GGally, tidySEM, corrplot, effectsize, lmerTest, MuMIn)

###########################################
# Importação e análise exploratória dos dados  
###########################################
SchoolsData <- read.csv("G:\\My Drive\\FPCEUP\\R trainning\\GitRepo\\Multilevel Regression\\MLReg_data\\SchoolsData.csv")

##  Estrutura geral Base de Dados (BD)  
str(SchoolsData)

## Fatorização de variáveis 
SchoolsData$SchoolContext <- as.factor(SchoolsData$SchoolContext) # transformar a variável "SchoolContext" num fator
sapply(SchoolsData, class) # verificar a classe de todas as variáveis de SchoolsData
table(SchoolsData$SchoolContext)

## Estatística descritiva  
library(psych)
psych::describe(SchoolsData[, -5:-7])
psych::describeBy(SchoolsData[, -5:-6], group="SchoolContext")

## Exploração gráfica dos dados  
### Relação entre *Happiness* (VD) e *Friends* (VI)  

db_sel_plot <- SchoolsData[SchoolsData$SchoolID %in% c(sample(1:10, 3), sample(11:20, 3)),]

Model.Plot.Friends <-ggplot(data = db_sel_plot, aes(x = Friends, y=Happiness, group=SchoolID))+   
  facet_grid( ~ SchoolID)+    
  geom_point(aes(colour = SchoolID))+ 
  geom_smooth(method = "lm", se = TRUE, aes(colour = SchoolID))+  
  xlab("Friends")+ylab("Happiness")+    
  theme(legend.position = "none")   
Model.Plot.Friends # Efeito parece variar em função da escola. Negativo para elevado SES e negativo para baixo SES*
  
  
### Relação entre *Happiness* (VD) e *Quality* (VI)  
Model.Plot.Quality <-ggplot(data = db_sel_plot, aes(x =Quality, y=Happiness,group=SchoolID))+    
  facet_grid( ~ SchoolID)+    
  geom_point(aes(colour = SchoolID))+ 
  geom_smooth(method = "lm", se = TRUE, aes(colour = SchoolID))+  
  xlab("School Quality")+ylab("Happiness")+    
  theme(legend.position = "none")   
Model.Plot.Quality # O efeito positivo parece mais consistente nas escolas de baixo SES*  
  
## Relação entre *GPA* (VI) e *Happiness* (VD)  
Model.Plot.GPA <-ggplot(data = db_sel_plot, aes(x =GPA, y=Happiness,group=SchoolID))+    
  facet_grid( ~ SchoolID)+    
  geom_point(aes(colour = SchoolID))+ 
  geom_smooth(method = "lm", se = TRUE, aes(colour = SchoolID))+  
  xlab("GPA")+ylab("Happiness")+    
  theme(legend.position = "none")   
Model.Plot.GPA # Efeito mais evidente em escolas de elevado SES*  
  
## Correlações para a amostra completa     
corr.student  <-  cor(SchoolsData[,1:4])
corrplot::corrplot(corr.student, method = "number", diag = FALSE, type = "lower")

## Correlações para escolas de elevado SES
SchoolsData_rich <- subset(SchoolsData, SchoolContext=="rich neighborhood")
corr.student_rich  <-  cor(SchoolsData_rich[,1:4])
corrplot(corr.student_rich, method = "number", diag = FALSE, type = "lower")

## Correlações para escolas de baixo SES    
{r, out.width = "600px"}
SchoolsData_poor <- subset(SchoolsData, SchoolContext=="poor neighborhood")
corr.student_poor  <-  cor(SchoolsData_poor [,1:4])
corrplot(corr.student_poor, method = "number", diag = FALSE, type = "lower") # CONCLUSÃO: Efeitos variáveis em função do tipo de escola**
  
###########################################  
# GLM e Regressão linear Simples    
###########################################
## Centrar as variáveis à “grand mean” para facilitar a interpretação dos dados  
SchoolsData$Friends.C <- scale(SchoolsData$Friends, scale = FALSE)[,] 
# equal to SchoolsData$Friends.C <- SchoolsData$Friends - mean(SchoolsData$Friends)
SchoolsData$Quality.C <- scale(SchoolsData$Quality, scale = FALSE)[,] 
SchoolsData$GPA.C <- scale(SchoolsData$GPA, scale = FALSE)[,] 

## Estimação OLS no R s  
Reg.Model <- lm(Happiness ~ Friends.C + Quality.C + GPA.C, data = SchoolsData)
summary(Reg.Model)$coefficients
 

Para a amostra total obtemos efeitos significativos de:   
  - Friends.C (*b* = 0.14, *p* < .001)    
- Quality.C (*b* = 0.42, *p* < .001)  
- GPA.C (*b* = -0.12, *p* < .001)   

*Será que obtemos os mesmos resultados em todas as escolas?*

layout(matrix(c(1,2,3,4),2,2))
plot(Reg.Model) # podemos avaliar visualmente a qualidade do modelo de regressão


## Estimação OLS do modelo para escolas de elevado SES  
Reg.Model_rich <- lm(Happiness ~ Friends.C + Quality.C + GPA.C, 
                     data = subset(SchoolsData, SchoolContext=="rich neighborhood"))

summary(Reg.Model_rich)$coefficients

## Estimação OLS do modelo para escolas de baixo SES 
Reg.Model_poor <- lm(Happiness ~ Friends.C + Quality.C + GPA.C, 
                     data = subset(SchoolsData, SchoolContext=="poor neighborhood"))
summary(Reg.Model_poor)$coefficients
  
## Estimação OLS - GPA e Happiness
ggplot(SchoolsData, aes(x=GPA.C, y=Happiness)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

## Estimação OLS - GPA e Happiness
ggplot(SchoolsData, aes(x=GPA.C, y=Happiness, shape=SchoolContext, color=SchoolContext)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)


###########################################  
# Regressão multinível  
###########################################  
  
## Cálculo do ICC no R  
library(lme4) # se necessário: install.packages("lme4")
head(SchoolsData, 3)

Null <- lmer(Happiness ~ 1 # define “Happinesss” em função do intercept (fixed effect)
             + (1|SchoolID), # e que cada escola tem o seu intercept (random effect)
             data=SchoolsData) # base de dados
summary(Null)
vc <- VarCorr(Null) 
print(vc, comp=c("Variance", "Std.Dev"))
vc <- as.data.frame(vc)[,c("grp", "vcov")]
vc[1,2]
vc[,2]
(ICC <- vc[1,2]/sum(vc[,2]))
  

ICC_user <- function(nome.modelo) {
  vc <- VarCorr(nome.modelo) 
  vc <- print(vc, comp=c("Variance", "Std.Dev"))
  vc <- as.data.frame(vc)[,c("grp", "vcov")]
  ICC <- vc[1,2]/sum(vc[,2])
  return(c(ICC = ICC))
}

ICC_user(Null)
  
library(performance)
icc(Null)
  
## Random Intercept Model com preditor de nível 1  
Model.1 <- lmer(Happiness ~ GPA.C + (1 | SchoolID), data=SchoolsData)
      
summary(Model.1)
summary(Model.1)$coefficients
summary(Model.1)$varcor
    
anova(Null, Model.1)
      
 # library(effectsize)
standardize(Model.1) # pré
standardize_parameters(Model.1) # pós
    
    
    
## R2
# install.packages("MuMIn")
# library(MuMIn)
r.squaredGLMM(Model.1, null=Null) # NOTA: R2m consiste na variância explicada apenas pelos fixed effects, enquanto que o indicador R2c representa a variância explicada pela totalidade do modelo, considerando tanto os fixed como os random effects*
      
## Os valores de *p* (significância)  
# library(lmerTest)
summary(Model.1, ddf = "lme4")
summary(Model.1, ddf = "Satterthwaite")
summary(Model.1, ddf = "Kenward-Roger")
    
## Random Intercept Model com preditores (L1 e L2)  
head(SchoolsData,3)
AveQual <- with(SchoolsData, tapply(Quality, SchoolID, mean))
SchoolsData$SchoolAveQuality <- AveQual[SchoolsData$SchoolID]
    
Model.2 <- lmer(Happiness ~ GPA.C + Friends.C + Quality.C + SchoolAveQuality + (1 | SchoolID), data=SchoolsData)
summary(Model.2, ddf = "Satterthwaite") 
summary(Model.2, ddf = "Satterthwaite")$coefficients
r.squaredGLMM(Model.2, null=Null)
r.squaredGLMM(Model.1, null=Null)
anova(Null, Model.1, Model.2)
    
    
## Random Intercept and slope Model  
Model.3 <- lmer(Happiness ~ GPA.C + Friends.C + Quality.C + 
                  (1 + Friends.C | SchoolID), data = SchoolsData)
summary(Model.3)
summary(Model.3)$coefficients
summary(Model.3)$varcor


# Podemos acrescentar ao modelo os random effects para as restantes variáveis de nível 1.   
Model.4 <- lmer(Happiness ~  GPA.C + Quality.C + Friends.C + 
                  (1 + GPA.C + Quality.C + Friends.C | SchoolID), data = SchoolsData)

summary(Model.4)

# A função ranova() pode-nos ajudar a melhorar o modelo  
# library(lmerTest)
ranova(Model.4) 
  
# Testamos o novo modelo agora sem os random effects para GPA e Quality.   
Model.5 <- lmer(Happiness ~ GPA.C + Quality.C + Friends.C + 
                  (1 + Friends.C | SchoolID), data = SchoolsData)
summary(Model.5)


#######################################
# Conclusão  
#######################################
## OLS Vs. Multinível  
Model2_SR <- lm(Happiness ~ Friends.C, data = SchoolsData)
summary(Model2_SR)$coefficients
# Pelo método clássico verificamos um efeito significativo de "Friends" sobre *Happiness* (*b* = 0.15, *p* < .001). 

Model.8 <- lmer(Happiness ~ Friends.C + 
                  (1 + Friends.C | SchoolID), data = SchoolsData)
summary(Model.8)$coefficients
# No entanto, se considerarmos no modelo o fato de estarmos perante uma estrutura aninhada dos dados, o mesmo efeito deixa de ser significativo (*p* = .116)


Model.8 <- lmer(Happiness ~ Friends.C + SchoolContext + Friends.C*SchoolContext +
                  (1 | SchoolID), data = SchoolsData)
summary(Model.8)

library(interactions)
probe_interaction(Model.8, Friends.C, SchoolContext)$interactplot
  

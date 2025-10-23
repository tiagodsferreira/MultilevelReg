library(lme4)
library(effects)
library(corrplot)
library(stargazer)
library(car)
library(MASS)
library(ggplot2)
library(texreg)


######################################################################
# Criar uma base de dados com estrutura multi-nível
######################################################################

set.seed(42) # para permitir reproduzir a mesma sequência de números aleatórios
nrich <- 40 # número de sujeitos  por escola ricas 
npoor <- 160 # número de sujeitos  por escola pobres
# Parâmetros desejados
S.F.Rich <- -1 # parâmetros para número de amigos em escolas ricas
S.F.Poor <- 5 # parâmetros para número de amigos em escolas pobres

S.Q.Rich <- 1 # parâmetros para qualidade da escolas percecionada em escolas ricas
S.Q.Poor <- 7 # parâmetros para qualidade da escolas percecionada em escolas pobres

S.G.Rich <- 3 # parâmetros para GPA em escolas ricas
S.G.Poor <- .7 # parâmetros para GPA em escolas ricas

# Escolas ricas
# Escola 1
X1 <- rnorm(nrich, 10, 2) # número de amigos observados
Z1 <- runif(nrich, 1.0, 4.0) # GPA observado
H1 <- runif(nrich, 1.0, 7.0) # Qualidade da escola observada
Y1 <- S.F.Rich*X1 + S.G.Rich*Z1 + S.Q.Rich*H1 + 80 + rnorm(nrich, sd= 5) # Equação para definir valores observados de Happiness

# Escola 2
X2 <- rnorm(nrich, 10, 2) # número de amigos observados
Z2 <- runif(nrich, 1.0, 4.0) # GPA observado
H2 <- runif(nrich, 1.0, 7.0) # Qualidade da escola observada
Y2 <- S.F.Rich*X2 + S.G.Rich*Z2 + S.Q.Rich*H2 + 75 + rnorm(nrich, sd= 5) # Equação para definir valores observados de Happiness

# Escola 3
X3 <- rnorm(nrich, 10, 2) # número de amigos observados
Z3 <- runif(nrich, 1.0, 4.0) # GPA observado
H3 <- runif(nrich, 1.0, 7.0) # Qualidade da escola observada
Y3 <- S.F.Rich*X3 + S.G.Rich*Z3 + S.Q.Rich*H3 + 90 + rnorm(nrich, sd= 5) # Equação para definir valores observados de Happiness

# Escolas pobres
# Escola 4
X4 <- rnorm(npoor, 5, 2) # número de amigos observados
Z4 <- runif(npoor, 1.0, 4.0) # GPA observado
H4 <- runif(nrich, 1.0, 7.0) # Qualidade da escola observada
Y4 <- S.F.Poor*X4 + S.G.Poor*Z4 + S.Q.Poor*H4 + 35 + rnorm(npoor, sd = 10) # Equação para definir valores observados de Happiness

# Escola 5
X5 <- rnorm(npoor, 5, 2) # número de amigos observados
Z5 <- runif(npoor, 1.0, 4.0) # GPA observado
H5 <- runif(nrich, 1.0, 7.0) # Qualidade da escola observada
Y5 <- S.F.Poor*X5 + S.G.Poor*Z5 + S.Q.Poor*H5 + 40 + rnorm(npoor, sd = 10) # Equação para definir valores observados de Happiness

# Escola 6
X6 <- rnorm(npoor, 5, 2) # número de amigos observados
Z6 <- runif(npoor, 1.0, 4.0) # GPA observado
H6 <- runif(nrich, 1.0, 7.0) # Qualidade da escola observada
Y6 <- S.F.Poor*X6 + S.G.Poor*Z6 + S.Q.Poor*H6 + 50 + rnorm(npoor, sd = 10) # Equação para definir valores observados de Happiness

# As 3 escolas ricas:
Student.Data.School.1<-data.frame(Happiness=Y1, Friends=X1, Quality=H1, GPA=Z1)
Student.Data.School.2<-data.frame(Happiness=Y2, Friends=X2, Quality=H2, GPA=Z2)
Student.Data.School.3<-data.frame(Happiness=Y3, Friends=X3, Quality=H3, GPA=Z3)

# As 3 escolas pobres:
Student.Data.School.4<-data.frame(Happiness=Y4, Friends=X4, Quality=H4, GPA=Z4)
Student.Data.School.5<-data.frame(Happiness=Y5, Friends=X5, Quality=H5, GPA=Z5)
Student.Data.School.6<-data.frame(Happiness=Y6, Friends=X6, Quality=H6, GPA=Z6)

# Correlações within-group - escola 1
corr.student  <-  cor(Student.Data.School.1)
corrplot(corr.student, method = "number",diag = FALSE,type = "lower")

# Correlações within-group - escola 4
corr.student  <-  cor(Student.Data.School.4)
corrplot(corr.student, method = "number",diag = FALSE,type = "lower")

# Construir a base de dados final com os dados de todos as crianças de todas as escolas

All.Schools.Data <- rbind(Student.Data.School.1, Student.Data.School.2, Student.Data.School.3, Student.Data.School.4, Student.Data.School.5, Student.Data.School.6) 
head(All.Schools.Data)

# Acrescentar uma variável identificadora do sujeito (StudentID)
All.Schools.Data$StudentID<-seq(1:nrow(All.Schools.Data))
head(All.Schools.Data)

# Acrescentar uma variável identificadora da escola (SchoolID)
All.Schools.Data$SchoolID<-c(rep(1, nrich), rep(2,nrich), rep(3, nrich), rep(4, npoor), 
                             rep(5, npoor), rep(6, npoor))

# Criar um fator (SchoolContext) que identifique escolas nos dois tipos de contextos
All.Schools.Data$SchoolContext[All.Schools.Data$SchoolID>3] <- "poor neighborhood"
All.Schools.Data$SchoolContext[All.Schools.Data$SchoolID<4] <- "rich neighborhood"
All.Schools.Data$SchoolContext <- as.factor(All.Schools.Data$SchoolContext)

# Guarda base de dados em ficheiro csv (SchoolsData.csv)
write.csv(All.Schools.Data, "C:\\Users\\tiago\\Google Drive\\FPCEUP\\R trainning\\PDPsi\\PDPsi_Multilevel\\DATA_Multilevel\\SchoolsData.csv", row.names=FALSE)


######################################################################
# 1. Importar e Explorar a base de dados
######################################################################
library(data.table)  # se necessário: install.packages("data.table")
SchoolsData <- fread("C:\\Users\\tiago\\Google Drive\\FPCEUP\\R trainning\\PDPsi\\PDPsi_Multilevel\\DATA_Multilevel\\SchoolsData.csv")

str(SchoolsData)
names(SchoolsData)
summary(SchoolsData)
SchoolsData$SchoolContext <- as.factor(SchoolsData$SchoolContext) # transformar a variável "SchoolContext" num fator

sapply(SchoolsData, class)

# Começamos por explorar graficamente a base de dados. As escolas 1, 2 e 3 estão situadas em contextos economicamente mais favorecidos, enquanto as escolas 4, 5 e 6 em contexto menos favorecidos. Usamos para o efeito o pacote ggplot2.

library(ggplot2) # se necessário install.packages("ggplot2")
table(SchoolsData$SchoolID, SchoolsData$SchoolContext) 

# Primeiro criamos um gráfico para verificar a relação entre os scores de felicidade do alunos ("Happiness" - VD) e número de amigos ("Friends", VI).

theme_set(theme_bw(base_size = 12, base_family = "")) # este código não é essencial e apenas serve para definir um tema comum aos gráficos criados posteriormente

Model.Plot.Friends <-ggplot(data = SchoolsData, aes(x = Friends, y=Happiness, group=SchoolID))+   
  facet_grid( ~ SchoolID)+    
  geom_point(aes(colour = SchoolID))+ 
  geom_smooth(method = "lm", se = TRUE, aes(colour = SchoolID))+  
  xlab("Friends")+ylab("Happiness")+    
  theme(legend.position = "none")   
Model.Plot.Friends 

# Tal como esperado, os efeitos entre as duas variáveis "Happiness" e "Friends" parecem variar de acordo com as escolas onde são avaliados. Visualizamos de seguida a relação entre qualidade da escola percecionada por cada um dos alunos ("Quality" - VI) e scores de felicidade ("Happiness" – VD).

Model.Plot.Quality <-ggplot(data = SchoolsData, aes(x =Quality, y=Happiness,group=SchoolID))+    
  facet_grid( ~ SchoolID)+    
  geom_point(aes(colour = SchoolID))+ 
  geom_smooth(method = "lm", se = TRUE, aes(colour = SchoolID))+  
  xlab("School Quality")+ylab("Happiness")+    
  theme(legend.position = "none")   
Model.Plot.Quality 

# Através deste gráfico percebemos que, embora pareça existe um efeito positivo entre as duas variáveis, esses efeitos parecem mais evidentes nas escolas inseridas em contextos desfavorecidos (escolas nr. 4, 5 e 6).

# Finalmente, verificamos a relação entre o desempenho académico ("GPA" - VI)  e scores de felicidade ("Happiness" – VD).

Model.Plot.GPA <-ggplot(data = SchoolsData, aes(x =GPA, y=Happiness,group=SchoolID))+    
  facet_grid( ~ SchoolID)+    
  geom_point(aes(colour = SchoolID))+ 
  geom_smooth(method = "lm", se = TRUE, aes(colour = SchoolID))+  
  xlab("GPA")+ylab("Happiness")+    
  theme(legend.position = "none")   
Model.Plot.GPA  

# Aqui, a relação (slope) entre as duas variáveis assemelhante nos dois tipos de escolas, embora pareçam existir diferenças nos intercepts dessas slopes (i.e., níveis médios de felicidade quando o valor de GPA é 0).  
# Analisamos agora as correlações entre as diferentes variáveis para a amostra completa, incluindo crianças em escolas em contexto favorecidos e desfavorecidos
names(SchoolsData)
corr.student  <-  cor(SchoolsData[,1:4])
corr.student
library(corrplot) # se necessário install.pachages("corrplot")
corrplot(corr.student, method = "number", diag = FALSE, type = "lower")
?corrplot

# Analisamos agora as mesmas correlações, separadamente para os dois tipos de escolas na amostra.  
SchoolsData_rich <- subset(SchoolsData, SchoolContext=="rich neighborhood")
corr.student_rich  <-  cor(SchoolsData_rich[,1:4])
corrplot(corr.student_rich, method = "number", diag = FALSE, type = "lower")
# escolas em contextos desfavorecidos
SchoolsData_poor <- subset(SchoolsData, SchoolContext=="poor neighborhood")
corr.student_poor  <-  cor(SchoolsData_poor [,1:4])
corrplot(corr.student_poor, method = "number", diag = FALSE, type = "lower")

# É de notar que o padrão de correlações parece mudar entre os dois grupos de escolas, sendo diferente do padrão de correlações observado para a amostra completa. Estes resultados sugerem a presença de relações diferentes entre variáveis observadas nas 6 escola.

######################################################################
# 2. Regressão simples
######################################################################
# De seguida, testamos um modelo de regressão simples. Esta abordagem analítica não considera o facto dos dados estarem organizados de forma hierárquica, isto é não tem em conta que os estudantes na amostra partilharem as mesmas escolas. Para facilitar a interpretação dos dados, vamos começar por centrar as variáveis à “grand mean”. Para isso, subtraímos a média de uma determinada variável ao valores individuais observados para cada um dos sujeitos nessa mesma variável. Conseguimos facilmente fazer isto através da função "scale()", com o argumento scale = FALSE.
# ?scale
SchoolsData$Friends.C <- scale(SchoolsData$Friends, scale = FALSE)[,] 

# Na prática, o resultado é obtido através do seguinte código:
# SchoolsData$Friends.C <- SchoolsData$Friends - mean(SchoolsData$Friends)

SchoolsData$Quality.C <- scale(SchoolsData$Quality, scale = FALSE)[,] 
SchoolsData$GPA.C <- scale(SchoolsData$GPA, scale = FALSE)[,] 

# Testamos agora o nosso modelo de regressão simples usando as variáveis centradas à média o que implica que ao valor observado de zero corresponde a média da amostra.

Reg.Model <- lm(Happiness ~ Friends.C + Quality.C + GPA.C, data = SchoolsData)
plot(Reg.Model) # podemos avaliar visualmente a qualidade do modelo de regressão
summary(Reg.Model) 

# Considerando a amostra na sua totalidade, o sumário do modelo de regressão simples indica a presença de efeitos significativos do número de amigos (b = 0.60, p < .01), qualidade da escola (b = 5.52, p < .01), e GPA (b = 1.61, p < .05) sobre a nossa variável dependente ("Happiness"). Mas será que este padrão de resultados repete-se quando analisamos os dados separadamente para as escolas nos diferentes tipos de contextos (i.e., contextos favorecidos e desfavorecidos)?

# Regressão simples para escolas favorecidas
# a função subset () permite selecionar os casos pretendidos, neste caso apenas as crianças em escoals favorecidas serão consideradas (?subset)
Reg.Model_rich <- lm(Happiness ~ Friends.C + Quality.C + GPA.C, 
                     data = subset(SchoolsData, SchoolContext=="rich neighborhood"))
summary(Reg.Model_rich)

# Verificamos que dois dos três efeitos deixam de ser significativos restando apenas o efeito significativo de "GPA" sobre "Happiness". De notar que o efeito do número de amigos no nível de felicidade muda até de sinal, passando a ser negativo (embora não significativo).

# Regressão simples para escolas desfavorecidas
Reg.Model_poor <- lm(Happiness ~ Friends.C + Quality.C + GPA.C, 
                     data = subset(SchoolsData, SchoolContext=="poor neighborhood"))
summary(Reg.Model_poor)

# No caso das crianças em contexto escolares desfavorecidos, o efeito do GPA no nível de felicidade não é significativo, mantendo-se os efeitos significativos do número de amigos e qualidade da escola sobre a variável dependente (i.e., happiness).

# Em síntese, os resultados anteriores não parecem refletir de forma exata o que se passa em cada uma das escolas. A regressão simples, aplicada à totalidade da amostra, indica que todas as variáveis independentes (VI) têm um efeito significativo sobre a variável dependente (VD) quando isso não é verdade para os dois grupos de escolas. Trata-se de um exemplo em que os resultados da regressão simples, por não considerar as estrutura aninhada ou hierárquica dos dados, pode conduzir a conclusões erradas acerca das relação entre variáveis.  
# Salientamos que este exemplo é meramente académico. Porque os dados foram simulados, sabíamos previamente que o padrão de associações entre variáveis estaria dependente do contexto económico da escola. Na realidade, o investigador poderá não ter acesso a essa informação quanto está a realizar as suas análises. Caso essa informação esteja disponível poderá facilmente considerá-la no modelo como um predito de nível 2 (i.e., escola). É ainda importante considerar que este exemplo apenas usa 6 escolas e que em situações "normais" o número de escolas é substancialmente maior não sendo realista tratar o contexto económico como um fator fixo, controlado ao nível do design do estudo.
# Uma forma alternativa de abordar este tipo de problemas metodológicos é através da implementação de modelos de regressão multi-nível. Este tipo de modelos permite aos investigadores distinguirem entre efeitos dentro ("within-group") e entre grupos ("between-group"), enquanto os modelos simples de regressão combinam este dois tipos de efeitos num coeficiente único (fixo) de regressão.

######################################################################
# 3. Regressão multi-nível
######################################################################

######################################################################
# 3.1. Modelo Nulo e ICC
######################################################################

# Quando resolvemos testar um modelo multi-nível é geralmente boa ideia perceber primeiro se, face aos nosso dados, será realmente necessário ter este tipo de abordagem analítica. Para percebermos essa necessidade, devemos testar o modelo multi-nível mais simples, chamado frequentemente de modelo nulo, e que avalia o efeito médio do contexto sobre a nossa VD, neste caso "Happiness". O modelo nulo não tem VI's, sendo apenas útil para obter uma estimativa das variâncias residuais e dos intercepts, quando apenas o efeito da escola é considerado. O modelo é representado pela seguinte equação:
# yij = β00 + U0j + εij
# Fixed effects:
# β0 - intercept do modelo
# Random effects:
# U0j - variância do grupo para o intercept
# εij - variância individual

# Para analisar modelos multi-nível no R recorremos ao pacote lme4.
library(lme4) # se necessário: install.packages("lme4")
head(SchoolsData)
?lmer
Null <- lmer(Happiness ~ 1 # define “Happinesss” em função do intercept (fixed effect)
             + (1|SchoolID), # e que cada escola tem o seu intercept (random effect)
             data=SchoolsData) # base de dados
# a formula do modelo especifica a variável dependente antes do operador "~" e os preditores após esse mesmo operador. A parte dos efeitos fixos (fixed-effects) surge logo depois de "~". O 1 representa o intercept. A parte aleatória do modelo (random-effects) surge depois, entre parêntesis. O modelo dos efeitos aleatórios é definida antes do operador "|". A variável que representa o cluster ou grupo (nível 2) na base de dados surge depois de "|". O argumento “data” permite identificar a base de dados que pretendemos utilizar na nossa análise.

summary(Null)
# O sumário do modelo nulo não é muito informativo, no entanto permite clarificar as duas componentes de variância, entre clusters (τ2) e indivíduos (σ2). Estes valores surgem no modelo na parte dedicada ao random effects. Neste caso τ2 = 94.7, enquanto que σ2 = 267.3. Podemos usar estes valores para calcular o Inter-Class Correlation (ICC), de forma a clarificar a necessidade de uma abordagem analítica multi-nível. O ICC permite avaliar o efeito do cluster nos nossos dados de Happiness, sendo uma medida da quantidade de variância da VD explicada pelo nível 2 do nosso modelo (escola). Podemos também interpretar o ICC como a correlação média nos scores de happiness observada entre estudantes da mesma escola. Um valor de ICC diferente de 0 justifica a adoção de modelos multi-nível.  
# Podemos calcular o ICC da seguinte forma:
# A função VarCorr() permite extrair as componentes de variância estimadas pelo modelo 
?VarCorr 
vc <- VarCorr(Null) 
# Utilizamos a função print() para visualizar essas componentes de variância, no entanto, por defeito, esta função apenas extrai os valores de desvio padrão
print(vc) 
# Assim, devemos especificar através do argumento "comp", que queremos também que o R nos mostre as variâncias 
print(vc, comp=c("Variance", "Std.Dev"))
# Criamos agora um objeto de dados apenas com as variâncias para o nosso modelo
vc <- print(vc, comp=c("Variance", "Std.Dev"))
vc <- as.data.frame(vc)[,c("grp", "vcov")]

# Com estes dados, podemos agora calcular facilmente o ICC = var. group / var. total, sendo que a var. total = var. grupo + var. total. 
vc[1,2]
vc[,2]
ICC <- vc[1,2]/sum(vc[,2])
ICC # Assim, verificamos que o cluster explica cerca de 20% da variância de “happiness”, o que justifica uma abordagem multi-nível

# Podemos embrulhar todas estas linhas de código numa função de forma a simplificar a nossa vida quando, no futuro, precisarmos novamente de calcular o ICC
ICC_user <- function(nome.modelo) {
  vc <- VarCorr(nome.modelo) 
  vc <- print(vc, comp=c("Variance", "Std.Dev"))
  vc <- as.data.frame(vc)[,c("grp", "vcov")]
  ICC <- vc[1,2]/sum(vc[,2])
  return(c(ICC = ICC))
}
ICC_user(Null) # Assim, apenas temos que especificar o nome do modelo nulo como argumento e a função ICC_user faz o resto

# Para simplificar ainda um pouco mais a nossa vida, podemos usar uma função pré-existente, embora isso obrigue à instalação de um novo pacote no R. Neste caso, usamos a função icc() do pacote performance
# se necesssário, install.packages("performance")
library(performance)
icc(Null)
?icc

######################################################################
# 3.2. Random Intercept Model com preditor de nível 1 
######################################################################
# Este modelo consiste numa extensão muito simples do modelo anterior, incluindo apenas um preditor de nível 1 (i.e., estudante). Assim, neste modelo consideramos o efeito do aproveitamento académico dos aluno medido através do GPA (fixed effect) sobre o score de felicidade. Como no modelo anterior, considera-se o efeito específico da escola sobre o intercept (random effect). Este modelo pode ser representado pela seguinte equação:
# yij = β0 + β1(GPA)ij + U0j + εij
# Fixed effects:
# β0 - intercept do modelo
# β1(GPA)ij - Efeito médio de GPA sobre happiness
# Random effects:
# U0j - variância do grupo
# εij - variáncia individual 

Model.1 <- lmer(Happiness ~ GPA.C + (1 | SchoolID), data=SchoolsData)
# A sintaxe do modelo, essencialmente, especifica que o score de "Happiness" é predito pela variável "GPA" (fixed effect). A sintaxe dentro de parêntesis serve para definir a parte aleatória do modelo (random effect) e a variável do cluster. Neste exemplo, "(1 | SchoolID)" indica que apenas iremos considerar a variação do intercept (random intercept model) dentro das diferentes escolas.

summary(Model.1)
# O sumário do modelo dá-nos os efeitos fixos e os desvios padrão para os efeitos aleatórios. Os resultados sugerem que o GPA dos alunos tem um efeito positivo (t > 1.9) no seu nível de felicidade, quando controlando as flutuações média dos scores de felicidade nas diferentes escolas. Assim, a um aumento de GPA de 1 ponto está associado um aumento médio de 1.79 pontos de felicidade.
# Depois de considerar o efeito de GPA em hapiness, o desvio padrão nos intercepts entre escolas (between-group) é de 9.732 (muito próximo do valor correspondente no modelo nulo), enquanto que o desvio padrão dos scores de hapiness dos individuos dentro das escolas (within-group) é de 16.284. O intercept fixo, isto é o intercept "médio" é de 90.18, representando a média de hapiness quando o GPA é 0. De notar que GPA está centrado à média, o que significa que o valor 0 corresponde ao valor médio da amostra nesta variável. Podemos ainda comparar o ajustamento do modelo 1 relativamente ao modelo nulo, apenas com o intercept aleatório.  

anova(Null, Model.1)

# Os valores de AIC e BIC são ligeiramente inferiores no Model.1, quando comparados com o modelo nulo, o que sugere um melhor ajustamento aos dados do Model.1. Podemos também calcular o ICC para o modelo. Verifica-se de forma a verificar quantidade de váriância de "hapiness" está presente ao nível do grupo (i.e., escola)
ICC_user(Model.1)

# Finalmente, calculamos a variância explicada (r2) pelo modelo 1 através da função r.squaredGLMM() do pacote MuMIn.
# install.packages("MuMIn")
library(MuMIn)
?r.squaredGLMM
r.squaredGLMM(Model.1, null=Null)
# R2m consiste na variância explicada apenas pelos fixed effects, enquanto que o indicador R2c representa a variância explicada pela totalidade do modelo, considerando tanto os fixed como os random effects.

######################################################################
# 3.3. Os valores de p (significância)
######################################################################
# Existe uma grande discussão relativamente à forma mais adequada de calcular os valore de p em modelos multi-nível. Diferentes programas estatísticos adotam  diferentes métodos para este cálculo. Por exemplo, no HLM os valores de p são calculados com recurso à distribuição t. Já no SAS e SPSS é utilizada a aproximação de Satterthwaite. Existe ainda a aproximação Kenward-Roger também disponível em algumas aplicações, nomeadamente no R. Estes métodos produzem regra geral resultados muito aproximados.

# Será necessário recorrer ao pacote lmerTest para obtermos os valores de p para o nosso modelo. Este pacote permite, através do argumento ddf, utilizar todos os diferentes métodos referidos anteriormente para calcular os valores de p.

# se necessário install.packages("lmerTest")
library(lmerTest)
# ?lmerTest::lmer
summary(Model.1, ddf = "lme4")
summary(Model.1, ddf = "Satterthwaite")
summary(Model.1, ddf = "Kenward-Roger")


######################################################################
# 3.4. Random Intercept Model com preditores de nível 1 e 2
######################################################################
# Apresentamos de seguida um modelo com fixed effects de variáveis no nível 1 (aluno) e 2 (estudante). Começamos por criar e incluir na nossa base de dados um preditor de nível 2 ("SchoolAveQuality"), que consistirá na qualidade média da escola percecionada pelos seus alunos.
head(SchoolsData)
AveQual <- with(SchoolsData, tapply(Quality, SchoolID, mean))
AveQual
SchoolsData$SchoolAveQuality[SchoolsData$SchoolID==1] <- AveQual[1]
SchoolsData$SchoolAveQuality[SchoolsData$SchoolID==2] <- AveQual[2]
SchoolsData$SchoolAveQuality[SchoolsData$SchoolID==3] <- AveQual[3]
SchoolsData$SchoolAveQuality[SchoolsData$SchoolID==4] <- AveQual[4]
SchoolsData$SchoolAveQuality[SchoolsData$SchoolID==5] <- AveQual[5]
SchoolsData$SchoolAveQuality[SchoolsData$SchoolID==6] <- AveQual[6]

# Assim, o modelo seguinte incluirá três preditores de nível 1, GPA, número de amigos e qualidade percecionada individualmente pelos alunos, e um preditor de nível 2, qualidade média da escola. Ainda serão considerados apenas os fixed effects destes preditores.

Model.2 <- lmer(Happiness ~ GPA.C + Friends.C + Quality.C + SchoolAveQuality + (1 | SchoolID), data=SchoolsData)

# Tal como os preditores de nível 1, o preditor de nível 2, "SchoolAveQuality" é incluído como fixed effect, não sendo incluída qualquer componente aleatória (random effect). Apresenta por isso um efeito fixo (médio) que é comum a todas as escolas. 

summary(Model.2, ddf = "Satterthwaite") 

# Verifica-se pelo sumário do modelo que todas as variáveis de nível 1 apresentam efeitos significativos sobre “Happiness”. Pelo contrário a qualidade média percecionada pelos alunos de cada uma das escolas (preditor de nível 2) não tem um efeito significativo sobre a VD.

# ?r.squaredGLMM 
r.squaredGLMM(Model.2, null=Null) # o modelo explica uma quantidade impressionante de variância muito superior à variância explica pelo modelo 1, apenas com 1 preditor
r.squaredGLMM(Model.1, null=Null)

# Usamos a função anova para comparar o ajustamento dos modelos testados até agora

anova(Null, Model.1, Model.2) # o modelo 2 destaca-se, representando os dados de forma mais correta.


############################################################################
# 3.5. Random Intercept Model com fatores de interação simples e multinível 
############################################################################
# Interação simples
Model.3 <- lmer(Happiness ~ GPA.C + Friends.C + Quality.C + Friends.C*Quality.C + SchoolAveQuality + 
                  (1 | SchoolID), data=SchoolsData)

# Neste modelo incluímos a interação entre preditores de nível 1 (i.e., indivíduo). Esta interação é definida na sintaxe do modelo, incluindo as duas variáveis separadas pela operador "*".  

summary(Model.3, ddf = "Satterthwaite") 

# Pelo sumário do modelo verificamos que "GPA", "Friends" e "Quality" apresentam-se como preditores significativos dos scores de "happiness". A interação entre "Friends" e "Quality" tem um efeito negativo (b=-.69, p>.001). Significa que o aumento da qualidade da escola percecionada individualmente está associada ao um enfraquecimento da relação entre o número de amigos e a felicidade.

# No modelo, seguinte testamos a interação entre preditores de nível 1  e 2 
Model.4 <- lmer(Happiness ~ GPA.C + Friends.C + Quality.C + SchoolAveQuality + 
                  Friends.C*SchoolAveQuality + (1 | SchoolID), data=SchoolsData)
summary(Model.4, ddf = "Satterthwaite")

# Neste caso, os resultados do fator de interação entre qualidade global da escola e número de amigos sugere que, quando a qualidade média da escola aumenta, o efeito entre número de amigos e felicidade torna-se maior (neste caso menos negativo).

############################################################################
# 3.6. Random Intercept and slope Model
############################################################################
# O modelo seguinte, para além do intercept, especifica o efeito do número de amigos na felicidade como random effect, preconizando, assim, que esta relação possa mudar de escola para escola. Mantemos GPA e Quality como fixed effects.

Model.5 <- lmer(Happiness ~ GPA.C + Friends.C + Quality.C + 
                  (1 + Friends.C | SchoolID), data = SchoolsData)
# cada escola tem o seu intercept e o efeito do número de amigos pode variar em função da escola.
summary(Model.5)

# Os resultados permitem descrever as estimativas para a variância do intercept (random), variância da slope (random), bem como as correlações entre estas random intercept e slope. Os resultados sugerem que o número de amigos não tem um efeito significativo sobre a felicidade quando controlando o efeito de escola no nível 2. Isto é não existe um efeito significativo entre “Friends” e “Happiness” que se possa ser generalizado aos diferentes grupos (i.e., escolas).
# Podemos acrescentar ao modelo os random effects para as restantes variáveis de nível 1.  

Model.6 <- lmer(Happiness ~  GPA.C + Quality.C + Friends.C + 
                  (1 + GPA.C + Quality.C + Friends.C | SchoolID), data = SchoolsData)

# Agora, cada uma das escolas tem associado um intercept único bem como as slopes para “GPA”, “Quality” e “Friends”.  

summary(Model.6)

# A função ranova() pode ajudar a melhorar o modelo, uma vez que permite verificar o efeito da retirada de cada um dos random effects do modelo  

ranova(Model.6)  

# Os resultados indicam que a eliminação do random effect de GPA não se traduz em alterações significativas do modelo. Isto sugere que o efeito fixo de GPA sobre a felicidade é semelhante entre as várias escolas da amostra. Testamos o novo modelo agora sem este random effect (GPA).  

Model.7 <- lmer(Happiness ~ GPA.C + Quality.C + Friends.C + 
                  (1 + Quality.C + Friends.C | SchoolID), data = SchoolsData)
summary(Model.7)

############################################################################
# 4. Conclusão
############################################################################
# Os resultados anteriores salientam a importância de uma abordagem multi-nível quando lidamos com dados ordenados de forma hierárquica. Isto fica claro se compararmos os resultados anteriores com os resultados que obteríamos com o uso de modelos simples de regressão.  

Model2_SR <- lm(Happiness ~ Friends.C, data = SchoolsData)
summary(Model2_SR)

# Os resultados da estimação do modelo de regressão simples anterior revelam um efeito significativo do número de amigos sobre os scores de felicidade (b = 0.53, p < .05). 

Model.8 <- lmer(Happiness ~ Friends.C + 
                  (1 + Friends.C | SchoolID), data = SchoolsData)
summary(Model.8) 

# No entanto, se considerarmos no modelo o fato de estarmos perante uma estrutura aninhada dos dados, podemos verificamos que o efeito do número de amigos sobre a felicidade não é significativo (p=.211), apesar de a estimativa até ser superior (b=1.894). Estes resultados ilustram a importância de considerar os diferentes níveis dos dados (i.e., individual e grupo), de forma a podermos tirar conclusões mais adequadas para a população. Assim, ao não considerar o nível escola, podíamos assumir que existe um efeito do número de amigos na felicidade para todas as crianças da nossa amostra, quando, na verdade, esse efeito apenas é significativo para crianças em algumas das escolas da amostra, como demonstram os resultados seguintes.

Model2_SR_rich <- lm(Happiness ~ Friends.C, data = subset(SchoolsData, SchoolContext=="rich neighborhood"))
summary(Model2_SR_rich)
Model2_SR_poor <- lm(Happiness ~ Friends.C, data = subset(SchoolsData, SchoolContext=="poor neighborhood"))
summary(Model2_SR_poor)

# Assim, o efeito positivo do número de amigos sobre o nível de felicidade apenas é significativo em escolas situadas em contexto desfavorecidos. Em síntese, os resultados da regressão multi-nível traduzem-se numa estimativa mais correta, indicando que não existe um efeito médio significativo do número de amigos sobre os scores de felicidade que é comum a todas as crianças, independentemente do contexto económico das escolas que frequentam.




##########
#exercício
##########
https://www.rensvandeschoot.com/tutorials/lme4/
  
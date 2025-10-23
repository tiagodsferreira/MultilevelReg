
# **EXERCÍCIO 1**  
#   1. Iniciar o R e verificar o local ou diretório de trabalho. Colocar nesse local o ficheiro de dados "dogs.csv";  

getwd()


# 2. Importar a base de dados dogs, atribuindo essa estrutura de dados ao valor DOGS. Ver datalhes da base de dados em: https://data.world/len/intelligence-of-dogs;    

DOGS <- read.csv("G:\\My Drive\\FPCEUP\\R trainning\\GitRepo\\R crash course\\R_crash_course\\R crash course-Data\\dogs.csv")
# pode usar a choose.files() para selecionar mais facilmente o ficheiro


# 3. Verificar classe, estrutura, nomes das variáveis e primeiras linhas/observações de GOT;    

class(DOGS)
str(DOGS)
names(DOGS)
head(DOGS)


# 4. Visualizar lista das raças na base de dados;  

DOGS$Breed 


# 5. Fazer sumário das variáveis em DOGS;   

summary(DOGS)


# 6. Criar e visualizar o objeto "dif_reps" definido pela diferença entre variáveis "reps_upper" e "reps_lower";  

dif_reps <- DOGS$reps_upper - DOGS$reps_lower
dif_reps 


# 7. Utilizar a função plot() para visualizar a variáveis "Classification". Use as.factor() se necessário;  

plot(as.factor(DOGS$Classification))


# 8. Fazer um histograma para a variável "obey"";  

hist(DOGS$obey)


# 9. Instalar (se necessário) e carregar o pacote "psych";  

install.packages("psych")
library(psych)


# 10. Usar as funcionalidades deste pacote para calcular estatística descritiva básica e correlações para as variáveis "obey", "reps_lower" e "reps_upper";  

psych::describe(DOGS[3:5])
corr.test(DOGS[3:5])


# 11. Criar um boxplot que represente o nível de obediência por cada classificação de raça;  

plot(obey ~ as.factor(Classification), data=DOGS)


# 12. Criar um modelo para verificar a existência de um efeito da classificação no nível de obediência. Clarificar que pares de classificações apresentam diferenças significativas em termos do seu nível de obediência;

model1 <- aov(obey ~ Classification, data=DOGS)  
summary(model1) 
TukeyHSD(model1)



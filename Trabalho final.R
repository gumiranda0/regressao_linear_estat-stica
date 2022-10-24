#################################################################
## TRABALHO FINAL
## Disciplina Regressao Linear I
## Especializacao em Estatistica - UFMG
## Prof. Guilherme Lopes de Oliveira
## Alunos Gustavo, Renato
#################################################################


#--
#Limpando a memoria do R (aconselhavel):
rm(list=ls(all=TRUE))


#--
# Ler conjunto de dados:
dados <- read.csv("planosaude.csv", header=T, dec=".", sep=",")
str(dados)   #Examina a estrutura do data frame "dados"
names(dados)

var= c("despmed","renda","idade","dcron","plano")

#--
#Analise descritiva (individual para cada variavel):
#despmed
summary(dados$despmed)
sd(dados$despmed)
hist(dados$despmed)
boxplot(dados$despmed)

#renda
summary(dados$renda)
sd(dados$renda)
hist(dados$renda)
boxplot(dados$renda)

#idade
summary(dados$idade)
sd(dados$idade)
hist(dados$idade)
boxplot(dados$idade)

#dcron
summary(dados$dcron)
sd(dados$dcron)
hist(dados$dcron)
boxplot(dados$dcron)

#plano (variavel categorica)
summary(dados$plano)
table(dados$plano) #tabela de frequencia
boxplot(dados$despmed ~ dados$plano) #relacionado com outra variável, pois é categórica


#--
#Analise do relacionamento entre as variaveis:
#renda
with(dados, plot(renda, despmed, pch=16, col="blue"))
with(dados, cor(idade, despmed, method="pearson"))

#idade
plot(dados$idade, dados$despmed, pch=16, col="blue")
cor(dados$idade, dados$despmed, method="pearson")

#dcron
with(dados, plot(dcron, despmed, pch=16, col="blue"))
with(dados, cor(dcron, despmed, method="pearson"))

#dcron&&idade
with(dados, plot(idade, dcron, pch=16, col="blue"))
cor(dados$idade, dados$despmed, method="pearson")

#plano
boxplot(dados$despmed ~ dados$plano) #Box-plot

if(!require(ggplot2)){ install.packages("ggplot2")}; require(ggplot2)
ggplot(dados, aes(x = renda, y = despmed, color = plano)) +    #Plota grafico
  geom_point(size=2, shape=16) +                           #Muda padr�o dos pontos  
  scale_color_manual(values=c("violetred", "royalblue", "red")) +  #Muda cores
  geom_smooth(method = "lm", se=FALSE)                     #Coloca linhas 


#--
#Ajuste do modelo de regressao linear múltipla (MRLM):
str(dados)
dados$plano <- factor(dados$plano, labels=c("bronze", "ouro", "esmeralda"))
modelo <- lm(despmed ~ renda + idade + dcron + idade:dcron + factor(plano), data = dados)
modelo            #Equa��o do modelo ajustado
summary(modelo)   #Mostra testes t e outros resumos do ajuste
anova(modelo)     #Tabela ANOVA
confint(modelo, level=0.95)  #Intervalos de Confianca para coeficientes





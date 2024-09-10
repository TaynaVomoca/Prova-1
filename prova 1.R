##############################
#PACOTES
##############################
install.packages("ggplot2")
library(ggplot2)
install.packages("corrplot")
library("corrplot")
install.packages("dplyr")
library(dplyr)
install.packages("readr") 
library(readr)
install.packages("esquisse")
library(esquisse)
install.packages("kmed")
library(kmed)
install.packages("sjPlot")
library(sjPlot)
install.packages("caret")
library(caret)
##############################

#a)
glimpse(leite)

#O R compreende como qualitativa uma variável, sendo ela a avaliação
#O restante (pH, temperatura, gosto, odor, gordura, turbidez, color) ele entende como quantitativas

#Não parece totalmente certo pois gosto, odor, gordura e turbidez são variáveis qualitativas.

#mudando com factor
leite$gosto <- factor(leite$gosto, levels = c(0,1), labels = c("ruim", "normal"))
leite$odor <- factor(leite$odor, levels = c(0,1), labels = c("ruim", "normal"))
leite$gordura <- factor(leite$gordura, levels = c(0,1), labels = c("baixo", "alto"))
leite$turbidez <- factor(leite$turbidez, levels = c(0,1), labels = c("baixa", "alta"))

glimpse(leite)

#b)
#Tabela de dupla entrada
table(leite$gosto, leite$avaliacao)
table(leite$odor, leite$avaliacao)
table(leite$gordura, leite$avaliacao)
table(leite$turbidez, leite$avaliacao)

#gráfico de barra
esquisser(leite)

#teste qui-quadrado
chisq.test(table(leite$avaliacao, leite$gosto))
chisq.test(table(leite$avaliacao, leite$odor))
chisq.test(table(leite$avaliacao, leite$gordura))
chisq.test(table(leite$avaliacao, leite$turbidez))

#Para todos valores menores que 0.05  podemos rejeitar a hipótese de independência.

#As variáveis que mais impactam a qualidade do leite sao, sequencialmente: turbidez, gordura, odor e gosto.

#c) 

leite_ruim <- leite %>% filter(avaliacao == "ruim")
leite_aceitavel <- leite %>% filter(avaliacao == "aceitável")

#calculo para leite ruim
mean(leite_ruim$pH, na.rm = TRUE) #Média = 6.588
median(leite_ruim$pH, na.rm = TRUE) #Mediana = 6.8
sd(leite_ruim$pH, na.rm = TRUE) #dp = 2.19

mean(leite_ruim$temperatura, na.rm = TRUE) #Média = 50.28
median(leite_ruim$temperatura, na.rm = TRUE) #Mediana = 45
sd(leite_ruim$temperatura, na.rm = TRUE) #dp = 13

mean(leite_ruim$color, na.rm = TRUE) #Média = 252.73
median(leite_ruim$color, na.rm = TRUE) #Mediana = 255
sd(leite_ruim$color, na.rm = TRUE) #dp = 3.23

boxplot(leite_ruim$pH)#boxplot pH
boxplot(leite_ruim$temperatura)#boxplot temperatura
boxplot(leite_ruim$color)#boxplot cor

#calculo para leite aceitável
mean(leite_aceitavel$pH, na.rm = TRUE) #Média = 6.658
median(leite_aceitavel$pH, na.rm = TRUE) #Mediana = 6.6
sd(leite_aceitavel$pH, na.rm = TRUE) #dp = 0.125

mean(leite_aceitavel$temperatura, na.rm = TRUE) #Média = 40
median(leite_aceitavel$temperatura, na.rm = TRUE) #Mediana = 38
sd(leite_aceitavel$temperatura, na.rm = TRUE) #dp = 3.69

mean(leite_aceitavel$color, na.rm = TRUE) #Média = 251.23
median(leite_aceitavel$color, na.rm = TRUE) #Mediana = 255
sd(leite_aceitavel$color, na.rm = TRUE) #dp = 4.81

boxplot(leite_aceitavel$pH)#boxplot pH
boxplot(leite_aceitavel$temperatura)#boxplot temperatura
boxplot(leite_aceitavel$color)#boxplot cor

#extra
t.test(mean(leite_aceitavel$pH))
t.test(leite_ruim$temperatura)

t.test(leite_aceitavel$color)
t.test(leite_ruim$color)

t.test(leite_aceitavel$pH)
t.test(leite_ruim$pH)

#d)
#teste e treino

indices <- sample(1:1059, 847, replace = FALSE)

treino <- leite[indices,]
teste <- leite[-indices,]

leite$avaliacao <- ifelse(leite$avaliacao == "aceitável", 1,0)

modelo <- glm(data = leite, formula = avaliacao ~., family = 'binomial')
summary(modelo)

#Todas as varáveis são estatisticamente significativas pois todos os valores são menores que 0.05

exp(coef(modelo))
#Com base no resultado, podemos ver que odor normal, gordura alta e ph, tem alto impacto na avaliação enquanto as demais como gosto normal, temperatura, turbidez alta e cor, impactam menos na avaliação.

#e)
predicoes <- predict(modelo, newdata = teste, type = 'response')
predicoes <- ifelse(predicoes > 0.5, 1, 0)

confusionMatrix(factor(predicoes), factor(teste$avaliacao))
#Curácia do modelo = 0.8208 

#f) As variáveis que a empresa deveria ficar mais atenta são as quem tem um maior impacto no leite, sendo eles:  odor, gordura alta e ph


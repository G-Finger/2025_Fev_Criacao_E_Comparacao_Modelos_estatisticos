# Jamais saia analisando dados sem fazer um planejamento prévio. 
# Definir nosso projeto de Curso
#---------------------------------------------------
# O que afeta a qualidade do ar? Como?
#---------------------------------------------------


#install.packages('Ecdat') # caso não estaja instalado
# chamar o pacote Ecdat e carregar
library(Ecdat)

data(Airq) # banco de dados da bilbioteca
names(Airq) # exibe o nome das variáveis

# Descrevendo as variáveis
# airq: índice de qualidade do ar (quando menor, melhor!)
# vala: Valor das empresas nas cidades (em milhares de dólares)
# rain: quantidade de chuva (em polegadas)
# coas: posição costeira da cidade (sim ou não)
# dens: densidade populacional (pessoas por milhas quadrada)
# medi: renda média per capital (Em dólares)

# Análise descritiva destes dados

summary(Airq) # resumo das variáveis. 

# variáveis podem ser contínuas ou categóricas. 

plot(airq~vala, data= Airq) # o ~ representa em função

# criando um modelo estatístico
# y (resposta) ~ x (explicativa)
# y ~x1 + x2 + x3 

#----------------------------------
#----------------------------------


# Montando o Modelo
m1<-lm(airq~vala, data=Airq) # lm (modelo linear)

# alguns dados podem não ser lineares
m1
summary(m1) # para saber a significancia

# o p-valor indica a dignificancia do modelo ou da váriavel
# se o p-valor < 0.05 é significante. Isto é, existe um efeito
# se o p-valor > 0.05 Não é significante. Isto é, não existe um efeito

# A variável vala não influenciou a qualidade do ar nas cidades. 

m2<-lm(airq~coas, data=Airq)
m2
# A Variável Coas afeta a váriavel "Airq"
summary(m2)
# Sim, a posição costeira inflencia ou afeta a qualidade do ar das cidades. 
plot(airq~coas, data= Airq)

# as cidades costeiras apresentam uma melhor qualidade do ar. 

# A variável Medi afeta a váriavel do ar?
m3<-lm(airq~medi, data=Airq)
summary(m3)
plot(airq~medi, data= Airq)

# a variável não afetou a qualidade do ar. 

# a quantidade de chuva influencia na qualidade do ar
m4<-lm(airq~rain, data=Airq)
summary(m4)
plot(airq~rain, data= Airq)

# a quantidade de chuva não afeta a qualidade do ar. 

# a densidade populacional afeta a qualidade do ar?
m5<-lm(airq~dens, data=Airq)
summary(m5)
plot(airq~dens, data= Airq)

# A densidade não afeta a qualidade do ar destas cidades

# retas de modelos não significativos são opcionais nos gráficos. 


# retas nos gráficos. 
plot(airq~rain, data= Airq)
#y= a + b*x
# b é inclinção da reta
# x é a variável explicativa
# a onde a reta irá tocar no eixo y - interecep
summary(m3)
curve(9.936e+01+5.638e-04*x,add=TRUE)

# melhorar o gráfico. 
plot(airq~medi, data= Airq, xlab='Renda média per Capita', ylab='Qualidade do ar', pch=1, col='Blue',
     cex.lab=1.1,main='Renda média - 2010')
curve(9.936e+01+5.638e-04*x,add=TRUE, col='darkBlue',lwd=2, lty=2)

#melhorando os gráficos. 

plot(airq~vala, data= Airq, xlab='Valor das Empresa $', ylab='Qualidade do ar', pch=16, col='Blue',
     cex.lab=1.0,main='Renda média - 2010')
curve(9.936e+01+5.638e-04*x,add=TRUE, col='darkBlue',lwd=2, lty=2)

plot(airq~coas, data= Airq, xlab='Posição Costeira', ylab='Qualidade do ar', col='lightblue',
     main= 'Análise da qualidade do ar')

summary(m5)
curve(1.054e+02+-3.857e-04*x, add= TRUE,col='darkBlue',lwd=2, lty=2)

#modelo de regressão múltipla, 

m_R_m1<-lm(airq~vala+coas, data=Airq)
summary(m_R_m1)

# então existe um efeito da posição costeira e do valor das empresas na qualidade do ar

# gráfico Regressão Múltipla
plot(airq~vala, data= Airq,xlab='Valor das Empresa $', ylab='Qualidade do ar')
curve(1.171e+02+  1.999e-03*x,add=TRUE) # cidade não costeiras
curve(1.171e+02+  1.999e-03*x+-2.968e+01,lty =2, add = TRUE) # cidade costeira
legend("bottomright",c('Não Costeira', 'Costeira'),pch=1,lty=c(1,2), bty="n")

# a qualidade do ar das cidades é afetada pelo valor das empresa
# quanto pela posição das cidades. 
# quanto maior o valor das empresa, pior a qualidade do ar nas cidades. 
# além disso as cidade não-costeiras apresentam qualidade do ar pior que as costeiras. 


lm(airq~vala+coas+dens, data=Airq)
summary(m_R_m2)

# Parece que a densidade populacional não interfere na qualidade do ar. 
# contraste de modelos
# comparar um modelo completo com um modelo sem a variável em questão
modelo_completo<-lm(airq~vala+coas+dens, data=Airq)
modelo_INcompleto<-lm(airq~vala+coas, data=Airq)

# os modelos são iguais?
# se p>0.05 não há siginificância (não existe diferença entre os modelos)
# então eu continuo com o modelo mais simples
# se p<0.05,os modelos são diferentes e a variável não deve ser retirar do modelo.

anova(modelo_completo,modelo_INcompleto)

# Gráfico Final

plot(airq~vala, data= Airq,xlab='Valor das Empresa $', ylab='Qualidade do ar',
     cex.lab=1.3,pch=17, col="blue")
curve(1.171e+02+  1.999e-03*x,add=TRUE, col='blue', lwd =1.4) # cidade não costeiras
curve(1.171e+02+  1.999e-03*x+-2.968e+01,lty =2, add = TRUE,col='blue', lwd =1.4) # cidade costeira
legend("bottomright",c('Não Costeira', 'Costeira'),pch=17,lty=c(1,2), bty="n", col=c('blue','blue'))

# Conclusão
#---------------------------------
# O que afeta a qualidade do ar nas cidades?
# As variáveis que afetaram foram: (a) valor das empresa e (b) a posição
# das cidades. Quanto maior o valor das empresa, pior a qualidade do ar. Cidades
# costeiras apresentam uma melhor qualidade. 


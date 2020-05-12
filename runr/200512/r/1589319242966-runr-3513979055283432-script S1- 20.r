
#UNIVERSIDADE ESTADUAL DE LONDRINA – CCE – Depto Estatística
#Atividades do Mestrado e Doutorado:“Ciências Biológicas”-turma 2020
#Profa. Dra. Ana Verginia Libos Messetti – messetti@uel.br


##########################################################
# PROGRAMA R
# INSTALAÇÃO "Home page do R" :
#(http://www.r-project.org)
# Espelho: Brasil (UFPR)
###########################################################
# INTRODUÇÃO a ESTATISTICA  - Ano de 2020
###########################################################

# OPERAÇÕES ARITMÉTICAS
  2+1 
  14-10
  4*5
  48/12
  sqrt(16)
 
# lER SEQUÊNCIA DE NÚMEROS 
  1:6

# VARIÁVEIS  
  X = 1:5 ;X
  Y = X+2 ;Y
  Z = X*Y ;Z 

# EXTRAIR ELEMENTOS MENOR QUE 15 DO CONJUNTO Z  
  Z[Z<15]
 


#-----------------------------------------------
# OBJETOS: Variáveis; Vetores; Matrizes; Funções; 
#-----------------------------------------------
# VETORES
#----------------------------------
 A= c(10,12,14,15,20) 
 A
# ou
 A= c(10,12,14,15,20); A

 length(A)    # largura do vetor
 mean(A)      # media 
 var(A)       # variância
 sd(A)        # desvio padrão



#----------------------------------
# MATRIZES
#----------------------------------
 E= matrix(1:12, ncol=3); E      #Define a matriz Ej
 length(E)                       #Número de elementos da matriz E 
 dim(E)                          #dimensão (linha x coluna)
 nrow(E)
 ncol(E)
 E [1,2]                         #elemento da linha i=1 e coluna j=2

#-----------------------------



#-------------------------------------------------
#  CRIAR ou LER ARQUIVOS
#-------------------------------------------------
# Para verificar as variáveis na memória: 
  ls()

# Para remover dados antigos da memória:
  rm(list=ls())
  A
# Para verificar as variáveis na memória: 
  ls()



#--------------------------------------------
# CRIAR UM CONJUNTO W - (VETOR)
#--------------------------------------------

 W= c(7,8,6,6,5,9,9,9,4); W
 table(W)
 length(W)



#-------------------------------------------
# OPÇÃO 2 -  CRIAR DATA-FRAME
#-------------------------------------------
M=2:8 
M
N=c(0,5,10,15,20,25,30)
M
N

DF= data.frame(M, N)
DF 
names(DF)                # nome das variáveis do data frame  
plot(DF)                 # gráfico de dispersão
mean(M)               # média da variável M
mean(DF$N)               # Média da variável N
var(DF$M)                # variância M
sd(DF$M)                 # desvio padrão M


#-----------------------------------------------
# opção 3 - IMPORTAR ARQUIVOS
#-----------------------------------------------
#------------------------------------------------
# leitura do excel arquivo.csv (pen drive)
#------------------------------------------------

#dados=read.csv("E:/Pasta/Arquivo2013.csv",sep=",",head=T)

#ou
dados=read.csv("E:/Mestrado Bio/Aedes 2013.csv",sep=",",head=T)
dados
attach(dados)

#atenção depende da versão (excel) usa: sep=";" ou sep=",". 

#=======================================================================









###################################################
# ATIVIDADE 1 - ANÁLISE EXPLORATÓRIA DE DADOS
##################################################

##############
# exercicio 3
#-------------
rm(list=ls())

hib = c("R","MR","MS","R","S","MS","MR","S","R","MS","R","S","MS","S",
"R","MR","R","MS","S","R","MS","S","MS","R","S","MR","MS","R","R","MR",
"MR","MS"); hib

table(hib)                #contar frequencias simples ou absoluta

t1 = table(hib); t1        #dar nome a tabela t1
addmargins(t1)

barplot(t1)                #gráfico simples  ou
barplot(table(hib))        

barplot(t1, xlab ="Resistência a ferrugem", ylab ="frequência", 
     main ="Resistência a ferrugem de 32 híbridos",
     col=c("purple","yellow","green3","blue"), names=c("MR","MS","R","S"),
     legend=FALSE)


barplot(t1, ylab ="Resistência a ferrugem", xlab ="freq", 
     col=c("lightblue", "mistyrose", "lightcyan","lavender"),
     names=c("MR","MS","R","S"),
     legend=FALSE, horiz = TRUE)
     title(main = "Resistência a ferrugem de 32 híbridos", font.main =4)





prop.table(t1)              #freq relativas
pie(t1)                     #gráfico de Setores 
pie(t1,prop.table(t1))




#-------------------
# gráfico de setores
#-------------------

pie(prop.table(t1),col=c("lavender", "cornsilk","blue","pink"),
    main="", bord="yellow",density=10, angle=15)

pie(prop.table(t1),col=c("lavender", "cornsilk","blue","pink"),
    main="", bord="red",density = 50, angle = 70)


#-----------------------------------------------------------






###################################
# Exercicio 4 
# Índice de gordura na manteiga 
#----------------------------------
#############################
# PACOTE FARAWAY   
#############################

#PARA BUSCAR BANCO DE DADOS
data(packages='faraway')

#Department of Mathematical Sciences,
#University of Bath, PROF.JULIAN FARAWAY
#Bath, BA2 7AY  UNITED KINGDOM


#History
#I got my B.A. in Mathematics (1982) and Diploma 
#in Mathematical Statistics (1983) from the 
#University of Cambridge. I have a
# PhD in Statistics (1987) from the Department 
#of Statistics, University of California, Berkeley. 
#I used to work at the Department of Statistics,
# University of Michigan(1987-2006).



########################################
# Indice de GORDURA NA MANTEIGA / RAÇA
#########################################
#-------------------------------
# pacote (FARAWAY)
#------------------------------
install.packages('faraway',dep=T) 
require(faraway)

data(butterfat)
head(butterfat)
 attach(butterfat)
butterfat



# n=100 vacas leiteras
# RAÇA (AYRSHIRE / CANADIAN / GUERNSEY /
         HOLSTEIN-FRESIAN / JERSEY )



#############################


#------------------------
# a. nomear as variáveis 
#------------------------

a=Butterfat; a     #INDICE DE GORDURA NA MANTEIGA 
b=Breed; b         #RAÇA  
c=Age; c           #FAIXA ETÁRIA





#------------------
# b. gráfico
#-----------------
 
t1=table(b); t1
barplot(t1)
 
20/100
pie(t1)






#----------------------------
# c. n=? tamanho da amostra
#----------------------------
 
t1=table(b); t1
barplot(t1)
pie(t1,prop.table(t1))

t2=table(c); t2
barplot(t2)







###--------------------------------------
#-------------------------------
# atenção - Var. qualitativa
#-------------------------------
t3=table(a) ;t3
barplot(a)           # não é gráfico para quantitativa

#--------------------------------------
# para variáveis quantitativas 
#optar por outros gráficos
#------------------------------------
hist(a)
boxplot(a)
##---------------------------------------






#----------------------------------
# d. Gráfico para “faixa etária”
# variável qualitativa 
#-----------------------------------
 
t2=table(c); t2
barplot(t2,col=c("blue","green"))


###-----------------------------------------------










##########################
# Exercício 5 
# Diâmetros dos Eucaliptos 
#---------------------------
rm(list=ls())

diam = c( 2.2,2.3,2.5,2.6,3.0,3.5,3.5,3.8,3.8,3.9, 
4.1,4.1,4.1,4.1,4.1,4.2,4.3,4.3,4.4,4.4,4.6,4.9,5, 
5.0,5.3,5.8,6,6,6,6,6,6.5,6.9,7.1,7.2,7.7,8.3,8.5,11.3,13.8);diam

range(diam)
ac= 13.8-2.2;  ac
k=sqrt(40);    k
c=ac/k;        c       #aproximadamente c=2

sort(diam)

#---------------------------------------
# a) Tabela de freq. e o  Histograma
#---------------------------------------
graf= hist(diam, plot=F, right=F); graf    #distribuição de frequência 


#----------------------------------
# b) opções de gráficos
#----------------------------------

#histograma------
hist(diam, xlab="Diâmetros", ylab="frequências", col="blue",right=F,main="") 


#boxplot-----

               

# ramos e folhas------







#----------------------------------
# c) quartis - Decis - Percentis
#     Resumo dos 5 números 
#   
#----------------------------------

min(diam)
max(diam) 

quantile(diam,type=5)         #min,q1,q2,q3,max
quantile(diam,0.25,type=5)         

quantile(diam,type=2)         #min,q1,q2,q3,max
quantile(diam,0.25,type=2)




####################################
#----------------------------------
# d)Análise Exploratória de Dados 
#----------------------------------

#=-------------------
#MEDIDAS DE POSIÇÃO
# ou tendencia central 
#--------------------
 
#Media 



#Mediana



#Moda
 table(diam)
which(table(diam)==max(table(diam)))

 




#---------------------- 
#MEDIDAS DE DISPERSÃO
# ou VARIABILIDADE
#----------------------
#AMPLITUDE TOTAL
 
 Xmax = max(diam); Xmax
 Xmin = min(diam); Xmin 
 AT = Xmax - Xmin; AT

 #ou somente a máximo e mínimo
 range(diam)




#---------------------
# variância
#---------------------
 
 


#---------------------
# desvio padrão
#---------------------
 


#-------------------------
# coeficiente de variação
#------------------------




####----------
#-------------
#SEPARATRIZES
#-------------

 quantile (diam, type=5)
 quantile (diam,0.5)
 quantile (diam,0.75)




#---------------------------------------
# e) QUANTOS OUTLIERS"???
#----------------------------------------





#-----------------------------------
# f) teste de Shapiro Wilk
#-----------------------------------
                           #p-valor<0.05 rejeita-se Ho 
                           #H0: Os dados seguem d. n.)  





#---------------------------------
# g)retirando outliers do conjunto 
# renomear o novo conjunto de dados 
#----------------------------------

diam2 = c( 2.2,2.3,2.5,2.6,3.0,3.5,3.5,3.8,3.8,3.9, 
4.1,4.1,4.1,4.1,4.1,4.2,4.3,4.3,4.4,4.4,4.6,4.9,5, 
5.0,5.3,5.8,6,6,6,6,6,6.5,6.9,7.1,7.2,7.7,8.3,8.5);diam2

length(diam2)
boxplot(diam2,main="Diâmetros", xlab="freq", horizontal=F,col="blue")



#-------------------------
# G.teste de shapiro-Wilk
#-------------------------
shapiro.test(diam2)




#-------------------------------------------
# MEDIDAS DESCRITIVAS 
##comparando as estimativas dos 2 conjuntos
# COM E SEM OUTLIERS
#-------------------------------------------

mean(diam)   
mean(diam2)

var(diam)
var(diam2)

sd(diam) 
sd(diam2)

cv= sd(diam)/mean(diam); cv 
cv= sd(diam2)/mean(diam2); cv 



#----------------------------------------
# g) teste de Shapiro Wilk sem outliers
#----------------------------------------
# O que ocorreu com a retirada dos outliers??




#-----------------------------------
# h) O que ocorreu sem outliers??
#-----------------------------------












####################################################
# Exercício 6 
# continuação ex4 - Índice de gordura na manteiga 
#---------------------------------------------------
########################################
# Indice de GORDURA NA MANTEIGA / RAÇA
#########################################
#-------------------------------
# pacote (FARAWAY)
#------------------------------
install.packages('faraway',dep=T) 
require(faraway)

data(butterfat)
head(butterfat)
 attach(butterfat)
butterfat

# n=100 vacas leiteras
# RAÇA (AYRSHIRE / CANADIAN / GUERNSEY /
         HOLSTEIN-FRESIAN / JERSEY )
#############################


#------------------------
# nomear as variáveis 
#------------------------

a=Butterfat; a     #INDICE DE GORDURA NA MANTEIGA 
b=Breed; b         #RAÇA  
c=Age; c           #FAIXA ETÁRIA

#-----------------------------------------
#  a - Boxplot para Indice de gordura
#-----------------------------------------





#---------------------------------------------
#  b -   Ramos e folhas para Indice de gordura
#---------------------------------------------


stem(a,scale=0.5)


#-----------------------------------------
#  c -   Resumo dos 5 numeros
#-----------------------------------------

quantile(a,type=2)



#-----------------------------------------
#  d - medidas descritivas
#-----------------------------------------


 




#-----------------------------------------
#  e - Teste de Shapiro-Wilk  
#    Ho: Os dados provém de uma população com 
#        distribuição normal 
#-----------------------------------------






#------------------------------
#  f - Grupo 1 - Jersey  
#-----------------------------

#GRUPO 1
gj = a[b=="Jersey"]; gj         # gordura / jersey
length(gj) 








#------------------------------
#   - Grupo 2 - Guernsey  
#-----------------------------


#GRUPO 2
gg = a[b=="Guernsey"]; gg         # gordura / guernsey
length(gg) 









#------------------------------
#  f - C.V dos Grupos  
#-----------------------------







#----------------------------------------
# g- boxplot para duas variáveis
#----------------------------------------












#############################################################
#############################################################
# ATIVIDADE 2 - Intervalo de Confiança e Teste de hipóteses
#############################################################
#---------------------------------------
# Intervalo de Confiança para proporção
# Default (Y, n, conf.level = 1-alfa)
#---------------------------------------
#----------------------------------------------------------------------
# Intervalo de Confiança para proporção
# Default: (po, n, conf.level = 1-alfa)
# Intervalo de Confiança para média - conhece a variabilidade
# Default: ci.mu.z(x, con=0.95, sigma=y)
# Intervalo de Confiança para média - desconhece a variabilidade
# Default: t.test(x, conf.level=0.95)
#-----------------------------------------------------------------------




#########################################
# Exercício 1 - 
#I.C. proporção de peixes acima de 50mm
#----------------------------------------
14/35     #40% dos 35 peixes (estimativa pontual) 

prop.test(14,35,conf.level = 0.90)   #alfa 10%
prop.test(14,35,conf.level = 0.95)   #alfa 5%
prop.test(14,35,conf.level = 0.99)   #alfa 1%



#-------------------------------- 
# Valores Z: tabela Normal padrão
# INTERVALO DE CONFIANÇA
#-------------------------------

z=qnorm(c( 0.05, 0.95),mean=0,sd=1);z  #alfa10%
z=qnorm(c(0.025,0.975),mean=0,sd=1);z  #alfa5%
z=qnorm(c(0.005,0.995),mean=0,sd=1);z  #alfa1%









##############################
# Exercício 2 
# I. C. para proporção  
#-------------------------------------



#-----------------------------------------
# programando os resultados
#-----------------------------------------
### para alfa 1%  #variável: Não imunizados
n=160; n
y=20
y/n
p=0.125;  p
1-0.125
q=0.875;  q

ep = sqrt((p*q)/n) ; ep
ep=0.026
z = qnorm(c(0.005, 0.995), mean=0, sd=1, lower.tail=TRUE);z
erroamostral = z*ep; erroamostral
ICP = c(p+z*ep);ICP

0.125-0.068




#--------------------------------------------
# Ex3- Forma direta no R
#--------------------------------------------














#########################################
# Exercício 3 -  I. C. 
# Pesos de pacotes de sementes milho 
#--------------------------------------

peso=c(20.05,20.1,20.25,19.78,19.69,19.90,20.2,19.8,19.7,20.3,19.93,20.25,
      20.18,20.01,20.09) 

mean(peso)
sd(peso)
length(peso)

boxplot(peso, las=1, col="green3", ylab= "Biomassa")
points(mean(peso), pch= '+', cex=1.5) 



#---------------------------------
# Programando o resultado
#---------------------------------
IC= mean(peso) + qnorm(c(0.025,0.975))*(0.2/sqrt(15));  IC  #programando os cálculos
ea=qnorm(c(0.025,0.975))*(0.2/sqrt(15)); ea





#----------------------- 
# tabela Normal padrão
#-----------------------
 z=qnorm(c(0.025,0.975),mean=0,sd=1);z    #alfa=5%




#-----------------------------
# FORMA DIRETA NO R
#-----------------------------
# 1) INSTALAR O PACOTE ****
#--------------------------------
install.packages('asbio',dep=T) 


#-----------------------------------
#2) carregar o pacote 
#----------------------------------


require(asbio)
 ci.mu.z(peso, con=0.99, sigma=0.2)









###################################
# Exercício 4 -  I.C. ouriço do mar
#----------------------------------

our=c(4.5,5.2,6.1,2.6,3.2,3.7,3.9,4.6,4.7,4.1)










#----------------------- 
# tabela "t de Student"
#-----------------------
qt(c(0.05,0.95),  df=9)    # n.s =10%
qt(c(0.025,0.975),df=9)    # n.s.=5%
qt(c(0.005,0.995),df=9)    # n.s =1%












###############################
# Exercício 5 - 
#IC para resíduos industriais
#----------------------------

#-------------------------------
# pacote (FARAWAY)
#------------------------------
install.packages('faraway',dep=T) 
require(faraway)

data(butterfat)
head(butterfat)
 attach(butterfat)
butterfat

# n=100 vacas leiteras
# RAÇA (AYRSHIRE / CANADIAN / GUERNSEY /
         HOLSTEIN-FRESIAN / JERSEY )



#------------------------
# a. nomear as variáveis 
#------------------------
a=Butterfat; a     #INDICE DE GORDURA NA MANTEIGA 
n=100
media=mean(a); media
dp=sd(a);dp



#--------------------------------------------
# programando o resultado - calculadora
#---------------------------------------------

# erro padrao----------------------
ep = (dp/sqrt(n)) ; ep   
ep=0.0715

# valor tabelado-------------------
t=qt(c(0.025,0.975),df=99) ;t   # ttab; n.s.=5%
1.98

#erro amostral ----------------------
erroamostral = t*ep; erroamostral
ICP = c(media+t*ep);ICP
#-----------------------------------------------







#----------------------
# forma direta no r
#----------------------

 t.test(a, conf.level=0.95)







####################################################
# Exercício 6 -  Teste de hipóteses 
#    Área foliar espécie Laguncularia rancemosa 
#---------------------------------------------------
#------------------------------------------------------------------
# Teste de Hipótese para Média  e Intervalos de confiança
# Default:t.test(x, y = NULL,
#          alternative = c('two.sided' ou  'less'  ou  'greater'),
#          mu = 0, paired = FALSE, var.equal = FALSE,
#          conf.level = 0.95, ...)
#-----------------------------------------------------------------

af=c(39.4,39.6,39.9,45.6,45.6,46.1,46.1,50.2,50.2,51,51.2,54.6,54.8,
     54.6,55.1,55.1,55.5,56.2,66.3,66.5)

length(af)
mean(af)
sd(af)







#-------------------------------------------------
# distribuição z (conhece Variância populacional)
# pacote  (BSDA)
#-------------------------------------------------
######################################################
#teste de hipótes para média (conhece a variabilidade)
#-----------------------------------------------------

require(BSDA)
sum(af)
zsum.test(mean.x=1023.6/20,sigma.x=4, n.x=20,
      alternative="two.sided",mu=50.76)



#----------------------- 
# tabela Normal padrão
#-----------------------
 z=qnorm(c(0.95),mean=0,sd=1);z    #alfa=5%
pvalor = 0.5-0.1808; pvalor










###############################################
# Exercício 7 -  Teste de hipóteses para média 
#                Área foliar da bromélia
#                Desconhece a variabilidade
#-----------------------------------------------
#---------------------------------
# Programa R de forma direta 
#---------------------------------

bro = c(17.3,15.2,8.2,18.4,9.1,15.4,13.4,19.6,10.5,26.4,8.6,12.8,9.5,10.6,9.8,
        17.0,19.1,10.8,8.8,13.1)
bro







#----------------------- 
# tabela "t de Student"
#-----------------------
qt(c(0.05),df=19,lower.tail=TRUE)      # alfa =5%; Unilateral esquerda










###############################################
# Exercício 8 -  Teste de hipóteses para média 
#                
#-----------------------------------------------
################-----------------------------------------------------
# Níveis de cálcio (mmol/l) e os níveis de albumina (g/l) no sangue
#-------------------------------------------------------------------

calcio= c(2.70,3.81,2.32,2.90,2.60,3.15,3.76,3.44)
albumina=c(43,42,42,40,42,38,34,42)







#--------------------------- FIM --------------------
















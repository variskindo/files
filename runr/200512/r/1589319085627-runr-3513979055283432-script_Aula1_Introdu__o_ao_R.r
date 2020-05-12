######################################################################
############### Aula 1 - Introdução ao software R ####################
######################################################################

# Instalar o interprete do R
#     https://www.r-project.org/
# Instalar o IDE, RStudio
#     https://rstudio.com/products/rstudio/

# Exploração e ajustes da interface

# Preparação do diretório de trabalho
getwd() # diretório atual
setwd("D:/AULAS/20200501_GLM/Aula_1_Software_R/") # seleciona o novo diretório



# Calculadora ########################################################



10 + 3
10 - 3
10 / 3
10 * 3
10 ^ 3
10 ** 3
(10 + 3) * 2



# Linguagem orientada por objetos #################################### 



# Veja o uso do operador "<-"
numeroA <- 10
numeroB <- 3
numeroC <- 2

# Operações podem ser realidas ao 'chamar' objetos pelos seus nomes
numeroA + numeroB
numeroA - numeroB
numeroA / numeroB
numeroA * numeroB
numeroA ^ numeroB
numeroA ** numeroB
(numeroA + numeroB) * numeroC

# Obs.: A linguagem é 'case sensitive'
dfa <- 2.1
dfA <- 1.2


# Funções ############################################################



# São comandos aplicados a objetos, que são informados como argumentos 
# entre '()'
sum(1, 1)
sqrt(9)



# Tipos de objetos ###################################################



# Depósito de diversos tipos de conteúdo em novos objetos
objetoA <- 35                      # numeric
objetoB <- "String de caracteres"  # character
objetoC <- 35L                     # integer
objetoD <- TRUE                    # logical (boolean)

# Checando o tipo
class(objetoA) # em alguns contextos pode ser chamado de 'double'
class(objetoB)
class(objetoC)
class(objetoD)

# Checando a estrutura
str(objetoA)
str(objetoB)
str(objetoC)
str(objetoD)



# Vetores ############################################################



c(1, 2, 3, 4) # concantenação de um cojunto específico de valores 
c(1:4)        # concatenação de um intervalo
1:4           # intervalo dispensa a função de concatenação
str(1:4)      # diagnostico: estrutura 
length(1:4)   # diagnostico: comprimento 
plot(c(1:10)) # diagnostico: visual

# exemplo: variação fictícia de temperatura diária durante um ano
{plot(rnorm(365, mean = 20, sd = 0.2) + 
  c(seq(1.5, -1, length.out = 160), seq(-1, 1.5, length.out = 205)), 
  type = "l", xlab = "Dia", ylab = "Temperatura (ºC)")
}

# um vetor pode ser construido de formas diferentes.
# Essa forma...
c(c(1:3), c(3:1))
# ...produz o mesmo vetor que essa:
vetorA <- 1:3
vetorB <- 3:1
vetorC <- c(vetorA, vetorB)
vetorC

# mas retém nomes de objetos, úteis para uso posterior, 
# Se estes objetos extras não são necessários
ls() # listando os objetos no ambiente do R
rm(vetorA, vetorB) # removendo alguns deles
ls() # veja como não estão mais disponíveis no ambiente

# É importante compreender como o comportamento de c() muda segundo a
# natureza do conteúdo.
# Vetor numérico
str(c(1, 2, 3, 4))
# Vetor de caracteres com um valor não-disponível ('NA')
str(c("um", "dois", "tres", NA))
# Vetor de valores lógicos ou booleanos
str(c(TRUE, FALSE, F, T, NA))
# Se um dos valores não é numérico, o restante é lido como caractere
str(c(1, "2", 3, NA))
# Vetor de valores inteiros
str(c(1L, 2L, 3L, NA))
# Se não são todos inteiros, são lidos como numéricos   
str(c(1L, 2L, 3, NA))
# Quando são combinados vários, character é o default por preservar 
# melhor o conteúdo
str(c(1L, 2.1, "tres", NA, TRUE))

# Outros valores especiais
1/0 # Infinito
0/0 # Idefinido

# Outras formas de criar vetores 
# Repetindo o valor uma determinada quantidade de vezes
?rep
rep(x = "TRUE", times = 4)
rep(FALSE, 4)
rep(1:3, 2)
rep(1:3, each = 2)
# ... por sequencias em intervalos definidos
seq(from = 0, to = 4, by = 0.5)
# ... por números gerados aleatoriamente (com distribução normal)
hist(rnorm(n = 100, mean = 2, sd = 0.5))

# As classes de vetores podem ser modificadas
# valores numéricos como caracteres 
str(as.character(c(1, 2, 3)))
# valores numéricos como valores inteiros
str(as.integer(c(1, 2, 3)))
# caracteres como valores numéricos
str(as.numeric(c("1B", "2", "3")))

fatores <- as.factor(c('A', 'B', 'C', 'A', 'B'))
fatores
length(fatores)
levels(fatores) # output é um vetor de strings
str(fatores)
as.character(fatores) # pode ser convertido em um vetor de strings
as.numeric(fatores) # ou em um vetor numerico
as.integer(fatores) # ou em um vetor de inteiros

# Mudando os nomes dos níveis e repetindo os diagnósticos
levels(fatores) <- c("um", "dois", "tres")
fatores
length(fatores)
levels(fatores)
str(fatores)
as.character(fatores)
as.numeric(fatores)
as.integer(fatores)



# Operações com vetores ##############################################



# Veja como a soma direta de vetores difere da função 'sum()'
vetor.x <- c(1, 1, 1, 1)
vetor.y <- c(1, 2, 3)
vetor.z <- c(10, 20)
vetor.i <- c(1, 1, 1, NA)

length(vetor.x)
length(vetor.y)

vetor.x + vetor.x # 
vetor.x + vetor.y # comprimento parcialmente compativel gera alerta
vetor.x + vetor.z # compativel, mas menor
vetor.x + vetor.i # compativel, mas com NA

sum(vetor.x, vetor.i) # NA gera comportamento indesejado
sum(vetor.x, vetor.i, na.rm = TRUE) # NA removido



# Pipelines ##########################################################



# Processos mais complexos como
# 'rnorm()' gera numeros aleatorios de distribuição gaussiana,
# 'round()' ajusta a quantidade de casas depois da virgula e
# 'sort()' coloca os valores em ordem decrescente
# Podem ser computados eusando uma sequencia de objetos no R base
A <- rnorm(n = 8)
A
B <- round(A, digits = 3)
B
C <- sort(B, decreasing = TRUE)
C

# Na forma de uma pipeline em R-base
sort(c(100, round(A, digits = 3)), decreasing = T)

# Na forma de uma pipeline no estilo do pacote 'dplyr'
library(dplyr) # carregando um pacote externo
# Baixar o pacote, caso não esteja instalado 
install.packages("dplyr") # executar e aguardar a instalação
library(dplyr) # segunda tentativa de carregar
A %>% round(3) %>% c(100, .) %>% sort(decreasing = T)


# Condições e Indexação ##############################################



# Condições dão valores lógicos como output
5 > 2                         # 'maior que'
2 > 5                         # 'maior que'
c(1, 3, 6) > 2                # 'maior que' em vetor
2 >= 2                        # 'maior ou igual a'
2 >= 1                        # 'maior ou igual a'
2 >= 5                        # 'maior ou igual a'
2 <= 5                        # 'menor ou igual a'
2 == 2                        # 'igual a'
2 == 3                        # 'igual a'
2 != 3                        # 'diferente de'
2 != 2                        # 'diferente de'
2 == 2 & 2 <= 3               # 'igual a' e 'menor ou igual a'
2 == 2 & 2 <= 1               # 'igual a' e 'menor ou igual a'
2 == 2 | 2 <= 1               # 'igual a' ou 'menor ou igual a'
3 != 4 | 3 >= 4               # 'diferente de' ou 'maior ou igual a'
3 != 3 | 3 >= 4               # 'diferente de' ou 'maior ou igual a'
3 %in% c(1, 2, 3, 4)          # 'está contido em'
c(1, 3, 6) %in% c(1, 2, 3, 4) # Está contido em


x <- c(1, 2, 3, 4, NA)
x <- c(1, 2, 3, 4)

if (anyNA(x)) {
  print("Tem NA")
} else {
  print("Não tem NA")
}



# Indexação em vetores ###############################################



# Criando o cetor usado nos exmeplos a seguir (sequência de 1 a 20,
# repetida 3x, com a concatenação de um NA no final)
x <- c(rep(seq(0, 20, 1), 3), NA)

# Indexação para obter subconjuntos ou subsets do vetor original
x[4]                          # 'somente o 4'
x[-4]                         # 'todos menos o 4'
x[2:4]                        # 'do 2 ao 4'
x[-(2:4)]                     # 'menos a seq de 2 a 4'
x[c(1, 5)]                    # 'somente 1 e 5'
x[x == 12]              # 'igual a 12'
x[x != 12]              # 'todos diferentes de 12'
x[x < 10]               # 'todos maiores que 10'
x[x <= 10]              # 'todos maiores ou iguais a 10'
x[x > 10 & x == 3]# 'maiores que 10' e 'iguais a 3'
x[x < 10 | x == 3]# 'menores que 10' ou 'iguais a 3'
x[x %in% c(2, 12)]      # todos os contidos no vetor
x[is.na(x)]             # valores NA

# Indexação para localizar as posições dos elementos que satisfazem as
# condições
which(x == 12)              # iguais a 12
which(x < 10)               # menores que 10
which(x <= 10)              # menores ou iguais a 10
which(x > 10 & x == 3)# maiores que 10 ou iguais a 3
which(x < 10 | x != 3)# menores que 10 ou diferentes de 3
which(x %in% c(2, 12))      # contidos no vetor

# Valor máximo no vetor
max(x) # note que o NA produz comportamento indesejado
max(x, na.rm = T) # mas pode ser ignorado
# Qual é a posição, ou índice, do valor máximo
which.max(x) # se existem multiplos, retorna somente do primeiro
# Para as posições de todos o que satisfazem essa condição
which(x == max(x, na.rm = T))

# Existem valores não-disponíveis no vetor?
anyNA(c(1, 2, NA, 4)) # o resultado é um valor lógico
# Quais valores satisfazem essa condição?
is.na(c(1, 2, NA, 4)) # o resultado é um vetor de valores lógicos
# Qual é a posição dos que satisfaem essa condição no vetor?
which(is.na(c(1, 2, NA, 4))) # o resultado é um índice
which(!is.na(c(1, 2, NA, 4))) # o resultado é um índice



# Matrizes ###########################################################



# Matrizes são vetores organizados em linhas e colunas, quando
# Primeiro, criamos um vetor
vetor.a <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
# Convertendo ele para o formato de uma matriz de 3 x 3
m <- matrix(vetor.a, nrow = 3, ncol = 3)
m # preenchimento por coluna
m <- matrix(vetor.a, nrow = 3, ncol = 3, byrow = T)
m # preenchimento por linha

# Alguns diagnósticos úteis
str(m)    # checar a estrutura
dim(m)    # checar as dimensões
length(m) # checar o comprimento do vetor implícito

attributes(vetor.a)
attributes(m)


# Exemplo: Matriz topografica com valores de altitude de um vulcão
m %>% glimpse() %>% image(col = viridis::viridis(100))

volcano %>% glimpse() %>% image(col = viridis::magma(100))


# Idexação e funções úteis
m[3, ]               # somente a linha 3 
m[, 3]               # somente a coluna 3
m[3, 3]              # somente o valor na linha 3 e na coluna 3
m[3]                 # sem coordenadas, o R le a matriz como vetor
m / 10               # operações matemáticas simples são recursivas
m + m                # operações matriciais 
m + c(0.1, 0.1, 0.1) # operações matriciais 
m %*% m           # multiplicação pela matriz transposta
m * t(m)           # multiplicação pela matriz transposta
sum(m)               # somatório
log(m)               # log

# Outra forma de criar uma matriz:
# Criar uma matriz vazia e introduzir o conteudo por indexação 
m <- matrix(nrow = 3, ncol = 3)
m[, 1] <- c(1, 2, 3)
m[, 2] <- c(4, 5, 6)
m[, 3] <- c(7, 8, 9)


# Reunir matrizes pre-existentes
cbind(m, m) # 'colando' em novas colunas
rbind(m, m) # 'colando' em nobas linhas 

# 
m <- matrix(vetor.a, nrow = 3, ncol = 3)
str(m)

x <- c(1, 2, 3, 4)
y <- c(4, 3, 2, 1)
m <- cbind(x, y)
str(m)

rownames(m) <- c("a", "b", "c", "d")
colnames(m) <- c("coluna 1", "coluna 2")
str(m)
m

attributes(m)
m
vetor.nomeado <- m[, "coluna 2"]
str(vetor.nomeado)
attributes(vetor.nomeado)



# Data frames ########################################################



# Compondo matriz para o exemplo
x <- c(1, 2, 3, 4)
y <- c(4, 3, 2, 1)
m <- cbind(x, y)
rownames(m) <- c("a", "b", "c", "d")

# Convertendo o tipo de objeto
dfA <- data.frame(m)
dfA <- as.data.frame(m)
m
dfA

# As diferenças são mais óbvias na estrutura dos objetos
str(m)
str(dfA)

# A indexação pode ser feita da mesma forma que em matrizes
dfA["a", "y"]
dfA[1, 2]
dfA[1, ]
# Com algumas praticidades extras, como a indexação por nome 
dfA$x 
dfA$y
# ... como a capacidade de reunir vetores de diferentes classes
dfA$z <- as.factor(c(2, 2, 1, 2))
dfA$log.de.X <- log(dfA$x)
dfA$log.de.X <- NULL
dfA$i <- as.character(c("um", "dois", "tres", "quatro"))
str(dfA)

dfA$especie <- c("Columba livia", "Passer domesticus", 
                 "Columba livia", "Passer domesticus")
dfA$morreu <- as.factor(c("morreu", "morreu","não morreu","morreu"))

# ... o input manual dos dados é mais simples
dfB <- 	data.frame(
  amostra = as.character(c("A", "B","C", "D", "E", "F")), # Importante para GLM
  massa = c(152, 171.5, 165, 152, NA, 165), 
  morreu = as.factor(c("sim", "não", "sim", "sim", "não", "sim")),
  pH = c(6.1, 7.3, 6, 6.1, 7.3, 6),
  tratamento = c(NA, "Pesticida", "Controle", "Pesticida",
                 "Controle", "Pesticida"),
  stringsAsFactors = FALSE)

dfB$tratamento <- as.factor(dfB$tratamento)
dfB
rownames(dfB) <- dfB$amostra
dfB

str(dfB)
nrow(dfB)
dim(dfB)
rownames(dfB)
colnames(dfB)
summary(dfB)
length(dfB)
ncol(dfB)
nrow(dfB)
dfB %>% as.matrix %>% length

# Exemplo de indexação orientada por uma condição 
dfB[dfB$massa > 165, ]
dfB[dfB$massa > 165, 1:2]
dfB[dfB$massa > 165, c(1, 4)]
dfB[dfB$massa > 165, c("pH", "tratamento")]


# Existem valores não-disponíveis no vetor?
anyNA(dfB) # o resultado é um valor lógico
# Quais valores satisfazem essa condição?
is.na(dfB) # o resultado é um vetor de valores lógicos
# Qual é a posição dos que satisfaem essa condição no vetor?
which(is.na(dfB), arr.ind = TRUE) # o resultado é um índice






# Importando dados ###################################################


"D:\AULAS\20200501_GLM\Aula_1_Software_R"

getwd()
list.files()
# setwd("/media/grosa/SSDext_GRos/AULAS/20200501_GLM/Aula_1_Software_R/")
setwd("D:/AULAS/20200501_GLM/Aula_1_Software_R/")
getwd()
list.files()

df.irisA <- read.csv(file = "iris.data.csv", 
                     row.names = 1) %>% glimpse()
df.irisB <- read.csv(file = "iris.dataV2.csv", header = F) %>% 
  glimpse()
df.irisC <- read.csv(file = "iris.dataV3.csv", sep = ";",
                     row.names = 1) %>% glimpse()


head(df.irisA, 10)
tail(df.irisA, 10)
head(df.irisB, 10)
head(df.iris, 10)

df.irisB$V6 %>% levels

read.csv(file = "iris.dataV2.csv", header = F, 
         na.strings = "") %>% glimpse()

df.irisC <- read.csv("iris.dataV2.csv", header = F, 
                     na.strings = "", row.names = 1,
                     stringsAsFactors = T) %>% glimpse()
str(df.irisC)
head(df.irisC, 10)
anyNA(df.irisC)
length(which(is.na(df.irisC)))
which(is.na(df.irisC), arr.ind = T)
df.irisC[-which(is.na(df.irisC), arr.ind = T)[, 1], ] %>% str

na.omit(df.irisC) %>% glimpse()



# Salvando o ambiente ################################################



save.image("")
load("")



# Exportando conjuntos de dados ######################################


df.irisC %>% str

# Identificar o diretório atual
getwd()
# Procurar o diretório de trabalho desejado
# setwd('')
# Lista arquivos
list.files(include.dirs = T)
# Criar um diretório chamado 'backup'
dir.create("backup")
# Verificar se ele consta no diretório de trabalho
list.files(include.dirs = T)
# Escrever um arquivo .csv com o conteúdo do objeto 'dfC'
write.csv2(x = df.irisC, file = "backup/df_backup2.csv")
# Ou um arquivo de texto
write.table(df.irisC, "backup/df_backup.txt", sep = ";")

saveRDS(df.irisC, "backup/dfC.objeto.bruto.rds")
rm(df.irisC)

ls()
df.irisC <- readRDS("backup/dfC.objeto.bruto.rds")
ls()


# Listas #############################################################




# Criando objetos dos próximos exemplos
x <- c(1, 2, 3, 4)
y <- c(4, 3, 2, 1)
z <- rnorm(6)
a <- c("um", "dois", "tres")
df <- dfB

# Criando listas:
list(NULL)                # Lista vazia      
list(x)                    
list(x, y)                       
list(x, y, z, a, df, sum)        

# A indexação é um pouco diferente do que em matrizes e dataframes
# Veja a partir deste exemplo:
lista1 <- list(x, y, z, a, df, sum)
# Elementos podem ser acessado como em um vetor
lista1[3]
lista1[3, ]
lista1[[3]]
# A indexação pode ser feita de forma recursiva no interior da lista
lista1[[3]][1]
lista1[[3]][2:3]
lista1[[5]]$altura
lista1[[5]][1, 3]
lista1[[5]]$peso[1]

# Veja o elemento da lista e o vetor a seguir
lista1[1]
c(2, 4, 6, 8)
# Tentativa de substituir o conteúdo pelo do vetor
lista1[1] <- c(2, 4, 6, 8) # não dá certo
lista1[1] # e destroi o elemento da lista

# Restaurando a lista ao estado original 
lista1 <- list(x, y, z, a, df, sum)
lista1[[1]] # Assim a indexação considera o elemento como vetor
lista1[[1]] <- c(2, 4, 6, 8) # substitui corretamente e 
lista1[[1]] # voilà

# Listas também suportam nomeação dos elementos
lista1 <- list(x, y, z, a, df, sum)
lista1
str(lista1) # Lista sem nomes
names(lista1) <- c(
  "vetor.num1", "vetor.num2", "vetor.rand1",
  "vetor.char", "conjunto de dados", "soma")
str(lista1) # lista com nomes

# O que abre novas possibilidades de indexação
lista1[1:2]  
lista1$vetor.rand1
lista1$vetor.rand1[2:3]
lista1$`conjunto de dados`$idade

# É possível desfazer a estrutura de uma lista
unlist(lista1[1:2])
do.call(cbind, lista1[1:2])
do.call(rbind, lista1[1:2])

lista2 <- lista1
lista2$inception <- lista1
str(lista2)

# lista2[3]$
lista2[[7]][[1]]
lista2[[7]]$vetor.num1
lista2$inception$`conjunto de dados`$idade

length(lista2)


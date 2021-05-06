# setwd('/storage/emulated/0/Documents/Onedrive')
# arquivos_compatives = arquivos_compatives2 = list.files(pattern = '.txt')
# for(i in 1:length(arquivos_compatives)){
#   arquivos_compatives2[i]=paste0(i,' - ',arquivos_compatives[i])
# }
# print(arquivos_compatives2)
# num_arq=as.numeric(readline('Selecione o arquivo de dados: '))
# arq = as.matrix(read.delim2(arquivos_compatives[num_arq],header = F))[,1]
# nome_arq = strsplit(arquivos_compatives[num_arq],split = '.txt')[[1]]
# num_arq = as.numeric(readline('Selecione o arquivo de chave: '))
# key = as.matrix(read.delim2(arquivos_compatives[num_arq],header = F))[,1]

# arq = as.matrix(read.csv('Teste.txt',header = F))[,1]
# key = as.matrix(read.delim2('Chave.txt',header = F))[,1]
a = readLines("stdin")
arq = a[1:(length(a)-2)]
key = a[length(a)-1]
func = a[length(a)]
alfabeto = c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',
             'A','B','C','D','E','F','G','H','I','J','K','L','M','N','P','Q','R','S','T','U','V','W','X','Y','Z',
             '1','2','3','4','5','6','7','8','9',' ')
alfabeto2 = c(alfabeto,alfabeto)
base_i = which(alfabeto == strsplit(key,split = "")[[1]][1])
desl_i = if(as.numeric(strsplit(key,split = "")[[1]][2])%%2 == 0){-1}else{1}
base_p = which(alfabeto == strsplit(key,split = "")[[1]][3])
desl_p = if(as.numeric(strsplit(key,split = "")[[1]][4])%%2 == 0){-1}else{1}

spttd = strsplit(arq,split = '')
# func=readline('Escolha o modo de opera��o (0 - Encode, 1 - Decode): ')

if(func == 0){
  ######### encode #########
  n_spttd=spttd
  for(j in 1:length(spttd)){
    nova_linha = spttd[[j]]
    evod=c(1:length(spttd[[j]]))%%2
    for(i in 1:length(evod)){
      if(evod[i] == 1){
        num = which(alfabeto == spttd[[j]][i])+base_i*desl_i
        cod = alfabeto2[if(num>0){num}else{num + length(alfabeto)}]
      }else{
        num = which(alfabeto == spttd[[j]][i])+base_p*desl_p
        cod = alfabeto2[if(num>0){num}else{num + length(alfabeto)}]
      }
      nova_linha[i]=cod
    }
    n_spttd[[j]]=nova_linha
  }
  n_spttd
  cod_final = NULL
  for(i in 1:length(n_spttd)){
    cod_final[i] = paste(n_spttd[[i]],collapse = "")
  }
  final = cod_final
}else if(func == 1){
  ######### decode ##########
  n_spttd=spttd
  for(j in 1:length(n_spttd)){
    nova_linha=n_spttd[[j]]
    evod=c(1:length(n_spttd[[j]]))%%2
    for(i in 1:length(nova_linha)){
      if(evod[i] == 1){
        num = which(alfabeto == nova_linha[i])-base_i*desl_i
        cod = alfabeto2[if(num>0){num}else{num + length(alfabeto)}]
      }else{
        num = which(alfabeto == nova_linha[i])-base_p*desl_p
        cod = alfabeto2[if(num>0){num}else{num + length(alfabeto)}]
      }
      nova_linha[i]=cod
    }
    n_spttd[[j]]=nova_linha
  }
  dec_final=NULL
  for(i in 1:length(n_spttd)){
    dec_final[i] = paste(n_spttd[[i]],collapse = "")
  }
  final = dec_final
}
final
write(final,file = paste0('Teste',if(func == 0){'_enc'}else('_dec'),'.txt',collapse = ""),)

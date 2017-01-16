complete<-function(directory,id=1:332){

nobs_vector<-c()
id_vector<-c(id)
##insere os "0"s na variavel id, pois do contrario a funcao nao encontra o arquivo
for(i in id){
  
  if(i<10){
    i_corrigido<-paste0("00",i,collapse="")
  }
  else{
    if(i>=10 & i<100){
      i_corrigido<-paste0("0",i,collapse="")
    }
    else{
      i_corrigido<-paste(i)
    }
  }
data<-read.csv(paste0(directory,i_corrigido,".csv",collapse=""))
##cria o vetor nobs_vector que contem o total de observaçoes de cada arquivo que é
##complete case
not_NAs<-complete.cases(data)
s<-sum(not_NAs)
nobs_vector<-append(nobs_vector,s)
}
##combina os vetores id_vector e nobs_vector e imprime
compcases<-cbind("ID"=id_vector,"nobs"=nobs_vector)
print(compcases)
##fim da funcao
}
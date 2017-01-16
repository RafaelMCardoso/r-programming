pollutantmean<-function(directory,pollutant,id=1:332){
  f_sum<-0
  s_sum<-0
  means_vector<-vector("numeric",length=length(id))
  ##print(means_vector)
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
    ##calcula a média ponderada dos valores nao-NAs e atribui as variaveis
    
    data<-read.csv(paste0(directory,i_corrigido,".csv",collapse=""))
    data_na<-is.na(data[,pollutant])
    f<-sum(!data_na)
    s<-sum(data[,pollutant],na.rm=TRUE)
    s_sum<-sum(s_sum,s)
    f_sum<-sum(f_sum,f)
  }
  final_mean<-s_sum/f_sum
  print(final_mean)
  ##fim da funcao
}
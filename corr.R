corr<-function(directory,threshold=0){
  
  setwd(directory)
  files_list<-list.files(directory,pattern=".csv")
  ##pattern=".csv" ignora subpastas e arquivos desnecessarios
  count<-vector("numeric",length = 0)
  cor_vector<-c()
  ##print(cor_vector)
  for(file in files_list){
    data<-read.table(file,header = T,sep = ",")
    compcases<-sum(complete.cases(data))
    
    if(compcases>=threshold){
      count<-vector("numeric",length = 1)
      
      x<-data[complete.cases(data),"nitrate"]
      y<-data[complete.cases(data),"sulfate"]  
      x_y_cor<-cor(x,y)
      cor_vector<-rbind(cor_vector,x_y_cor)
    }
    else{
      next
    }
    
  }
  if(length(count)==0){print(count)
  }
  else{
    row.names(cor_vector)=NULL
    print(as.vector(cor_vector)) }
  
  ##fim da funcao
}
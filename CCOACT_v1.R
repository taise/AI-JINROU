CCOACT<- function(PUBLICLOG,PRIVATELOG){
  # 初期値
  yn <- 0
  
  # 自分の正体
  MYTRUTH <- as.character(PRIVATELOG$MSG1[PRIVATELOG$TYPE=="TRUTH"])
  # CCOは無し
  
  return(list(yn,MYTRUTH))
  
}

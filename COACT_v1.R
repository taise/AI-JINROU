COACT <- function(PUBLICLOG,PRIVATELOG){
  # 初期値
  yn <- 0
  
  # 自分の正体を取得
  MYTRUTH <- as.character(PRIVATELOG$MSG1[PRIVATELOG$TYPE=="TRUTH"])
  # 占い師と霊媒師は即時CO, 人外は沈黙
  if (MYTRUTH %in% c("占い師","霊媒師")){
    yn <- 1
  }
  
  return(list(yn,MYTRUTH))
  
}

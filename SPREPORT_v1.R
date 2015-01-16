SPREPORT <- function(PUBLICLOG,PRIVATELOG){
  
  # 自分の正体を取得
  MYTRUTH <- as.character(PRIVATELOG$MSG1[PRIVATELOG$TYPE=="TRUTH"])
  
  # 占い師
  if (MYTRUTH == "占い師"){
    spr1 <- PRIVATELOG$MSG1[PRIVATELOG$TYPE=="URANAI" & PRIVATELOG$day==(max(PUBLICLOG$day)-1)]
    spr2 <- PRIVATELOG$MSG2[PRIVATELOG$TYPE=="URANAI" & PRIVATELOG$day==(max(PUBLICLOG$day)-1)]
  } else if (MYTRUTH == "霊媒師"){
    spr1 <- PRIVATELOG$MSG1[PRIVATELOG$TYPE=="REIBAI" & PRIVATELOG$day==(max(PUBLICLOG$day)-1)]
    spr2 <- PRIVATELOG$MSG2[PRIVATELOG$TYPE=="REIBAI" & PRIVATELOG$day==(max(PUBLICLOG$day)-1)]
  }
  
  return(list(as.character(spr1),as.character(spr2)))
  
}

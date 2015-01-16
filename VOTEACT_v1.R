VOTEACT<- function(PUBLICLOG,PRIVATELOG){
  
  # 自分の名前を取得
  MYNAME <- as.character(PRIVATELOG$MSG2[PRIVATELOG$TYPE=="TRUTH"])
  # 自分の正体を取得
  MYTRUTH <- as.character(PRIVATELOG$MSG1[PRIVATELOG$TYPE=="TRUTH"])
  # 参加者の名前を取得
  TSURI <- as.character(PUBLICLOG$MSG2[PUBLICLOG$TYPE=="SEAT"])
  # 死者のリストを取得
  DEAD <- unique(as.character(PUBLICLOG$MSG2[as.character(PUBLICLOG$TYPE) %in% c("DEAD","EXECUTED")]))
  # 白出し
  WHITE <- unique(as.character(PUBLICLOG$MSG2[
    (as.character(PUBLICLOG$TYPE) == "SPREPORT") & (as.character(PUBLICLOG$MSG1) == "人間")
      ]))
  # 黒出し
  BLACK <- unique(as.character(PUBLICLOG$MSG2[
    (as.character(PUBLICLOG$TYPE) == "SPREPORT") & (as.character(PUBLICLOG$MSG1) == "人狼")
    ]))
  # 初期値：サイレントエラー回避用(自分に投票)
  vote <- MYNAME
  
  # 自分は外す
  TSURI <- setdiff(TSURI,MYNAME)
  # 死者を外す
  TSURI <- setdiff(TSURI,DEAD)
  # 黒出しされている人がいればその人に投票する
  # 白出しされている人がいればその人は外す
  if (length(intersect(BLACK,TSURI))>0){
    TSURI2 <- intersect(BLACK,TSURI)
  } else {
    TSURI2 <- setdiff(TSURI,WHITE)
  }
  
  # 占い師COと霊媒師COは外す
  TSURI3 <- setdiff(TSURI2,as.character(PUBLICLOG$NAME[PUBLICLOG$TYPE=="CO"]))
  
  # 自分が人狼の場合、仲間の人狼は外す
  if (MYTRUTH=="人狼"){
    TSURI4 <- setdiff(TSURI3,PRIVATELOG$MSG2[PRIVATELOG$TYPE=="JINROU"])
  } else {
    TSURI4 <- TSURI3
  }
  
  # ランダムに選択
  if (length(TSURI4) > 0){
    vote <- sample(TSURI4,size=1)
  } else if (length(TSURI3) > 0){
    vote <- sample(TSURI3,size=1)
  } else if (length(TSURI2) > 0){
    vote <- sample(TSURI2,size=1)
  } else if (length(TSURI) > 0){
    vote <- sample(TSURI,size=1)
  }
  
  return(vote)
}

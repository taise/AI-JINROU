NIGHTACT<- function(PUBLICLOG,PRIVATELOG){
  
  # 自分の名前を取得
  MYNAME <- as.character(PRIVATELOG$MSG2[PRIVATELOG$TYPE=="TRUTH"])
  # 自分の正体を取得
  MYTRUTH <- as.character(PRIVATELOG$MSG1[PRIVATELOG$TYPE=="TRUTH"])
  # 参加者の名前を取得
  NIGHT <- as.character(PUBLICLOG$MSG2[PUBLICLOG$TYPE=="SEAT"])
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
  
  # 初期値
  night <- MYNAME
  PROB <- rep(1,length=length(NIGHT))
  
  # 占い師
  if (MYTRUTH=="占い師"){
    # 自分は外す
    PROB[NIGHT==MYNAME] <- 0
    # 死者を外す
    PROB[NIGHT %in% DEAD] <- 0
    # 黒出しされている人がいればその人は外す
    PROB[NIGHT %in% BLACK] <- 0
    # 白出しされている人がいればその人は外す
    PROB[NIGHT %in% WHITE] <- 0
    # 占い師COと霊媒師COは外す
    PROB[NIGHT %in% as.character(PUBLICLOG$NAME[PUBLICLOG$TYPE=="CO"])] <- 0
  } else if (MYTRUTH=="騎士"){
    # 白出しされている人がいればその人は優先
    PROB[NIGHT %in% WHITE] <- 5
    # 占い師COと霊媒師COは優先
    PROB[NIGHT %in% as.character(PUBLICLOG$NAME[PUBLICLOG$TYPE=="CO"])] <- 10
    # 自分は外す
    PROB[NIGHT==MYNAME] <- 0
    # 死者を外す
    PROB[NIGHT %in% DEAD] <- 0
    # 黒出しされている人がいればその人は外す
    PROB[NIGHT %in% BLACK] <- 0
  }　else if (MYTRUTH=="人狼"){
    # 白出しされている人がいればその人は優先
    PROB[NIGHT %in% WHITE] <- 5
    # 占い師COと霊媒師COは優先
    PROB[NIGHT %in% as.character(PUBLICLOG$NAME[PUBLICLOG$TYPE=="CO"])] <- 10
    # 黒出しされている人がいればその人は外す
    PROB[NIGHT %in% BLACK] <- 0
    # 人狼を外す
    PROB[NIGHT %in% as.character(PRIVATELOG$MSG2[PRIVATELOG$TYPE=="JINROU"])] <- 0
    # 死者を外す
    PROB[NIGHT %in% DEAD] <- 0
  }
  
  
  # ランダムに選択
  night <- sample(NIGHT,size=1,prob=PROB)
  return(night)
}

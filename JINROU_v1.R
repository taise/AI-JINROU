# GAMEMASTER(MAIN)
# 将棋棋士の人狼準拠
# 11人村

JINROU <- function(seed=123456,open=0,YOURNAME=""){
  
  set.seed(seed)
  # YAKU
  YAKU <- c("人狼","人狼","多重人格","占い師","霊媒師","騎士",rep("市民",length=5))
  N <- length(YAKU)
  RAND1 <- runif(N)
  TRUTH <- YAKU[order(RAND1)]
  
  # NPC NAME
  NAME <- paste("ゴノレゴ",seq(N),sep="")
  
  # KAKE
  KAKE <- 0
  
  # 名前の入力
  if (YOURNAME==""){
    YOURNAME <- readline("名前を入力してください:")
  }
  # 終了判定関数
  ENDJINROU <- function(PUBLIC,PRIVATE){
    COUNT <- merge(
      subset(PUBLIC,TYPE %in% c("DEAD","EXECUTED"),select=c(MSG2,TYPE)),
      subset(PRIVATE,TYPE=="TRUTH",select=c(TYPE,MSG1,MSG2)),
      by="MSG2",all=T)
    MURABITO <- sum(is.na(COUNT$TYPE.x))
    JINROU <- sum(is.na(COUNT$TYPE.x) & (COUNT$MSG1=="人狼"))
    TAJU <- sum(is.na(COUNT$TYPE.x) & (COUNT$MSG1=="多重人格"))
    
    if (JINROU == 0){
      hantei <- 1
    } else if (((JINROU+TAJU) > (MURABITO/2)) | (JINROU >= (MURABITO/2))){
      hantei <- 2
    } else {
      hantei <- 0
    }
    return(hantei)
  }
  
  # NAME
  NAME[1] <- YOURNAME
  
  # DEAD or ALIVE
  STATUS <- rep("ALIVE",length=N)
  
  # 役職欠け
  RAND2 <- runif(N)
  if (KAKE == 1){
    TRUTH[which.max(RAND2 * (TRUTH != "人狼"))] <- "市民"
  }
  # seat order
  # SEAT[i] == j means ID j is sitting on seat i
  # GETSEAT[i] == j means ID i is sitting on seat j
  RAND3 <- runif(N)
  SEAT <- order(RAND3)
  GETSEAT <- order(SEAT)
  
  PUBLIC <- data.frame(
    NAME = as.character("MASTER"),
    TYPE = as.character("TIME"),
    MSG1 = as.character("***GAME START***"),
    MSG2 = as.character(""),
    day = 0
  )
  
  # TRUTH
  PRIVATE <- data.frame(
    TARGET = as.character(NAME),
    NAME = as.character("SYSTEM"),
    TYPE = as.character("TRUTH"),
    MSG1 = as.character(TRUTH),
    MSG2 = as.character(NAME),
    day = 0
  )
  
  # SHARE JINROU
  for (i in seq(N)[TRUTH == "人狼"]){
    sys <- data.frame(
      TARGET = as.character(NAME[i]),
      NAME = as.character("SYSTEM"),
      TYPE = as.character("JINROU"),
      MSG1 = as.character("人狼"),
      MSG2 = as.character(NAME[seq(N)[TRUTH == "人狼"]]),
      day = 0
    )
    PRIVATE <- rbind(PRIVATE,sys)
  }
  
  cat(paste("あなたの役職は",TRUTH[1],"です。",sep=""))
  if (TRUTH[1]=="人狼"){
    cat(paste("仲間の人狼は",NAME[TRUTH=="人狼"][-1],"さんです。"))
  }
  readline("確認したらEnterを押してください:")
  
  rm(TRUTH)
  
  # CO object
  CO <- rep("GLAY",length=length(YAKU))
  
  
  # 座席
  tmp <- data.frame(
    NAME = as.character("MASTER"),
    TYPE = as.character("SEAT"),
    MSG1 = as.character(seq(N)),
    MSG2 = as.character(NAME[SEAT[seq(N)]]),
    day = 0
  )
  PUBLIC <- rbind(PUBLIC,tmp)
  cat("座席は順に:\n")
  cat(paste(seq(N),":",NAME[SEAT[seq(N)]],sep=""))
  cat("\n")
  
  # GAME START
  daynow <- 0
  lastdead <- N
  
  on <- 1
  
  #########################
  # LOOP                  #
  #########################
  repeat{
    daynow <- daynow + 1
    
    tmp <- data.frame(
      NAME = as.character("MASTER"),
      TYPE = as.character("TIME"),
      MSG1 = as.character("***NEW MORNING***"),
      MSG2 = as.character(""),
      day = daynow
    )
    PUBLIC <- rbind(PUBLIC,tmp)
    cat("MASTER:恐ろしい夜が明け、朝が来ました。昨晩の犠牲者は・・・\n")
    Sys.sleep(3)
    dead <- as.character(PUBLIC$MSG2[as.character(PUBLIC$TYPE) == "DEAD" & PUBLIC$day == (daynow-1)])
    if (length(dead) > 0){
      cat(paste("MASTER:",dead,"さんでした\n",sep=""))
    } else {
      cat(paste("MASTER:いませんでした\n",sep=""))
    }
    
    # 終了判定
    if (ENDJINROU(PUBLIC,PRIVATE) == 1){
      cat("市民陣営の勝利です！！\n")
      break
    } else if (ENDJINROU(PUBLIC,PRIVATE) == 2){
      cat("人狼陣営の勝利です！！\n")
      break
    }
    # REPORT
    if (daynow > 1){
      # special power report
      for (i in sample(seq(N))){
        if ((CO[i] != "GLAY") & (STATUS[i] == "ALIVE")){
          spr <- SPREPORT(PUBLIC,PRIVATE[PRIVATE$TARGET==NAME[i],-1])
          tmp <- data.frame(
            NAME = as.character(NAME[i]),
            TYPE = as.character("SPREPORT"),
            MSG1 = spr[[1]],
            MSG2 = spr[[2]],
            day = daynow
          )
          PUBLIC <- rbind(PUBLIC,tmp)
          cat(paste(NAME[i],":",spr[[2]],"は",spr[[1]],"でした\n",sep=""))
          Sys.sleep(1)
        }
      }
    }
    
    
    # CO and CCO
    for (i in sample(seq(N))){
      if ((CO[i] == "GLAY") & (STATUS[i] == "ALIVE")){
        co <- COACT(PUBLIC,PRIVATE[PRIVATE$TARGET==NAME[i],-1])
        if (co[[1]] == 1){
          tmp <- data.frame(
            NAME = as.character(NAME[i]),
            TYPE = as.character("CO"),
            MSG1 = co[[2]],
            MSG2 = as.character(""),
            day = daynow
          )
          PUBLIC <- rbind(PUBLIC,tmp)
          cat(paste(NAME[i],":私は",co[[2]],"です。\n",sep=""))
          Sys.sleep(1)
          CO[i] <- co[[2]]
          
          for (j in sample(seq(N))){
            if ((CO[j] == "GLAY") & (STATUS[j] == "ALIVE")){
              cco <- CCOACT(PUBLIC,PRIVATE[PRIVATE$TARGET==NAME[j],-1])
              if (cco[[1]] == 1){
                tmp <- data.frame(
                  NAME = as.character(NAME[j]),
                  TYPE = as.character("CCO"),
                  MSG1 = cco[[2]],
                  MSG2 = as.character(""),
                  day = daynow
                )
                PUBLIC <- rbind(PUBLIC,tmp)
                cat(paste(NAME[j],":私も",cco[[2]],"です。\n",sep=""))
                Sys.sleep(1)
                CO[j] <- cco[[2]]
              }
            }
          }
        }
      }
    }
    
    
    # VOTE
    VOTEVEC <- rep(0,length=N)
    
    for (i in seq(N)){
      j <- SEAT[(lastdead + i - 1) %% N + 1]
      if (STATUS[j] == "ALIVE"){
        if (j==1){
          for (loop in seq(5)){
            cat("投票先を番号で選択してください\n")
            cat(paste(seq(N)[STATUS == "ALIVE"][-1],":",NAME[STATUS == "ALIVE"][-1],sep=""))
            cat("\n")
            votechar <- readline("選択:")
            if (votechar %in% as.character(seq(N)[STATUS == "ALIVE"][-1])){
              vote <- NAME[as.numeric(votechar)]
              break
            }
            vote <- YOURNAME
          }
        } else {
          vote <- VOTEACT(PUBLIC,PRIVATE[PRIVATE$TARGET==NAME[j],-1])
          cat(paste(NAME[j],":私は",vote,"に投票します。\n",sep=""))
          Sys.sleep(1)
        }
        VOTEVEC[j] <- seq(N)[NAME==vote]
        tmp <- data.frame(
          NAME = as.character(NAME[j]),
          TYPE = as.character("VOTE"),
          MSG1 = as.character(""),
          MSG2 = vote,
          day = daynow
        )
        PUBLIC <- rbind(PUBLIC,tmp)
      }
    }
    
    # TABLE
    MAXVOTE <- max(as.integer(table(VOTEVEC[VOTEVEC!=0])))
    MAXTARG <- as.integer(names(table(VOTEVEC[VOTEVEC!=0]))[as.integer(table(VOTEVEC[VOTEVEC!=0])) == MAXVOTE])
    
    if (length(MAXTARG) == 1){
      # excecution
      STATUS[MAXTARG] <- "DEAD"
      lastdead <- SEAT[MAXTARG]
      tmp <- data.frame(
        NAME = as.character("MASTER"),
        TYPE = as.character("EXECUTED"),
        MSG1 = as.character(""),
        MSG2 = as.character(NAME[MAXTARG]),
        day = daynow
      )
      PUBLIC <- rbind(PUBLIC,tmp)
      cat(paste("MASTER:投票の結果、",NAME[MAXTARG],"さんは処刑されます。\n",sep=""))
      Sys.sleep(1)
    }
    
    if (length(MAXTARG) > 1){
      # REVOTE
      REVOTEVEC <- rep(0,length=N)
      tmp <- data.frame(
        NAME = as.character("MASTER"),
        TYPE = as.character("REVOTE"),
        MSG1 = as.character(""),
        MSG2 = as.character(NAME[MAXTARG]),
        day = daynow
      )
      PUBLIC <- rbind(PUBLIC,tmp)
      cat("MASTER:投票の結果、")
      cat(paste(NAME[MAXTARG],"さん",sep=""))
      cat("で再投票になります。\n",sep="")
      Sys.sleep(1)
      
      for (i in seq(N)){
        j <- SEAT[(lastdead + i - 1) %% N + 1]
        if ((STATUS[j] == "ALIVE") & !(j %in% MAXTARG)){
          if (j==1){
            for (loop in seq(5)){
              cat("再投票先を番号で選択してください\n")
              cat(paste(seq(N)[MAXTARG],":",NAME[MAXTARG],sep=""))
              cat("\n")
              votechar <- readline("選択:")
              if (votechar %in% as.character(seq(N)[MAXTARG])){
                revote <- NAME[as.numeric(votechar)]
                break
              }
              revote <- YOURNAME
            }
          } else {
            revote <- REVOTEACT(PUBLIC,PRIVATE[PRIVATE$TARGET==NAME[j],-1])
            cat(paste(NAME[j],":私は",revote,"に投票します。\n",sep=""))
            Sys.sleep(1)
          }
          REVOTEVEC[j] <- seq(N)[NAME==revote]
          tmp <- data.frame(
            NAME = as.character(NAME[j]),
            TYPE = as.character("REVOTE"),
            MSG1 = as.character(""),
            MSG2 = revote,
            day = daynow
          )
          PUBLIC <- rbind(PUBLIC,tmp)
        }
      }
      
      # TABLE
      if (sum(REVOTEVEC) > 0){
        MAXREVOTE <- max(as.integer(table(REVOTEVEC[REVOTEVEC!=0])))
        MAXRETARG <- as.integer(names(table(REVOTEVEC[REVOTEVEC!=0]))[as.integer(table(REVOTEVEC[REVOTEVEC!=0])) == MAXREVOTE])
      } else {
        MAXRETARG <- MAXTARG
      }
      if (length(MAXRETARG) == 1){
        # excecution
        STATUS[MAXRETARG] <- "DEAD"
        lastdead <- GETSEAT[MAXRETARG]
        tmp <- data.frame(
          NAME = as.character("MASTER"),
          TYPE = as.character("EXECUTED"),
          MSG1 = as.character(""),
          MSG2 = as.character(NAME[MAXRETARG]),
          day = daynow
        )
        PUBLIC <- rbind(PUBLIC,tmp)
        cat(paste("MASTER:以上の結果、",NAME[MAXRETARG],"さんは処刑されます。\n",sep=""))
      } else {
        # ties broken randomly
        EXECSAMP <- sample(MAXRETARG,size=1)
        STATUS[EXECSAMP] <- "DEAD"
        lastdead <- GETSEAT[EXECSAMP]
        tmp <- data.frame(
          NAME = as.character("MASTER"),
          TYPE = as.character("EXECUTED"),
          MSG1 = as.character(""),
          MSG2 = as.character(NAME[EXECSAMP]),
          day = daynow
        )
        PUBLIC <- rbind(PUBLIC,tmp)
        cat(paste("MASTER:抽選の結果、",NAME[EXECSAMP],"さんは処刑されます。\n",sep=""))
      }
    }
    # 終了判定
    if (ENDJINROU(PUBLIC,PRIVATE) == 1){
      cat("市民陣営の勝利です！！\n")
      break
    } else if (ENDJINROU(PUBLIC,PRIVATE) == 2){
      cat("人狼陣営の勝利です！！\n")
      break
    }
    
    cat("MASTER:容疑者を処刑したにも関わらず、恐ろしい夜がやってきます・・・\n")
    Sys.sleep(3)
    # NIGHT ACTION
    
    WOLFVOTE <- rep(0,length=N)
    
    for (i in seq(N)){
      if (STATUS[i] == "ALIVE"){
        # 特殊能力ごとに能力発揮
        if (as.character(PRIVATE$MSG1[
          PRIVATE$TYPE=="TRUTH" & PRIVATE$TARGET == NAME[i]
          ]) == "占い師"){
          if (i == 1){
            for (loop in seq(5)){
              cat("占い先を番号で選択してください\n")
              cat(paste(seq(N)[STATUS == "ALIVE"][-1],":",NAME[STATUS == "ALIVE"][-1],sep=""))
              cat("\n")
              uranaichar <- readline("選択:")
              if (uranaichar %in% as.character(seq(N)[STATUS == "ALIVE"][-1])){
                uranai <- NAME[as.numeric(uranaichar)]
                break
              }
              uranai <- YOURNAME
            }
          } else {
            uranai <- NIGHTACT(PUBLIC,PRIVATE[PRIVATE$TARGET==NAME[i],-1])
          }
          if (as.character(PRIVATE$MSG1[
            PRIVATE$TYPE=="TRUTH" & PRIVATE$TARGET == uranai
            ]) == "人狼"){
            uranai_res <- "人狼"
          } else {
            uranai_res <- "人間"
          }
          sys <- data.frame(
            TARGET = as.character(NAME[i]),
            NAME = as.character("SYSTEM"),
            TYPE = as.character("URANAI"),
            MSG1 = as.character(uranai_res),
            MSG2 = as.character(uranai),
            day = daynow
          )
          PRIVATE <- rbind(PRIVATE,sys)
        } else if (as.character(PRIVATE$MSG1[
          PRIVATE$TYPE=="TRUTH" & PRIVATE$TARGET == NAME[i]
          ]) == "霊媒師"){
          # 当日処刑された者がいれば霊媒結果を取得
          exec <- as.character(PUBLIC$MSG2[as.character(PUBLIC$TYPE) == "EXECUTED" & PUBLIC$day == daynow])
          if (length(exec) == 1){
            if (as.character(PRIVATE$MSG1[
              PRIVATE$TYPE=="TRUTH" & PRIVATE$TARGET == exec
              ]) == "人狼"){
              reibai_res <- "人狼"
            } else {
              reibai_res <- "人間"
            }
            sys <- data.frame(
              TARGET = as.character(NAME[i]),
              NAME = as.character("SYSTEM"),
              TYPE = as.character("REIBAI"),
              MSG1 = as.character(reibai_res),
              MSG2 = as.character(exec),
              day = daynow
            )
            PRIVATE <- rbind(PRIVATE,sys)      
          }
          if (i==1){
            cat(paste(exec,"は",reibai_res,"でした。\n"))
          }
        } else if (as.character(PRIVATE$MSG1[
          PRIVATE$TYPE=="TRUTH" & PRIVATE$TARGET == NAME[i]
          ]) == "騎士"){
          # 護衛対象を指定
          if (i==1){
            for (loop in seq(5)){
              cat("護衛先を番号で選択してください\n")
              cat(paste(seq(N)[STATUS == "ALIVE"][-1],":",NAME[STATUS == "ALIVE"][-1],sep=""))
              cat("\n")
              guardchar <- readline("選択:")
              if (guardchar %in% as.character(seq(N)[STATUS == "ALIVE"][-1])){
                guard <- NAME[as.numeric(guardchar)]
                break
              }
              guard <- YOURNAME
            }
          } else {
            guard <- NIGHTACT(PUBLIC,PRIVATE[PRIVATE$TARGET==NAME[i],-1])
          }
          sys <- data.frame(
            TARGET = as.character(NAME[i]),
            NAME = as.character("SYSTEM"),
            TYPE = as.character("GUARD"),
            MSG1 = as.character(""),
            MSG2 = as.character(guard),
            day = daynow
          )
          PRIVATE <- rbind(PRIVATE,sys)  
        } else if (as.character(PRIVATE$MSG1[
          PRIVATE$TYPE=="TRUTH" & PRIVATE$TARGET == NAME[i]
          ]) == "人狼"){
          # 襲撃対象を指定
          JINROUS <- PRIVATE$MSG2[(PRIVATE$TYPE=="TRUTH") & (PRIVATE$MSG1=="人狼")]
          if (i == 1){
            for (loop in seq(5)){
              cat("襲撃先を番号で選択してください\n")
              cat(paste(seq(N)[(STATUS == "ALIVE") & !(NAME %in% JINROUS)],":",NAME[(STATUS == "ALIVE") & !(NAME %in% JINROUS)],sep=""))
              cat("\n")
              targetchar <- readline("選択:")
              if (targetchar %in% as.character(seq(N)[(STATUS == "ALIVE") & !(NAME %in% JINROUS)])){
                target <- NAME[as.numeric(targetchar)]
                break
              }
              target <- YOURNAME
            }
          } else {
            if (WOLFVOTE[1] != 0){
              target <- NAME[WOLFVOTE[1]]
            } else {
              target <- NIGHTACT(PUBLIC,PRIVATE[PRIVATE$TARGET==NAME[i],-1])
            }
          }
          WOLFVOTE[i] <- seq(N)[NAME == target]
          sys <- data.frame(
            TARGET = as.character(NAME[i]),
            NAME = as.character("SYSTEM"),
            TYPE = as.character("TARGET"),
            MSG1 = as.character(""),
            MSG2 = as.character(target),
            day = daynow
          )
          PRIVATE <- rbind(PRIVATE,sys)
        }
      }
    }
    
    
    # 襲撃処理
    # TABLE
    MAXWOLFVOTE <- max(as.integer(table(WOLFVOTE[WOLFVOTE!=0])))
    MAXWOLFTARG <- as.integer(names(table(WOLFVOTE[WOLFVOTE!=0]))[as.integer(table(WOLFVOTE[WOLFVOTE!=0])) == MAXWOLFVOTE])
    
    
    # 襲撃対象の決定
    if (length(MAXWOLFTARG) == 1){
      WOLFTARGET <- MAXWOLFTARG
    } else {
      # ties broken randomly
      WOLFTARGET <- sample(MAXWOLFTARG,size=1)
    }
    
    # 襲撃を判定
    GUARDTARGET <- as.character(PRIVATE$MSG2[
      PRIVATE$TYPE=="GUARD" & PRIVATE$day == daynow
      ])
    
    if (length(GUARDTARGET) == 0){
      GUARDTARGET <- as.character("")
    }
    
    if (GUARDTARGET != NAME[WOLFTARGET]){
      # 死亡判定
      STATUS[WOLFTARGET] <- "DEAD"
      lastdead <- GETSEAT[WOLFTARGET]
      tmp <- data.frame(
        NAME = as.character("MASTER"),
        TYPE = as.character("DEAD"),
        MSG1 = as.character(""),
        MSG2 = as.character(NAME[WOLFTARGET]),
        day = daynow
      )
      PUBLIC <- rbind(PUBLIC,tmp)
    }
  }
  if (open==1){
    for (i in seq(N)){
      cat(as.character(PRIVATE$MSG2[i]),"は",as.character(PRIVATE$MSG1[i]),"でした。\n")
    }
  }
}

proxyURL=curl::ie_get_proxy_for_url()
httr::with_config(httr::use_proxy(proxyURL), pacman::p_load_gh('ewenme/fplr'))
httr::with_config(httr::use_proxy(proxyURL), pacman::p_load_gh('ewenme/fplinear'))

players=fplr::fpl_get_players()

setDT(players)
players[,VAPM:=(points_per_game-2)/now_cost]

players[position=="Goalkeeper",VAPEM:=(points_per_game-2)/(now_cost-4)]
players[position=="Defender",VAPEM:=(points_per_game-2)/(now_cost-4)]
players[position=="Midfielder",VAPEM:=(points_per_game-2)/(now_cost-4.5)]
players[position=="Forward",VAPEM:=(points_per_game-2)/(now_cost-4.5)]

players[position=="Goalkeeper",PPEM:=points_per_game/(now_cost-4)]
players[position=="Defender",PPEM:=points_per_game/(now_cost-4)]
players[position=="Midfielder",PPEM:=points_per_game/(now_cost-4.5)]
players[position=="Forward",PPEM:=points_per_game/(now_cost-4.5)]

saveRDS(players,"C:/Users/p762309/Documents/FPL/players09082018.RDS")

teamPicker = function(gks,defs,mfs,fwds) {
  ret=rbind(
    gks[order(-points_per_game,total_points)][1:2,],
    defs[order(-points_per_game,total_points)][1:5,],
    mfs[order(-points_per_game,total_points)][1:5,],
    fwds[order(-points_per_game,total_points)][1:3,]
  )
  ret[,.(id,web_name,position,now_cost,total_points,points_per_game,minutes,team_name,selected_by_percent,bps,influence,creativity,threat)]
}

players2=players[,.(id,web_name,position,now_cost,total_points,points_per_game,PPEM,VAPM,VAPEM,
                    minutes,team_name,selected_by_percent,bps,influence,creativity,threat)]

# let player i = x_i
# let price i  = p_i
# let ppg 1    = ppg_i
# Set up problem: maximise ppg_i*x_i subject to:
# x_i      <= 1
# x_df     <= 2
# x_mf     <= 5
# x_fd     <= 5
# x_gk     <= 3
# sum(x_i) <= 15
# p_i*x_i  <= 100
teamPickerOpt = function(playerData,minMinutes=0,metric="points") {
  players=playerData[minutes>minMinutes,]
  players=players[order(position)]
  
  totCount=nrow(players)
  defCount=nrow(players[position=="Defender"])
  fwdCount=nrow(players[position=="Forward"])
  gkCount=nrow(players[position=="Goalkeeper"])
  mfCount=nrow(players[position=="Midfielder"])
  
  defCnstrnt=c(rep(1,defCount),rep(0,fwdCount),rep(0,gkCount),rep(0,mfCount))
  fwdCnstrnt=c(rep(0,defCount),rep(1,fwdCount),rep(0,gkCount),rep(0,mfCount))
  gkCnstrnt=c(rep(0,defCount),rep(0,fwdCount),rep(1,gkCount),rep(0,mfCount))
  mfCnstrnt=c(rep(0,defCount),rep(0,fwdCount),rep(0,gkCount),rep(1,mfCount))
  totCnstrnt=rep(1,totCount)
  priceCnstrnt = players$now_cost
  indvdlCnstrnt = diag(totCount)
  
  f.obj = if (metric=="ppg") {
    players$points_per_game
  } else if(metric=="points") {
    players$total_points
  } else if(metric=="vapm") {
    players$VAPM
  } else if(metric=="ppem") {
    players$PPEM
  } else if(metric=="vapem") {
    players$VAPEM
  } else {
    stop()
  }
  
  f.con = rbind(defCnstrnt,fwdCnstrnt,gkCnstrnt,mfCnstrnt,totCnstrnt,priceCnstrnt,indvdlCnstrnt)
  f.dir = c(rep("=",5),rep("<=",totCount+1))
  f.rhs = c(5,3,2,5,15,100,rep(1,totCount))
  
  sol=lp("max",f.obj,f.con,f.dir,f.rhs,all.int = TRUE)
  players[sol$solution==1,]
}

# gkRng(1,1)
# defRng c(3,5)
# mfRng c(2,5)
# fwdRng (1,3)
#

teamPickerEleven = function(playerData,moneySpent = 80, minMinutes=0,metric="points") {
  players=playerData[minutes>minMinutes,]
  players=players[order(position)]
  
  totCount=nrow(players)
  defCount=nrow(players[position=="Defender"])
  fwdCount=nrow(players[position=="Forward"])
  gkCount=nrow(players[position=="Goalkeeper"])
  mfCount=nrow(players[position=="Midfielder"])
  
  defCnstrnt=c(rep(1,defCount),rep(0,fwdCount),rep(0,gkCount),rep(0,mfCount))
  fwdCnstrnt=c(rep(0,defCount),rep(1,fwdCount),rep(0,gkCount),rep(0,mfCount))
  gkCnstrnt=c(rep(0,defCount),rep(0,fwdCount),rep(1,gkCount),rep(0,mfCount))
  mfCnstrnt=c(rep(0,defCount),rep(0,fwdCount),rep(0,gkCount),rep(1,mfCount))
  totCnstrnt=rep(1,totCount)
  priceCnstrnt = players$now_cost
  indvdlCnstrnt = diag(totCount)
  
  f.obj = if (metric=="ppg") {
    players$points_per_game
  } else if(metric=="points") {
    players$total_points
  } else if(metric=="vapm") {
    players$VAPM
  } else if(metric=="ppem") {
    players$PPEM
  } else if(metric=="vapem") {
    players$VAPEM
  } else {
    stop()
  }
  f.con = rbind(defCnstrnt,defCnstrnt,fwdCnstrnt,fwdCnstrnt,gkCnstrnt,mfCnstrnt,mfCnstrnt,totCnstrnt,priceCnstrnt,indvdlCnstrnt)
  f.dir = c(">=","<=",">=","<=","=",">=","<=","=","<=",rep("<=",totCount))
  f.rhs = c(3,5,1,3,1,2,5,11,moneySpent,rep(1,totCount))
  
  sol=lp("max",f.obj,f.con,f.dir,f.rhs,all.int = TRUE)
  players[sol$solution==1,]
}

myTeam=players2[web_name%in%c("Cech","Smalling","Young","Aurier","De Bruyne","Son","Martial","Groß","Lallana","Aubameyang","Vardy","Ryan","Zaha","Maguire","Danilo"),]

myTeam2=players2[web_name%in%c("Fabianski","Ryan","Young","Azpiculeta","Zanka","Davies","Mendy","Groß","Salah","Fernandinho","Milivojevic","Lingard","Zaha","Vardy","Arnautovic"),]

library(lpSolve)
library(jsonlite)
library(readr)

# fpl bootstrap static
fpl_static <- "https://fantasy.premierleague.com/drf/bootstrap-static"
# fpl fixtures
fpl_fixtures <- "https://fantasy.premierleague.com/drf/fixtures/"
# fpl player summary
fpl_player_summary <- "https://fantasy.premierleague.com/drf/element-summary/"
# fpl user competition entry
fpl_user_entry <- "https://fantasy.premierleague.com/drf/entry/"

jsonlite::read_json(fpl_user_entry, simplifyVector = TRUE)

path = "C:/Users/p762309/Fantasy-Premier-League/data/2017-18/players"
path2 = "C:/Users/p762309/Fantasy-Premier-League/data/2017-18/player_idlist.csv"
playerIds=read_csv(path2)

fileList=list.files(path)
finalDS=NULL
for (file in fileList) {
  temp=read_csv(paste0(path,'/',file,'/gw.csv'))
  finalDS=rbind(finalDS,temp)
}

finalDS=merge(finalDS,playerIds,by.x = "element",by.y = "id")

saveRDS(finalDS,paste0(path,'/allData.RDS'))

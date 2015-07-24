library(plyr)
library(dplyr)
library(RCurl)
library(jsonlite)
library(curl)
setwd("~/Documents/Pinball/UPWC/Data")
source("upwc_keys.R") # this file records the api.key variable. you must obtain an apikey from IFPA!

expiration_time<-364 # number of days until the title expires

# This function builds THE ENTIRE UPWC history. It takes several minutes to run.
buildEntireHistory<-function(){
  api.calls<<-0  
  startHistory()
  repeat{
    result<-updateNextChampion()
    print(result)
    if (result == "Up to date"){
      break
    }
  }
}

startHistory<-function(){
  # construct initial current_champ_private file. 
  private_player_info<-list(player_id=4543,date_won="1980-10-26")
  private_df<-data.frame(lapply(private_player_info,function(x) t(data.frame(x))))
  write.csv(private_df,'current_champ_private.csv',row.names=FALSE)
  #build data frame of all tournaments. NOTE. This file realistically only needs to be generated once every year. once built, it's good to go.:)
  if(!(file.exists("tournament_frame.csv"))){
    past_tournaments<<-constructTournamentFrame()
  }else{
    past_tournaments<<-read.csv("tournament_frame.csv",na.strings = c('None'),stringsAsFactors = F,colClasses = c(NA,NA,NA,"Date",NA,NA,NA,NA,NA)) 
  }
  past_tournaments$event_date<-as.Date(past_tournaments$event_date)
  # records the current_champ table
  ifpaRecordChampInfo(api.key = api.key,player.number = 4543,date.won = "1980-10-26")
  
  # start championdata file
  championdata<-list(Champion = "<img src='/img/US.png'> <a href = 'http://www.ifpapinball.com/player.php?p= 4543 '> Dallas Overturf </a>",
             Date = "1980-10-26",
             Tournament = "<a href = 'http://www.ifpapinball.com/view_tournament.php?t= 1 '> U.S. Open </a>",
             Defeated = "Defaulted" 
               )
  championdata<-data.frame(lapply(championdata,function(x) t(data.frame(x))))
  write.csv(championdata,'championdata.csv',row.names=FALSE) 
}

# Update UPWC. Call this to update the next champion. It automatically constructs new tables to be used. 
updateNextChampion<-function(){
  # load data
  past_tournaments<-read.csv("tournament_frame.csv",na.strings = c('None'),stringsAsFactors = F,colClasses = c(NA,NA,NA,"Date",NA,NA,NA,NA,NA))  
  championdata<-read.csv("championdata.csv",stringsAsFactors = F,na.strings = c('None'),colClasses = c(NA,"Date",NA,NA))
  current_champ<-read.csv("current_champ_private.csv", stringsAsFactors = F, colClasses = c(NA,"Date"))
  # current_champ is the player number and date won of the current champion. 
  # If there is a date and no champ, that means the champion has expired. The date is the date that the title expired

  # check that there is a recorded champion. If not, find the winner of the next available tournament
  if(is.na(current_champ$player_id)){
    # there is no champion, but there is an exp.date. find the next tournament, and the winner of that tournament. 
    next_tournament <- ifpaGetNextValidTournament(api.key = api.key,past.tournaments = past_tournaments,exp.date = current_champ$date_won)
    # current_champ$player_id <- next_tournament$winner_player_id
    tournament.id <- next_tournament$tournament_id
    tournament.date<-next_tournament$event_date 
    result<-ifpaRecordUPWCMatch(api.key = api.key,championdata = championdata, tournament.id=tournament.id, tournament.date = tournament.date)
    result<-paste("Defaulted Match:",result)
    return(result)
  }else{
  
  # get all tournaments the player has played in
  tournaments<-ifpaGetPlayerTournaments(api.key = api.key,player.number = current_champ$player_id)
  # get subset occurring after title was won
  new_tournaments<-subset(tournaments,tournaments$results.event_date > current_champ$date_won)
  # check the difference between date won and most recent tournament is no more than expiration_time days
  valid_tournament_dates<-((min(new_tournaments$results.event_date) - current_champ$date_won) < expiration_time)
  # check that there are new tournaments
  new_tournaments_found<-(dim(new_tournaments)[1] > 0)
  # check if there are new tournaments
  if(new_tournaments_found){
    # check that the tournaments are within one year. otherwise the championship is forfeited
    if(!valid_tournament_dates){
      expireChampion(date.won = current_champ$date_won)
      return("No Match, Title Expired")
    }
    # get the most recent tournaments
    new_tournaments<-subset(new_tournaments,new_tournaments$results.event_date==min(new_tournaments$results.event_date))        
    # check that there is not more than 1 event for that tournament. If so, choose the one with the largest WPPR value
    if(dim(new_tournaments)[1]>1){
      new_tournaments<-ifpaGetBestTournament(api.key = api.key,tournaments = new_tournaments)
    }
    # record event_date and id
    tournament.date<-new_tournaments$results.event_date
    tournament.id<-new_tournaments$results.tournament_id
    
    result<-ifpaRecordUPWCMatch(api.key = api.key, 
                                championdata = championdata, 
                                tournament.id = tournament.id, 
                                tournament.date = tournament.date,
                                reigning.id = current_champ$player_id)
    #update_server()
    result<-paste("Title match:",result)
    return(result)
    
  }else{
    # no new tournaments were found. This means the champion is either expired, or no longer with us. 
    if((Sys.Date()-current_champ$date_won) < expiration_time){
      # the reigning champion is still valid
      return("Up to date")
    }else{
      # the reigning champion has expired. update the current_champ_private file to reflect expiration date 
      expireChampion(date.won = current_champ$date_won)
      return("Champion retired, title expired")
    }  
  }
  }
}

ifpaRecordUPWCMatch <- function(api.key,championdata,tournament.id,tournament.date,reigning.id){
  # record defeated champion
  if(missing(reigning.id)){
    reigning.champ <- "Defaulted"
  }else{
    reigning.champ <- ifpaGetPlayerInfo(api.key = api.key, player.number = reigning.id)
    reigning.champ <- paste(reigning.champ$player.first_name,reigning.champ$player.last_name)
  }
  t.results<-ifpaGetTournamentResults(api.key = api.key,tournament.key = tournament.id)
  if (is.null(t.results)){# if null, it means that a clear winner was not found
    # update the private player info.  The logic here is that the current champ remains, but the date is updated so that this event isn't considered on the next them
    current_champ<-read.csv("current_champ_private.csv", stringsAsFactors = F, colClasses = c(NA,"Date"))
    current_champ$date_won<-tournament.date
    write.csv(current_champ,'current_champ_private.csv',row.names=FALSE)
    result<-"No Clear Winner! No match recorded."
  }else{
    winner.id<-t.results$Id
    p_name<-t.results$Champion
    country_code<-t.results$Country
    tournament.name<-t.results$Tournament_Name
    if(!(reigning.champ == "Defaulted")){
      if(winner.id == reigning.id){
        reigning.champ <- "Defended"
      }
    }
    # construct row 
    img<-paste("<img src='/img/",country_code,".png'>",sep = "")
    name_url<-paste(img,"<a href = 'http://www.ifpapinball.com/player.php?p=", winner.id,"'>", p_name, "</a>")
    tournament_url<-paste("<a href = 'http://www.ifpapinball.com/view_tournament.php?t=", tournament.id,"'>", tournament.name, "</a>",sep = "")
    #new_row<-c(name_url,as.Date(tournament.date),tournament_url,reigning.champ)
    # bind to championdata
    
    new_row<-list(Champion = name_url,
                  Date = tournament.date,
                  Tournament = tournament_url,
                  Defeated = reigning.champ
    )
    new_row<-data.frame(lapply(new_row,function(x) t(data.frame(x))))
    championdata<-rbind(championdata,new_row)
    
    
    # construct Ranking table. All winners of the UPWC, rank, and the number of times they've won. 
    f<-table(championdata$Champion)
    f<-data.frame(f)
    names(f)<-c("Champion","Wins") 
    f$Rank<-rank(f$Wins,ties.method = c("min"))# count ranks
    f$Rank<-dim(f)[1]-(f$Rank-1)# invert ranks
    f<-arrange(f,-Wins)# sort the table
    f<-f[c(3,1,2)]# reorder columns
    #determine number of appearances
    f$id<-gsub(pattern = "[^0-9]",replacement = "",x = f$Champion)
    upwc.match.ids<-gsub(pattern = "[^0-9]",replacement = "",x = championdata$Tournament)
    f$Appearances<-sapply(X = f$id,
                          FUN = function(x){
                            value<-ifpaGetUPWCAppearances(api.key = api.key,player.id = x,upwc.match.ids = upwc.match.ids)
                          } )
    f$id <- NULL
    # write top champs
    write.csv(f,'top_champs.csv',row.names=FALSE)
    # write results
    write.csv(championdata,'championdata.csv',row.names=FALSE)
    # write recent resutls
    if(dim(championdata)[1] > 10){
      recent<-championdata[(dim(championdata)[1]-9):(dim(championdata)[1]),]
      recent<-arrange(recent,desc(Date))
      write.csv(recent,'recent_results.csv',row.names=FALSE)
    }  

    #record newest champ
    ifpaRecordChampInfo(api.key = api.key,player.number = winner.id, date.won = tournament.date)
    result<-"Champion Recorded"
  }
  return(result)
  #update_server()
}

# expire champion
expireChampion<-function(date.won){
  private_player_info<-list(player_id=NA,date_won=(date.won + expiration_time))
  private_df<-data.frame(lapply(private_player_info,function(x) t(data.frame(x))))
  private_df$date_won<-as.Date(private_df$date_won)
  write.csv(private_df,'current_champ_private.csv',row.names=FALSE)
}

# gets info about a player and records it
ifpaRecordChampInfo<-function(api.key,player.number,date.won){
  url<-paste("https://api.ifpapinball.com/v1/player/",player.number,"?api_key=",api.key,sep = "")
  j <- fromJSON(url)
  name<-paste(j$player$first_name,j$player$last_name)
  country_code<-j$player$country_code
  img<-paste("<img src='/img/",country_code,".png'>",sep = "")
  url<-paste(img,"<a href = 'http://www.ifpapinball.com/player.php?p=", player.number,"'>", name, "</a>")
  home_town<-paste(j$player$city,", ",j$player$state,sep = "")
  rank<-j$player_stats$current_wppr_rank
  age<-j$player$age
  
  # record csv for website
  player_info<-list(Name=url,Hometown=home_town,Age=age,IFPA_Rank=rank,Held_Since=date.won)
  df<-data.frame(lapply(player_info,function(x) t(data.frame(x))))
  write.csv(df,'current_champ.csv',row.names=FALSE)
  
  # record csv for future analysis
  private_player_info<-list(player_id=player.number,date_won=date.won)
  private_df<-data.frame(lapply(private_player_info,function(x) t(data.frame(x))))
  write.csv(private_df,'current_champ_private.csv',row.names=FALSE)
  
  api.calls<<-api.calls+1
}

# construct datafame of all tournaments. WARNING: this makes about 40 API calls in a short period of time.
constructTournamentFrame<-function(api.key,start.pos){
  i<-start.pos
  results<-ifpaGetTournaments(api.key,i)
  repeat{
    i<-i+250
    results_next<-ifpaGetTournaments(api.key,i)
    if(is.null(dim(results_next))){
      break
    }else{
    results<-rbind(results,results_next)
    }
  }
  results$event_date<-as.Date(results$event_date)
  write.csv(results,'tournament_frame.csv',row.names=FALSE)
  return(results)
}

# in the event that returns a champion times out, this function finds the next valid tournament, given expiration date
ifpaGetNextValidTournament<-function(api.key,past.tournaments,exp.date){
  tournaments <- subset(past.tournaments,past.tournaments$event_date > exp.date)
  next_available_dates <- min(tournaments$event_date)
  tournaments <- subset(past.tournaments,past.tournaments$event_date==next_available_dates)
  # check that there is not more than 1 event for that tournament. If so, choose the one with the largest WPPR value
  if(dim(tournaments)[1]>1){
    tournaments$value<-sapply(X = tournaments$tournament_id,
                              FUN = function(x){
                                value<-ifpaGetTournamentValue(api.key,x)
                              }
    )
    tournaments<-subset(tournaments,tournaments$value==max(tournaments$value))
  }
  return(tournaments)
}

# return a dataframe of 250 tournaments from a given starting point. start.pos of 1 is the most recent tournament in IFPA records 
ifpaGetTournaments<-function(api.key,start.pos){
  url<-paste("https://api.ifpapinball.com/v1/tournament/list?api_key=",api.key,"&start_pos=",start.pos,"&count=250",sep = "")
  json_data <- fromJSON(url)
  total_tournaments<-json_data$total_results
  if(start.pos>total_tournaments){
    results<-0
  }else{
  tournaments <- json_data["tournament"]
  tournaments<-data.frame(tournaments)
  names(tournaments)<-c("tournament_id","tournament_name","event_name","event_date","winner_name","winner_player_id","country_code","country_name","player_count")
  results<-tournaments
  }
  api.calls<<-api.calls+1
  return(results)
}

# given dataframe of tournament results, return the one with the highest WPPR value
ifpaGetBestTournament<-function(api.key,tournaments){
  tournaments$value<-sapply(X = tournaments$results.tournament_id,
         FUN = function(x){
           value<-ifpaGetTournamentValue(api.key,x)
           }
        )
  tournaments<-subset(tournaments,tournaments$value==max(tournaments$value))
  # check if there are still more than one tournaments
  if(dim(tournaments)[1] > 1){
    # choose a random tournament
    rand.tournament <- sample(1:dim(tournaments)[1],1)
    tournaments <- tournaments[rand.tournament,]
  }
  return(tournaments)
}

# return WPPR value of tournament
ifpaGetTournamentValue<-function(api.key,tournament.number){
  url<-paste("https://api.ifpapinball.com/v1/tournament/",tournament.number,"/results?api_key=",api.key,sep = "")
  json_data <- fromJSON(url)
  api.calls<<-api.calls+1
  return(as.numeric(json_data$tournament$event_value))
}

# return tournament results
ifpaGetTournamentResults<-function(api.key,tournament.key){
  url<-paste("https://api.ifpapinball.com/v1/tournament/",tournament.key,"?api_key=",api.key,sep = "")
  json_data <- fromJSON(url)
  api.calls<<-api.calls+1
  #determine if there was a winner recorded
  if (is.null(json_data$tournament$event)){
    results<-NULL
  }else{
    id<-json_data$tournament$events$winner_player_id
    name<-paste(json_data$tournament$events$winner_first_name,json_data$tournament$events$winner_last_name)
    date<-json_data$tournament$events$event_date
    player_info<-ifpaGetPlayerInfo(api.key,id)
    country_code<-player_info$player.country_code
    tournament_name<-json_data$tournament$tournament_name
    results<-list(Champion = name,Id = id, Date = date, Country = country_code, Tournament_Name = tournament_name)
  }
  return(results)
}

# returns a given players entire results history
ifpaGetPlayerTournaments<-function(api.key,player.number){
  url<-paste("https://api.ifpapinball.com/v1/player/",player.number,"/results?api_key=",api.key, sep = "")
  json_data <- fromJSON(url)
  tournaments <- json_data["results"]
  tournaments <-data.frame(tournaments)
  tournaments$results.event_date <- as.Date(tournaments$results.event_date)
  api.calls<<-api.calls+1
  return(tournaments)
}

# returns info about a player
ifpaGetPlayerInfo<-function(api.key,player.number){
  url<-paste("https://api.ifpapinball.com/v1/player/",player.number,"?api_key=",api.key,sep = "")
  json_data <- fromJSON(url)
  player <- json_data["player"]
  player <-data.frame(player)
  api.calls<<-api.calls+1
  return(player)
}

# get the tournament name for a given tournament id number
ifpaGetTournamentName<-function(api.key,tournament.number){
  url<-paste("https://api.ifpapinball.com/v1/tournament/",tournament.number,"?api_key=",api.key,sep = "")
  json_data <- fromJSON(url)
  tournament_name<-json_data$tournament$tournament_name
  api.calls<<-api.calls+1
  return(tournament_name)
}

#WIP--streak analysis. computes the streak that a player goes on
doStreakAnalysis <- function(){
  championdata<-read.csv("championdata.csv",stringsAsFactors = F,na.strings = c('None'),colClasses = c(NA,"Date",NA,NA))
  streak<-c(0)
  for (i in 2:dim(championdata)[1]){
    if (championdata$Champion[i]==championdata$Champion[i-1]){
      streak<-append(streak,streak[i-1]+1)
    }else{
      streak<-append(streak,0)
    }
  }
  championdata$streak<-streak
  championdata
}

# returns the number of upwc appearanes for a player.
ifpaGetUPWCAppearances<-function(api.key,player.id,upwc.match.ids){
  tournaments <- ifpaGetPlayerTournaments(api.key = api.key,player.number = player.id)
  tournaments <- tournaments$results.tournament_id
  appearances <- sum(tournaments %in% upwc.match.ids)
  return(appearances)
}
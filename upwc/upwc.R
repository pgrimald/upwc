library(plyr)
library(dplyr)
library(RCurl)
library(jsonlite)
library(curl)
setwd("~/Documents/Pinball/UPWC/Data")
source("upwc_keys.R")

#import data-------------------------------------------------------------------------------------------------
#wppr <- read.csv('wppr_results.csv',na.strings=c('None'))
#champ_data<-read.csv('ifpa_champion_data.csv',na.strings=c('None'),stringsAsFactors=FALSE)

#convert dates
#wppr$tournament_dt<-as.Date(wppr$tournament_dt,format = "%m/%d/%Y")


expiration_time<-364 # number of days until the title expires

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
             Tournament = "<a href = 'http://www.ifpapinball.com/view_tournament.php?t= 1 '> U.S. Open </a>" 
               )
  championdata<-data.frame(lapply(championdata,function(x) t(data.frame(x))))
  write.csv(championdata,'championdata.csv',row.names=FALSE) 
}

# Update UPWC.
updateNextChampion<-function(){
  # load data
  past_tournaments<-read.csv("tournament_frame.csv",na.strings = c('None'),stringsAsFactors = F,colClasses = c(NA,NA,NA,"Date",NA,NA,NA,NA,NA))  
  championdata<-read.csv("championdata.csv",stringsAsFactors = F,na.strings = c('None'),colClasses = c(NA,"Date",NA))
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
    
    result<-ifpaRecordUPWCMatch(api.key = api.key, championdata = championdata, tournament.id = tournament.id, tournament.date = tournament.date)
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

ifpaRecordUPWCMatch <- function(api.key,championdata,tournament.id,tournament.date){
  #search past tournaments first, otherwise use api to get results. NOTE: this block is currently uncommented because there is a bug in the ifpa api. The actual winner listed in past tournaments is incorrect!!!! 
#   if (tournament.id %in% past_tournaments$tournament_id){
#     tournament<-subset(past_tournaments,past_tournaments$tournament_id==tournament.id)
#     winner.id<-tournament$winner_player_id
#     p_name<-tournament$winner_name
#     country_code<-tournament$country_code
#     tournament.name<-tournament$tournament_name
#   }else{
#   url<-paste("https://api.ifpapinball.com/v1/tournament/",tournament.id,"/results?api_key=",api.key,sep = "")
#   t.res<-fromJSON(url)
#   tournament.name<-t.res$tournament$tournament_name
#   t.res<-t.res$tournament$results
#   winner_row<-subset(t.res,t.res$position==1)
#   #extract info
#   winner.id<-winner_row$player_id
#   p_name<-paste(winner_row$first_name,winner_row$last_name)
#   country_code<-winner_row$country_code
  #}
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
    
    # construct row 
    img<-paste("<img src='/img/",country_code,".png'>",sep = "")
    name_url<-paste(img,"<a href = 'http://www.ifpapinball.com/player.php?p=", winner.id,"'>", p_name, "</a>")
    tournament_url<-paste("<a href = 'http://www.ifpapinball.com/view_tournament.php?t=", tournament.id,"'>", tournament.name, "</a>",sep = "")
    new_row<-c(name_url,as.Date(tournament.date),tournament_url)
    # bind to championdata
    
    new_row<-list(Champion = name_url,
                  Date = tournament.date,
                  Tournament = tournament_url
    )
    new_row<-data.frame(lapply(new_row,function(x) t(data.frame(x))))
    championdata<-rbind(championdata,new_row)
    
    
    # construct Ranking table. All winners of the UPWC, rank, and the number of times they've won. 
    f<-table(championdata$Champion)
    f<-data.frame(f)
    names(f)<-c("Champion","Wins") 
    f$Rank<-rank(f$Wins,ties.method = c("min"))#count ranks
    f$Rank<-dim(f)[1]-(f$Rank-1)#invert ranks
    f<-arrange(f,-Wins)#sort the table
    # write results
    write.csv(championdata,'championdata.csv',row.names=FALSE)
    # write recent resutls
    if(dim(championdata)[1] > 10){
      recent<-championdata[(dim(championdata)[1]-9):(dim(championdata)[1]),]
      recent<-arrange(recent,desc(Date))
      write.csv(recent,'recent_results.csv',row.names=FALSE)
    }
    # write top champs
    write.csv(f,'top_champs.csv',row.names=FALSE)
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

#Original Analysis --------------------------------------

#constructs the UPWC champ data. This is only to save on the number of API calls. Really only used once. 
construct_UPWC_champ_data<-function(all_champions){
  champion_info<-data.frame()
  for (i in 1:length(all_champions)){
    player_info<-ifpaGetPlayerInfo(api.key = api.key,player.number = all_champions[i])
    champion_info<-rbind(champion_info,player_info)
  }
  return(champion_info)
}


#returns the winner of a given tournament
tournament_winner<-function(tournament.key,c){
  #c is champion
  winner<-subset(wppr$player_key,wppr$tournament_key==tournament.key & wppr$position==1)
  if (length(winner)==0){
    winner<-c
  }
  return(winner)
}

#returns a vector of the players in a tournament
players_in_tournament<-function(tournament.key){
  players<-subset(wppr$player_key,wppr$tournament_key==tournament.key)
  return(players)
}

#returns the number of players in a tournament
num_players_in_tournament<-function(tournament.key){
  players<-players_in_tournament(tournament.key)
  return(length(players))
}

tournament_date<-function(tournament.key){
  date<-subset(wppr$tournament_dt,wppr$tournament_key==tournament.key)
  return(date[1])
}

#number of events on a date
tournaments_on_date<-function(date){
  tourneys<-subset(wppr$tournament_key,wppr$tournament_dt==date)
  return(unique(tourneys))
}

#value of a tournament
tournament_value<-function(tournament.key){
  points<-subset(wppr$total_points,wppr$tournament_key==tournament.key)
  return(max(points))
}

#determine UPWC match given vector of tournaments
unofficial_tournament<-function(tournaments){
  #get vector of points for each tournament
  points<-sapply(tournaments,function(x) tournament_value(x))
  #get largest tournament amount
  p<-max(points)
  #extract the smallest tournaments
  tournaments<-subset(tournaments,points==p)
  return(tournaments)
}

#all tournaments that an individual player has played in
tournaments_including_player<-function(player.key){
  tournaments<-unique(subset(wppr$tournament_key,wppr$player_key==player.key))
  return(tournaments)
}

#determine if player was in a tournament, return a boolean
player_was_in_tournament<-function(p,t){
  players<-players_in_tournament(t)
  return(p%in%players)
}


find_valid_dates<-function(d){
  #returns a vector of valid matches that exist within the grace period
  event_dates<-unique(wppr$tournament_dt)
  elapsed_time<-event_dates-d
  valid_range<-(elapsed_time>0 & elapsed_time<expiration_time)
  valid_dates<-subset(event_dates,valid_range)
  return(valid_dates)
}

find_concession_match<-function(d){
  #finds the first match to use after the concession of a champion
  event_dates<-unique(wppr$tournament_dt)
  elapsed_time<-event_dates-d
  valid_range<-(elapsed_time>expiration_time)
  
  valid_dates<-subset(event_dates,valid_range)
  #determine earliest date
  earliest_date<-range(valid_dates)
  
  
  t<-tournaments_on_date(earliest_date[1])
  
  #if multiple tournaments, filter to only the official one
  t<-unofficial_tournament(t)
  
  return(t)
}

find_next_match_2<-function(c,d){
  #c==champion number, d==date
  
  #get all the champs event dates
  e<-unique(subset(wppr$tournament_dt,wppr$player_key==c))
  #get all valid event dates
  valid_events<-find_valid_dates(d)
  
  #check if there are any events in the grace period
  if ((length(valid_events))==0){
    t<-find_concession_match(d)
    return(t)
  }else if ((sum(e %in% valid_events))<1){
    #determine if champ has any valid events in grace period
    t<-find_concession_match(d)
    return(t)
    
  }else{
    #get distances
    dist<-e-d
    #get next most recent date that is greater than 0
    nearest<-subset(dist,dist>0)
    if(length(nearest)==0){
      return("error")#if this happens, it means the person has no future events!!!!!
    }
    
    
    nearest<-min(nearest)
    e<-subset(e,dist==nearest)
    
    #get the tournaments on the most recent date (in case multiple events on same day)
    t<-tournaments_on_date(e)
    #get the tournaments that the champ played in
    in_t<-sapply(t,function(x) player_was_in_tournament(c,x))
    t<-subset(t,in_t)
    #if multiple tournaments, filter to only the official one
    t<-unofficial_tournament(t)
    return(t)
  }
}

find_next_match<-function(c,d){
  #c==champion number, d==date
  
  #get all the champs event dates
  e<-unique(subset(wppr$tournament_dt,wppr$player_key==c))
  
  #get distances
  dist<-e-d
  #get next most recent date that is greater than 0
  nearest<-subset(dist,dist>0)
  if(length(nearest)==0){
    return("error")#if this happens, it means the person has no future events!!!!!
  }
  
  
  nearest<-min(nearest)
  e<-subset(e,dist==nearest)
  
  #get the tournaments on the most recent date (in case multiple events on same day)
  t<-tournaments_on_date(e)
  #get the tournaments that the champ played in
  in_t<-sapply(t,function(x) player_was_in_tournament(c,x))
  t<-subset(t,in_t)
  #if multiple tournaments, filter to only the official one
  t<-unofficial_tournament(t)
  return(t)
  
}

check_if_champion<-function(c,d){
  #checks to see if champion is current. Logic is if the number of future tournaments is shorter than the grace period, and the champion has not played in them, then we are up to date
  #get all the champs event dates
  e<-unique(subset(wppr$tournament_dt,wppr$player_key==c))
  #get all valid event dates
  valid_events<-find_valid_dates(d)
  result<-F
  
  if ((sum(e %in% valid_events))<1){
    result<-(range(wppr$tournament_dt)[2]-d)<expiration_time
    
  }
  return(result)
}


#determines past history based on the CSV file. New functions will update based on output. 
determine_base_history<-function(){
  print("writing history")
  all_tournaments<-unique(wppr$tournament_key)
  champion<-tournament_winner(1)
  champion_date<-tournament_date(1)
  champs<-c(champion)
  dates<-c(champion_date)
  historical_tournaments<-c(1)
  
  i<-1
  repeat{        
    #find the next UWPC match!
    m<-find_next_match_2(champion,champion_date)
    
    champion<-tournament_winner(tournament.key = m,c = champion)
    champion_date<-tournament_date(tournament.key = m)
    
    #update results
    champs<-append(champs,champion)
    dates<-append(dates,champion_date)
    historical_tournaments<-append(historical_tournaments,m)
    
    i<-i+1    
    if(i>133){  #only do this check near end, to save processing
      if(check_if_champion(c = champion,d = champion_date)){
        break
      }
    }
  }
  results <- data.frame(champs,dates,historical_tournaments)
  names(results) <- c("Champion","Date","Tournament")
  
  #construct current champion csv
  ifpaRecordChampInfo(api.key = api.key,player.number = champion,date.won = champion_date)
  
  #construct winners table. All winners of the UPWC, rank, and the number of times they've won. 
  f<-table(results$Champion)
  f<-data.frame(f)
  names(f)<-c("Champion","Wins") #name columns
  
  f$Rank<-rank(f$Wins,ties.method = c("min"))#count ranks
  f$Rank<-dim(f)[1]-(f$Rank-1)#invert ranks
  f<-arrange(f,-Wins)#sort the table
  #determine number of times played
  #1)get subset of only tournaments that were upwc matches
  wppr2<-subset(wppr,wppr$tournament_key %in% results$Tournament)
  #2)for all champions, count how many times they appear in subset. NOTE: early IFPA matches only included winner, so results may not be historically accurate
  f$Appearances<-sapply(f$Champion, function(x) sum(wppr2$player_key==x))
  #reorder columns of f dataframe
  f<-f[,c(3,1,2,4)]
  
  #add urls
  results$Champion<-sapply(
    results$Champion,
    FUN = function(x){
      country_code<-subset(champ_data,champ_data$player.player_id==x)[6]
      img<-paste("<img src='/img/",country_code,".png'>",sep = "")
      p_name<-paste(subset(champ_data,champ_data$player.player_id==x)[2], subset(champ_data,champ_data$player.player_id==x)[3])
      url<-paste(img,"<a href = 'http://www.ifpapinball.com/player.php?p=", x,"'>", p_name, "</a>")
      url
    } 
  )
  results$Tournament<-sapply(
    results$Tournament,
    FUN = function(x){
      tournament_name<-ifpaGetTournamentName(api.key,x)
      url<-paste("<a href = 'http://www.ifpapinball.com/view_tournament.php?t=", x,"'>", tournament_name, "</a>")
      url
    } 
  )
  f$Champion<-sapply(
    f$Champion,
    FUN = function(x){
      country_code<-subset(champ_data,champ_data$player.player_id==x)[6]
      img<-paste("<img src='/img/",country_code,".png'>",sep = "")
      p_name<-paste(subset(champ_data,champ_data$player.player_id==x)[2], subset(champ_data,champ_data$player.player_id==x)[3])
      url<-paste(img,"<a href = 'http://www.ifpapinball.com/player.php?p=", x,"'>", p_name, "</a>")
      url
    }
  )
  #write all results
  write.csv(results,'championdata.csv',row.names=FALSE)
  #write recent resutls
  recent<-results[(dim(results)[1]-9):(dim(results)[1]),]
  recent<-arrange(recent,desc(Date))
  write.csv(recent,'recent_results.csv',row.names=FALSE)
  #write top champs
  write.csv(f,'top_champs.csv',row.names=FALSE)
  
  print("done")
  return(results)
}

#upwc<-determine_history()

best_finish<-function(tournament){
  a<-subset(wppr,wppr$tournament_key==tournament)
  return(min(a$position))
}

best_finish_analysis<-function(){
  t<-unique(wppr$tournament_key)
  t.res<-sapply(t,function(x) best_finish(x))
  return(t.res)
}
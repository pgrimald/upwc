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

#Update UPWC. This is run AFTER determining the base history.-------------------------------------------------

update_next_champion<-function(){
  #upload the previous results
  current_champ<-read.csv("current_champ_private.csv",na.strings=c('None'), stringsAsFactors=F, colClasses=c(NA,"Date"))
  championdata<-read.csv("championdata.csv",stringsAsFactors=F,na.strings=c('None'))
  #current_champ is the player number and date won of the current champion. 
  #If there is a date and no champ, that means the champion has expired. The date is the date that the title expired
  
  #check that there is a recorded champion. if there is a date
  
  #get all players tournaments
  tournaments<-ifpa_get_player_tournaments(api.key = api.key,player.number = current_champ$player_id)
  #get subset occurring after title was won
  new_tournaments<-subset(tournaments,tournaments$results.event_date > current_champ$date_won)
  #check if there are new tournaments
  if(dim(new_tournaments)[1]>0){
    #get the most recent tournaments
    new_tournaments<-subset(new_tournaments,new_tournaments$results.event_date==min(new_tournaments$results.event_date))    
    
    #check that there is not more than 1 event for that tournament. If so, choose the one with the largest WPPR value
    if(dim(new_tournaments)[1]>1){
      new_tournaments<-ifpa_get_best_tournament(new_tournaments)
    }
    #record event_date-
    tournament.date<-new_tournaments$results.event_date
    #find the winner of the tournament
    tournament.id<-new_tournaments$results.tournament_id
    url<-paste("https://api.ifpapinball.com/v1/tournament/",tournament.id,"/results?api_key=",api.key,sep = "")
    t.res<-fromJSON(url)
    tournament.name<-t.res$tournament$tournament_name
    t.res<-t.res$tournament$results
    winner_row<-subset(t.res,t.res$position==1)
    #extract info
    winner.id<-winner_row$player_id
    p_name<-paste(winner_row$first_name,winner_row$last_name)
    country_code<-winner_row$country_code
    #construct row 
    img<-paste("<img src='/img/",country_code,".png'>",sep = "")
    name_url<-paste(img,"<a href = 'http://www.ifpapinball.com/player.php?p=", winner.id,"'>", p_name, "</a>")
    tournament_url<-paste("<a href = 'http://www.ifpapinball.com/view_tournament.php?t=", tournament.id,"'>", tournament.name, "</a>",sep = "")
    new_row<-c(name_url,tournament.date,tournament_url)
    #bind to championdata
    championdata<-rbind(championdata,new_row)
    #construct Ranking table. All winners of the UPWC, rank, and the number of times they've won. 
    f<-table(championdata$Champion)
    f<-data.frame(f)
    names(f)<-c("Champion","Wins") 
    f$Rank<-rank(f$Wins,ties.method = c("min"))#count ranks
    f$Rank<-dim(f)[1]-(f$Rank-1)#invert ranks
    f<-arrange(f,-Wins)#sort the table
    #write results
    write.csv(championdata,'championdata.csv',row.names=FALSE)
    #write recent resutls
    recent<-championdata[(dim(championdata)[1]-9):(dim(championdata)[1]),]
    recent<-arrange(recent,desc(Date))
    write.csv(recent,'recent_results.csv',row.names=FALSE)
    #write top champs
    write.csv(f,'top_champs.csv',row.names=FALSE)
    #record newest champ
    ifpa_record_champ_info(api.key = api.key,player.number = winner.id, date.won = tournament.date)
    #upload to server
    update_server()
    print(results)
    print(tournament.date)
    print(winner.id)
  }else{
    #no new tournaments were found, so now make sure that a year has not passed
    if((Sys.Date()-current_champ$date_won)>365){
      
    }else{
      #the reigning champion has expired
    }
    
        
    return("No new champions found")  
  }
}


#given dataframe of tournament results, return the one with the highest WPPR value
ifpa_get_best_tournament<-function(api.key,tournaments){
  tournaments$value<-sapply(X = tournaments$results.tournament_id,
         FUN = function(x){
           value<-ifpa_get_tournament_value(api.key,x)
           }
        )
  tournaments<-subset(tournaments,tournaments$value==max(tournaments$value))
  return(tournaments)
}

#return WPPR value of tournament
ifpa_get_tournament_value<-function(api.key,tournament.number){
  url<-paste("https://api.ifpapinball.com/v1/tournament/",tournament.number,"/results?api_key=",api.key,sep = "")
  json_data <- fromJSON(url)
  return(json_data$tournament$event_value)
}

#return tournament results
ifpa_get_tournament_results<-function(api.key,tournament.key){
  url<-paste("https://api.ifpapinball.com/v1/tournament/list?api_key=",api.key,"&start_pos=",start.pos,"&count=250",sep = "")
  json_data <- fromJSON(url)
  tournaments <- json_data["tournament"]
  tournaments<-data.frame(tournaments)
  names(tournaments)<-c("tournament_id","tournament_name","event_name","event_date","winner_name","winner_player_id","country_code","country_name","player_count")
  return(tournaments)
}

#return a high level list of tournaments
ifpa_get_tournaments<-function(api.key,start.pos){
  url<-paste("https://api.ifpapinball.com/v1/tournament/list?api_key=",api.key,"&start_pos=",start.pos,"&count=250",sep = "")
  json_data <- fromJSON(url)
  tournaments <- json_data["tournament"]
  tournaments<-data.frame(tournaments)
  names(tournaments)<-c("tournament_id","tournament_name","event_name","event_date","winner_name","winner_player_id","country_code","country_name","player_count")
  return(tournaments)
}

#returns a given players entire results history
ifpa_get_player_tournaments<-function(api.key,player.number){
  url<-paste("https://api.ifpapinball.com/v1/player/",player.number,"/results?api_key=",api.key, sep = "")
  json_data <- fromJSON(url)
  tournaments <- json_data["results"]
  tournaments <-data.frame(tournaments)
  return(tournaments)
}

#returns info about a player
ifpa_get_player_info<-function(api.key,player.number){
  url<-paste("https://api.ifpapinball.com/v1/player/",player.number,"?api_key=",api.key,sep = "")
  json_data <- fromJSON(url)
  player <- json_data["player"]
  player <-data.frame(player)
  return(player)
}

#returns info about a player
ifpa_record_champ_info<-function(api.key,player.number,date.won){
  url<-paste("https://api.ifpapinball.com/v1/player/",player.number,"?api_key=",api.key,sep = "")
  j <- fromJSON(url)
  name<-paste(j$player$first_name,j$player$last_name)
  country_code<-j$player$country_code
  img<-paste("<img src='/img/",country_code,".png'>",sep = "")
  url<-paste(img,"<a href = 'http://www.ifpapinball.com/player.php?p=", player.number,"'>", name, "</a>")
  home_town<-paste(j$player$city,", ",j$player$state,sep = "")
  rank<-j$player_stats$current_wppr_rank
  age<-j$player$age
  
  #record csv for website
  player_info<-list(Name=url,Hometown=home_town,Age=age,Ifpa_Rank=rank,Held_Since=date.won)
  df<-data.frame(lapply(player_info,function(x) t(data.frame(x))))
  write.csv(df,'current_champ.csv',row.names=FALSE)
  
  #record csv for future analysis
  private_player_info<-list(player_id=player.number,date_won=date.won)
  private_df<-data.frame(lapply(private_player_info,function(x) t(data.frame(x))))
  
  
  write.csv(private_df,'current_champ_private.csv',row.names=FALSE)
}


ifpa_get_tournament_name<-function(api.key,tournament.number){
  url<-paste("https://api.ifpapinball.com/v1/tournament/",tournament.number,"?api_key=",api.key,sep = "")
  json_data <- fromJSON(url)
  tournament_name<-json_data$tournament$tournament_name
  return(tournament_name)
}

#constructs the UPWC champ data. This is only to save on the number of API calls. Really only used once. 
construct_UPWC_champ_data<-function(all_champions){
  champion_info<-data.frame()
  for (i in 1:length(all_champions)){
    player_info<-ifpa_get_player_info(api.key = api.key,player.number = all_champions[i])
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
  valid_range<-(elapsed_time>0 & elapsed_time<365)
  valid_dates<-subset(event_dates,valid_range)
  return(valid_dates)
}

find_concession_match<-function(d){
  #finds the first match to use after the concession of a champion
  event_dates<-unique(wppr$tournament_dt)
  elapsed_time<-event_dates-d
  valid_range<-(elapsed_time>365)
  
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
    result<-(range(wppr$tournament_dt)[2]-d)<365
    
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
ifpa_record_champ_info(api.key = api.key,player.number = champion,date.won = champion_date)

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
    tournament_name<-ifpa_get_tournament_name(api.key,x)
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



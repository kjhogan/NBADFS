#function to get JSONS for season
getSeasonJSONS <- function(season, type) {
 
  
  if (length(intersect(type, c("REG", "POST"))) < 1){
    return ("Please provide correct season type (REG/POST)")
  }
  else{
    packages <- #need all of these installed including some from github
      c('dplyr',
        'rvest',
        'dplyr',
        'pipeR',
        'stringr',
        'lubridate',
        'ggplot2',
        'jsonlite')
    lapply(packages, library, character.only = T)
    
    link <- paste0("http://www.nfl.com/scores/",as.character(season), "/",type )
    
    for (i in c(1:17)) {
      link <- paste0(link, as.character(i))
      games <- read_html(link) %>% html_nodes('.game-center-link') %>% html_attr("href")
      for (j in 1:length(games)){
        gameid <- strsplit(games[j], "/")[[1]][3]
        jsonlink <- paste0("http://www.nfl.com/liveupdate/game-center/", gameid, "/", gameid, "_gtd.json")
        write(toJSON(fromJSON(jsonlink)), paste0(gameid,'.json'));
        print(jsonlink)
      }
      
      print(link)
      link <- paste0("http://www.nfl.com/scores/",as.character(season), "/",type )
    }
    
    
    return("Done");
  }
  
}

#function to parse NFL games by individual plays
parseNFLGame <- function(season, gameId) {
  
  packages <- #need all of these installed including some from github
    c('dplyr',
      'rvest',
      'dplyr',
      'pipeR',
      'stringr',
      'lubridate',
      'ggplot2',
      'jsonlite')
  
  
  lapply(packages, library, character.only = T)
  
  game <- fromJSON(paste0('./NFLPBP/',as.character(season), '/',gameId,'.json'), flatten = TRUE)
  
  drives <- game[[1]]$drives
  descriptions <- c()
  posteam <- c()
  down <- c()
  qtr <- c()
  ydstogo <- c()
  time <- c()
  
  for (i in 1:length(drives)){
    for (j in 1:length(drives[[i]]['plays'][[1]])){
      descriptions <- c(descriptions,drives[[i]]['plays'][[1]][[j]]['desc'][[1]]); 
      posteam <- c(posteam,drives[[i]]['plays'][[1]][[j]]['posteam'][[1]]); 
      down <- c(down,drives[[i]]['plays'][[1]][[j]]['down'][[1]]); 
      qtr <- c(qtr,drives[[i]]['plays'][[1]][[j]]['qtr'][[1]]); 
      ydstogo <- c(ydstogo,drives[[i]]['plays'][[1]][[j]]['ydstogo'][[1]]); 
      time <- c(time,drives[[i]]['plays'][[1]][[j]]['time'][[1]]); 
    }
  }
  
  gamesummary <- data.frame(unlist(qtr), unlist(time), unlist(posteam), unlist(down), unlist(ydstogo), unlist(descriptions), stringsAsFactors = FALSE)
  return(gamesummary)
}


#function to parse pass plays
getQBPassData <- function(season,gameId, type){
  packages <- #need all of these installed including some from github
    c('dplyr',
      'rvest',
      'dplyr',
      'pipeR',
      'stringr',
      'lubridate',
      'ggplot2',
      'jsonlite')
  
  
  lapply(packages, library, character.only = T)
  
  if(type == "REG"){
    game <- fromJSON(paste0('./NFLPBP/',as.character(season), '/',gameId,'.json'), flatten = TRUE)
  }
  
  else {
    game <- fromJSON(paste0('./NFLPBP/POST/',as.character(season), '/',gameId,'.json'), flatten = TRUE)
  }
  nflstatid <- c(4, 14, 15, 16, 19, 20, 21, 22, 23, 24, 77, 78, 85, 110, 111, 112, 113, 115, 79, 
                 7, 6, 82, 25, 26, 8, 80, 120, 402, 5, 93, 104, 105)
  
  nflstat <- c('pass_1st', 'pass_inc', 'pass_cmp', 'pass_td', 'pass_int', 'pass_sk', 'rec_yds', 
               'rec_ydsTD','rec_yds_NR', 'rec_yds_TDNR', 'pa_2pt_good', 'pa_2pt_miss', 'pass_def','qb_hit', 
               'pass_cmp_ayds', 'pass_inc_ayds', 'yac', 'target', 'def_tkl', 'failed3rd', 'successful3rd', 
               'ast_tkl', 'def_int', 'pick6', '4thconversion', 'primary_tkl', 'tfl', 'def_tfl', 'pen_1st', 
               'penalty', 'rec_2ptmd', 'rec2ptmss')
  
  lookup <- data.frame(nflstatid, nflstat, stringsAsFactors = FALSE)
  
 
  
  drives <- game[[1]]$drives
  teams <- c(game[[1]]$home$abbr[[1]], game[[1]]$away$abbr[[1]])
  sites <- c("home", "away")
  qbnames <- getQBNames(game)
  scoresummary <- getScoreSummary(game)
  season <- as.character(season)
  game_date <- as.Date(substr(gameId,1,8),"%Y%m%d") #turn gameId into date
  tm <- c()
  opp <- c()
  qbName <- c()
  receiverName <- c()
  receiverPos <- c()
  defenderName <- c()
  defenderPos <- c()
  down <- c()
  distance <- c()
  qtr <- c()
  time <- c()
  pass_result <- c()
  isFirst <- c()
  isTD <- c()
  isInt <- c()
  isPick6 <- c()
  isQBHit <- c()
  air_yds <- c()
  yac <- c()
  yards <- c()
  isPenalty <- c()
  penalty <- c()
  formation <- c()
  huddle <- c()
  direction <- c()
  long_short <- c()
  desc <- c()
  isSack <- c()
  tmscore <- c()
  oppscore <- c()
  site <- c()
  drivenumber <- c()
  yardline <- c()
  isRedZone <- c()
  playseconds <- c()
  
  data <- data.frame(sequence = integer(), clubcode = character(), playername = character(), statId = integer(), yards = integer(), stringsAsFactors = FALSE)
  temp_data <- data.frame(sequence = integer(), clubcode = character(), playername = character(), statId = integer(), yards = integer(), stringsAsFactors = FALSE)
  test_data <- data.frame(sequence = integer(), clubcode = character(), playername = character(), statId = integer(), yards = integer(), stringsAsFactors = FALSE)

  for (i in 1:length(drives)){
    for (j in 1:length(drives[[i]]['plays'][[1]])){
      
      desc <- drives[[i]]['plays'][[1]][[j]]['desc'][[1]]
      

       if((grepl('pass',desc) || grepl('sacked', desc) || grepl('scrambles', desc)) && !grepl('kicks', desc)) { 
         
         #append basic variables
         tm <- append(tm,drives[[i]]['plays'][[1]][[j]]['posteam'][[1]]);
         opp <- append(opp, teams[!(teams == drives[[i]]['plays'][[1]][[j]]['posteam'][[1]])])
         down <- append(down, drives[[i]]['plays'][[1]][[j]]['down'][[1]])
         distance <- append(distance, drives[[i]]['plays'][[1]][[j]]['ydstogo'][[1]])
         qtr <- append(qtr, drives[[i]]['plays'][[1]][[j]]['qtr'][[1]])
         time <- append(time, drives[[i]]['plays'][[1]][[j]]['time'][[1]])
         site <- append(site, sites[drives[[i]]['plays'][[1]][[j]]['posteam'][[1]]])
         yrdsplit <- unlist(strsplit(drives[[i]]['plays'][[1]][[j]]['yrdln'][[1]], split = " "))
         yardline <- append(yardline, drives[[i]]['plays'][[1]][[j]]['yrdln'][[1]])
         playtime <- timeConvert(drives[[i]]['plays'][[1]][[j]]['time'][[1]],drives[[i]]['plays'][[1]][[j]]['qtr'][[1]])
         playseconds <- append(playseconds, playtime)
     
         
        tmscoredf <- filter(scoresummary, (tm == drives[[i]]['plays'][[1]][[j]]['posteam'][[1]]) & seconds <= playtime)
        oppscoredf <- filter(scoresummary, (tm == teams[!(teams == drives[[i]]['plays'][[1]][[j]]['posteam'][[1]])]) & seconds <= playtime)

        
        if(nrow(tmscoredf) > 0) {
          tmcurrentscore <- tmscoredf$score[length(tmscoredf$score)]
        }
        
        else{
          tmcurrentscore <- 0
        }
        
        if(nrow(oppscoredf) > 0) {
          oppcurrentscore <- oppscoredf$score[length(oppscoredf$score)]
        }
        
        else{
          oppcurrentscore <- 0
        }
        
        tmscore <- append(tmscore, tmcurrentscore)
        oppscore <- append(oppscore, oppcurrentscore)
        
        if(yrdsplit[1] == teams[!(teams == drives[[i]]['plays'][[1]][[j]]['posteam'][[1]])] & yrdsplit[2] <= 20){
          isRedZone <- append(isRedZone, c(1))
        }
        else{
          isRedZone <- append(isRedZone, c(0))
        }
         #description split up
         desclist <- unlist(strsplit(desc, split = " ")) 
         pendesclist <- unlist(strsplit(desc, split = ","))
         
         #parse desc for formation, huddle, direction, long_short, and penalty
         if (grepl('Shotgun',desc)) {
           formation <- append(formation, 'Shotgun')
         }
         else {
           formation <- append(formation, 'Under Center')
         }
         
         if (grepl('No Huddle',desc)) {
           huddle <- append(huddle, 'No Huddle')
         }
         else {
           huddle <- append(huddle, 'Huddle')
         }
         
         if (grepl('short',desc)) {
           long_short <- append(long_short, 'short')
         }
         else {
           long_short <- append(long_short, 'long')
         }
         
         if (grepl('left',desc)) {
           direction <- append(direction, 'left')
         }
         else if (grepl('middle',desc)) {
           direction <- append(direction, 'middle')
         }
         else {
           direction <- append(direction, 'right')
         }
        
        #penalty from description
         if (grepl('PENALTY',desc)) {
           isPenalty <- append(isPenalty, c(1))
           if(length(trimws(pendesclist[which(grepl(c("PENALTY on"),pendesclist)) + 1])) > 1) {
             penalties <- trimws(pendesclist[which(grepl(c("PENALTY on"),pendesclist)) + 1])
             penalty <- append(penalty, paste0(penalties[1], ", ", penalties[2]))
           }
           else{
           penalty <- append(penalty,trimws(pendesclist[which(grepl(c("PENALTY on"),pendesclist)) + 1]))
           }
         }
         
         else {
           isPenalty <- append(isPenalty, c(0))
           penalty <- append(penalty,'none')
         }

         #pull out sacks
        if (grepl('sacked',desc)) {
          isSack <- append(isSack, c(1))
          pass_result <- append(pass_result,"sack")
        }
        else {
          isSack <- append(isSack, c(0))
        }
        
        #scramble
        if (grepl('scrambles',desc)) {
          if(!grepl('pass', desc)) {
            pass_result <- append(pass_result,"scramble")      
          }
          else if(grepl('Illegal Forward Pass', desc)){
            pass_result <- append(pass_result, "inc")
          }
          
        }

        
         #loop through sub tables
         players <- drives[[i]]['plays'][[1]][[j]]['players']
         
         for (k in 1:length(players)) {
           for(l in 1:length(players[k])){
             if(length(players[k][[l]]) > 0){
               for (m in 1:length(players[k][[l]])) {
                data <- data.frame(players[k][[l]][[m]])
                data <- left_join(data, lookup, c("statId" = "nflstatid"))
                if (ncol(data) < 6) {
                  data$playerName <- rep("", nrow(data))
                  data <- data[,c(1,2,6,3,4,5)]
                }
                else{
                  data <- data
                }
    
                temp_data <- rbind(temp_data, data)
                
               }
             }
             else { "Not working"}
           }
           
           #First Down
           team_basic <- filter(temp_data, nflstat %in% c('pass_1st', 'pen_1st'))
           
           if(nrow(team_basic) > 0) {
             isFirst <- append(isFirst, c(1))
           }
           
           else {
             isFirst <- append(isFirst, c(0))
           }
           
           #QB and Passing Mapping
           pass_basic <-filter(temp_data, nflstat %in% c('pass_cmp' , 'pass_inc', 'pass_td', 'pass_int', 'pa_2pt_good', 'pa_2pt_miss'))
           pass_adv <- filter(temp_data, nflstat %in% c('pass_cmp_ayds' , 'pass_inc_ayds'))
           
           if (nrow(pass_basic) > 0) {
            
               qbName <- append(qbName, pass_basic$playerName)

               yards <- append(yards, pass_basic$yards)
               if(nrow(pass_adv) > 0 ) { 
                 air_yds <- append(air_yds, pass_adv$yards)
               }
               else{
                 air_yds <- append(air_yds, c(0))
               }
               
               
               
               if (pass_basic$nflstat %in% c('pass_cmp' ,  'pass_td', 'pa_2pt_good')){
                 pass_result <- append(pass_result, 'cmp')
                 isInt <- append(isInt, c(0))
                 if (pass_basic$nflstat == 'pass_td') { 
                   isTD <- append(isTD, c(1))
                 }
                 else {
                   isTD <- append(isTD, c(0))
                 }
               }
             
               else if (pass_basic$nflstat %in% c('pass_inc','pass_int','pa_2pt_miss')) { 
                 if (grepl('sacked',desc)) {pass_result <- pass_result[-length(pass_result)]}
                 pass_result <- append(pass_result, 'inc')
                 isTD <- append(isTD, c(0))
                 if (pass_basic$nflstat == 'pass_int') { isInt <- append(isInt, c(1))}
                 else {isInt <- append(isInt, c(0))}
               }
           }
            
           else {
             if (grepl('sacked',desc) || grepl('scrambles', desc)) { pass_result <- pass_result}
             else{
               pass_result <- append(pass_result, 'inc')
             }
             yards <- append(yards, c(0))
             isTD <- append(isTD, c(0))
             isInt <- append(isInt, c(0))
             if(length(intersect(qbnames, desclist)) > 0){
               qbName <- append(qbName, intersect(qbnames, desclist)[1])
             }
             
             else{
               qbName <- append(qbName, "n/a")
             }
             
             if(nrow(pass_adv) > 0 ) { 
               air_yds <- append(air_yds, pass_adv$yards)
             }
             else{
               air_yds <- append(air_yds, c(0))
             }
           }
           

           # Receiver Mapping
           rec_basic <- filter(temp_data, nflstat == "target")
           rec_advanced <- filter(temp_data, nflstat == "yac")
           
           if (nrow(rec_basic) > 0) {
             receiverName <- append(receiverName, rec_basic$playerName)
           }
           
           else {
             if (grepl(c("."), desclist[match(c("to"), desclist) + 1])) {
               receiverName <- append(receiverName,  desclist[match(c("to"), desclist) + 1])
             }
             else{
               receiverName <- append(receiverName, c("n/a"))
             }
             
           }
           
          if (nrow(rec_advanced) > 0) {
            if(length(rec_advanced$yards) > 1) {
              temp_yac <- sum(rec_advanced$yards)
              yac <- append(yac, temp_yac)
            }
            else {
              yac <- append(yac, rec_advanced$yards)
            }
             
            }

          else {
              yac <- append(yac, c(0))
  
              }
          
          
          #Defensive mapping for defendername, QB Hit, penalty, interceptions/pick 6's
          def_basic <- filter(temp_data, nflstat %in% c('pass_def','qb_hit', 'def_tkl','primary_tkl', 'ast_tkl', 
                                                        'def_int', 'pass_def', 'pick6', 'primary_tkl', 
                                                        'qb_hit', 'tfl', 'penalty'))
          
          if (nrow(def_basic) > 0) {
            
            if(nrow(filter(def_basic, nflstat == 'pass_def')) > 0){
              isPick6 <- append(isPick6, c(0))
              isQBHit <- append(isQBHit, c(0))
              def_df <- filter(def_basic, nflstat == 'pass_def') 
              def_df <- filter(def_df, rank(sequence, ties.method="first") == 1)
              defenderName <- append(defenderName, def_df$playerName)
              
            }
            else if(nrow(filter(def_basic, nflstat == 'def_int')) > 0){
              isPick6 <- append(isPick6, c(0))
              isQBHit <- append(isQBHit, c(0))
              def_df <- filter(def_basic, nflstat == 'def_int') 
              defenderName <- append(defenderName, def_df$playerName)
              
            }
            
            else if(nrow(filter(def_basic, nflstat == 'def_tkl')) > 0){
              isPick6 <- append(isPick6, c(0))
              isQBHit <- append(isQBHit, c(0))
              def_df <- filter(def_basic, nflstat == 'def_tkl') 
              def_df <- filter(def_df, rank(sequence, ties.method="first") == 1)
              defenderName <- append(defenderName, def_df$playerName)
              
            }
            
            else if(nrow(filter(def_basic, nflstat == 'primary_tkl')) > 0){
              isPick6 <- append(isPick6, c(0))
              isQBHit <- append(isQBHit, c(0))
              def_df <- filter(def_basic, nflstat == 'primary_tkl') 
              def_df <- filter(def_df, rank(sequence, ties.method="first") == 1)
              defenderName <- append(defenderName, def_df$playerName)
              
            }
            
            
            else if(nrow(filter(def_basic, nflstat == 'pick6')) > 0){
              isQBHit <- append(isQBHit, c(0))
              isPick6 <- append(isPick6, c(1))
              def_df <- filter(def_basic, nflstat == 'pick6') 
              defenderName <- append(defenderName, def_df$playerName)
              
            }
            
            else if(nrow(filter(def_basic, nflstat == 'tfl')) > 0){
              isPick6 <- append(isPick6, c(0))
              isQBHit <- append(isQBHit, c(0))
              def_df <- filter(def_basic, nflstat == 'tfl') 
              defenderName <- append(defenderName, def_df$playerName)
              
            }
            
            else if(nrow(filter(def_basic, nflstat == 'ast_tkl')) > 0){ 
              isPick6 <- append(isPick6, c(0))
              isQBHit <- append(isQBHit, c(0))
              def_df <- filter(def_basic, nflstat == 'ast_tkl')
              def_df <- filter(def_df, rank(sequence, ties.method="first") == 1)
              defenderName <- append(defenderName, def_df$playerName)
              
            }
            
            else if(nrow(filter(def_basic, nflstat == 'qb_hit')) > 0){
              isPick6 <- append(isPick6, c(0))
              def_df <- filter(def_basic, nflstat == 'qb_hit')
              isQBHit <- append(isQBHit, c(1))
              defenderName <- append(defenderName, def_df$playerName)
              
            } 
            
            else if(nrow(filter(def_basic, nflstat == "penalty")) > 0) {
              isPick6 <- append(isPick6, c(0))
              isQBHit <- append(isQBHit, c(1))
              def_df <- filter(def_basic, nflstat == 'penalty')
              def_df <- filter(def_df, clubcode == opp[length(opp)])
              def_df <- filter(def_df, rank(sequence, ties.method="first") == 1)
              if(length(intersect(desclist, c("Defensive"))) > 0){
                defenderName <- append(defenderName, def_df$playerName)
              }
              else {
                defenderName <- append(defenderName, "n/a")
              }
            }
            
            else {
              isPick6 <- append(isPick6, c(0))
              isQBHit <- append(isQBHit, c(0))
              defenderName <- append(defenderName, c('n/a'))
              
            }
            
          }
          
          else {
            defenderName <- append(defenderName, c('n/a'))
            isQBHit <- append(isQBHit, c(0))
            isPick6 <- append(isPick6, c(0))
            
          }
          
           test_data <- rbind(test_data, temp_data)
           data <- data.frame()
           temp_data <- data.frame()
          
         }
         
       }
     
    }
    
  }

#   uncomment below for bug testing
#   print(paste0("tm: ",length(tm)))
#         print(paste0("opp: ",length(opp)))
#         print(paste0("qbName: ",length(qbName)))
#         print(paste0("receiverName: ",length(receiverName)))
#         print(paste0("defenderName: ",length(defenderName)))
#         print(paste0("down: ",length(down)))
#         print(paste0("distance:",length(distance)))
#         print(paste0("qtr:",length(qtr)))
#         print(paste0("pass_result:",length(pass_result)))
#         print(paste0("isFirst:",length(isFirst)))
#         print(paste0("isTD:",length(isTD)))
#         print(paste0("isInt:",length(isInt)))
#         print(paste0("isPick6:",length(isPick6)))
#         print(paste0("isQBHit:",length(isQBHit)))
#         print(paste0("air_yds:",length(air_yds)))
#         print(paste0("yac:",length(yac)))
#         print(paste0("yards:",length(yards)))
#         print(paste0("isPenalty:",length(isPenalty)))
#         print(paste0("formation:",length(formation)))
#         print(paste0("huddle:",length(huddle)))
#         print(paste0("direction:",length(direction)))
#         print(paste0("long_short:",length(long_short)))
#         print(paste0("time:",length(time)))
#         print(paste0("penalty:",length(penalty)))
#           print(paste0("isSack:", length(isSack)))
#         print(paste0("site:", length(site)))
# #         --print(paste0("drivenum:", length(drivenum)))
#         print(paste0("yardline:", length(yardline)))
#         print(paste0("isRedzone:", length(isRedZone)))
#         print(paste0("tmscore: ", length(tmscore)))
#         print(paste0("oppscore: ",length(oppscore)))
   
#   make dataframe to return
   passingdf <- data.frame(gameId = rep(gameId, length(tm)), season = rep(season, length(tm)), game_date = rep(game_date, length(tm)), tm, opp, site, qbName, 
                           receiverName , receiverPos = rep("tbd", length(tm)), defenderName , defenderPos = rep("tbd", length(tm)), down, 
                           distance, qtr, time,  pass_result, isFirst, isTD, isInt, isPick6,
                           isQBHit, air_yds, yac, yards, isPenalty, penalty, formation, huddle, 
                           direction, long_short,yardline, isRedZone, tmscore, oppscore, isSack, stringsAsFactors = FALSE);
 
#  uncomment below for testing  
#   return("passingdf");
  return(passingdf);
}


#function to return game summaries by team
parseNFLTmData <- function(season,gameId) {
  
  # install packages
  packages <- #need all of these installed including some from github
    c('dplyr',
      'rvest',
      'dplyr',
      'pipeR',
      'stringr',
      'lubridate',
      'ggplot2',
      'jsonlite')
  lapply(packages, library, character.only = T)
  
  #bring in game JSON
  game <- fromJSON(paste0('./NFLPBP/',season, '/', gameId,'.json'), flatten = TRUE)
  
  
  drives <- game[[1]]$drives  #drives segment of pbp
  game_date <- as.Date(substr(gameId,1,8),"%Y%m%d") #turn gameId into date
  
  #assign team/site/opponent to each record
  hometm <- game[[1]]$home$abbr 
  awaytm <- game[[1]]$away$abbr

  
  #create variable instances used in for loop
  pos <- c()
  plays <- c()
  
  #loop through drives to get drive count and number of plays
  for (i in 1:length(drives)){
    
    pos <- append(pos,drives[[i]]['start'][[1]]['team'][[1]]) #add team name to possession vector
    
    note <- unlist(drives[[i]]['plays'][[1]][[1]]['note'][[1]][1]) #get note for first play of drive
    note <- note[is.character(note)] #remove NA's from note
    playcount <- as.numeric(drives[[i]]['numplays'][[1]]) #get numplay stat from drive summary
    
    #subtract kickoff plays from drives
    if (!is.null(note) && length(note) > 0) {
      if (note == "KICKOFF") {plays <- append(plays, playcount-1)}
      else {plays <- append(plays, playcount) }
    }
    else {
      plays <- append(plays, playcount)
    }
    
  }
  
  #remove NA's from both lists
  pos <- pos[!is.na(pos)]
  plays <- plays[!is.na(plays)]
  
  #create off and def poss/play count
  homepos <- length(pos[(pos == hometm)])
  awaypos <- length(pos[(pos == awaytm)])
  homeplays <- sum(plays[(pos == hometm)])
  awayplays <- sum(plays[(pos == awaytm)])
  
  
  #create home dataframe
  homedf <- data.frame(gameId, game_date, tm = hometm, opp = awaytm,
                       Q1 = game[[1]]$home$score[[1]], Q2 = game[[1]]$home$score[[2]], Q3 = game[[1]]$home$score[[3]],
                       Q4 = game[[1]]$home$score[[4]], OT = game[[1]]$home$score[[5]], Tot = game[[1]]$home$score[[6]],
                       game[[1]]$home$stats$team[1], game[[1]]$home$stats$team[2], game[[1]]$home$stats$team[3], 
                       game[[1]]$home$stats$team[4], game[[1]]$home$stats$team[5], game[[1]]$home$stats$team[6], 
                       game[[1]]$home$stats$team[7], game[[1]]$home$stats$team[8], game[[1]]$home$stats$team[11], 
                       pos = homepos, plays = homeplays, opp_pos = awaypos, opp_plays = awayplays, stringsAsFactors=FALSE)
  
  #create away data element
  awaydf <- data.frame(gameId, game_date, tm = awaytm, opp = hometm,
                       Q1 = game[[1]]$away$score[[1]], Q2 = game[[1]]$away$score[[2]], Q3 = game[[1]]$away$score[[3]],
                       Q4 = game[[1]]$away$score[[4]], OT = game[[1]]$away$score[[5]], Tot = game[[1]]$away$score[[6]],
                       game[[1]]$away$stats$team[1], game[[1]]$away$stats$team[2], game[[1]]$away$stats$team[3], 
                       game[[1]]$away$stats$team[4], game[[1]]$away$stats$team[5], game[[1]]$away$stats$team[6], 
                       game[[1]]$away$stats$team[7], game[[1]]$away$stats$team[8], game[[1]]$away$stats$team[11], 
                       pos = awaypos, plays = awayplays, opp_pos = homepos, opp_plays = homeplays, stringsAsFactors=FALSE)
  
  #combine home and away
  summarydf <- rbind(homedf, awaydf)
  
  return(summarydf);
}

#function to get team summaries for entire season
getSeasonSummaries <- function(season) {
  
  #need all of these installed including some from github
  packages <- c('dplyr', 'rvest', 'pipeR', 'stringr','lubridate', 'ggplot2','jsonlite')
  lapply(packages, library, character.only = T)
  
  path = paste0('./NFLPBP/', as.character(season))
  out.file<-""
  file.names <- dir(path, pattern =".json") 
  file.names <- gsub('\\.json', '', file.names)
  
  seasondf <- parseNFLTmData(season,file.names[1])
  
  for (i in 2:length(file.names)){
    seasondf <- rbind(seasondf, parseNFLTmData(season, file.names[i]))
  }
  
  return(seasondf);
  
  
}

#function to loop through season directory for all pass plays
getSeasonPassData <- function(season, type) {
  
  #need all of these installed including some from github
  packages <- c('dplyr', 'rvest', 'pipeR', 'stringr','lubridate', 'ggplot2','jsonlite')
  lapply(packages, library, character.only = T)
  type <- type 
  
  if(type == "REG") {
    path = paste0('./NFLPBP/', as.character(season))
    out.file<-""
    file.names <- dir(path, pattern =".json") 
    file.names <- gsub('\\.json', '', file.names)
    
    seasondf <- getQBPassData(season,file.names[1], type)
    
    
    for (i in 2:length(file.names)){
      print(file.names[i])
      seasondf <- rbind(seasondf, getQBPassData(season, file.names[i], type))
    }
    
    return(seasondf);
  }
  
  else if(type == "POST") {
    
    path = paste0('./NFLPBP/POST/', as.character(season))
    out.file<-""
    file.names <- dir(path, pattern =".json") 
    file.names <- gsub('\\.json', '', file.names)
    
    seasondf <- getQBPassData(season,file.names[1], type)
    
    
    for (i in 2:length(file.names)){
      print(file.names[i])
      seasondf <- rbind(seasondf, getQBPassData(season, file.names[i], type))
    }
    
    return(seasondf); 
  }
  
  else {
    return("Input valid arguments");
  }

}

#helper function to get "passer" names at top of json
getQBNames <- function(game) {
  
  home <- c(game[[1]]$home$abbr[[1]])
  away <- c(game[[1]]$away$abbr[[1]])
  homepassing <- game[[1]]$home$stats$passing
  awaypassing <- game[[1]]$away$stats$passing
  homeQBNames <- c()
  awayQBNames <- c()
  
  
    for(i in 1:length(homepassing)){
      HQB <- homepassing[[i]]['name'][[1]]
      homeQBNames <- append(homeQBNames, HQB)
    }
  
    for(i in 1:length(awaypassing)){
    AQB <- awaypassing[[i]]['name'][[1]]
    awayQBNames <- append(awayQBNames, AQB)
    }
  
  
  qbs <- append(homeQBNames, awayQBNames)
  
  return(qbs)
}

#helper function to get score at time of play
getScoreSummary <- function(game) {
  
  teams <- c(game[[1]]$home$abbr[[1]], game[[1]]$away$abbr[[1]])
  drives <- game[[1]]$drives
  scoresummary <- game[[1]]$scrsummary
  playid <- c()
  time <- c()
  qtr <- c()
  type <- c()
  desc <- c()
  qtr <- c()
  tm <- c()
  opp <- c()
  playval <- c()
  points <- c()
  oppscore <- c()
  playid <- append(playid, names(scoresummary))
  
  
  for(i in 1:length(scoresummary)){

    tm <- append(tm , scoresummary[[i]]['team'][[1]])
    oppname <- teams[!(teams == scoresummary[[i]]['team'][[1]])]
    opp <- append(opp, oppname)
    qtr <- append(qtr, scoresummary[[i]]['qtr'][[1]])
    
    currenttype <- scoresummary[[i]]['type'][[1]]
    if(length(currenttype) < 1) {
      currenttype <- "D2PM"
    }
    else {
      currenttype <- currenttype
    }
    type <- append(type, currenttype)
#     oppscores <- tmscore[tm == oppname]
#     print(tmscore[tm == oppname])
#     tmscores <- tmscore[(tm == scoresummary[[i]]['team'][[1]])]
#     tmcurrentscore <- tmscores[which.max(tmscores)]
#     oppscore <- append(oppscore, oppscores[which.max(oppscores)])
    
    desc <- scoresummary[[i]]['desc'][[1]]
      
    if (currenttype == "SAF") {
      playval <- 2
    }
    
    else if (currenttype == "FG"){
      playval <- 3
    }
    
    else if (currenttype == "D2PM") {
      playval <- 2
    }
    else {
      playval <- 6
      if(grepl("kick", desc)){
        if(grepl("is good", desc)){ playval <- playval + 1}
        else { playval <- playval}
      }
      else {
        if(grepl("good", desc)){ playval <- playval + 2}
        else { playval <- playval}
      }
    }

    points <- append(points, playval)
    
  }
  
playlist <- c()
for (i in 1:length(drives)){
  for (j in 1:length(drives[[i]]['plays'][[1]])){
    playlist <- append(playlist,(drives[[i]]['plays'][[1]]))
  }
}
    
for(l in 1:length(playid)){
  time <- append(time,playlist[playid[l]][[1]]['time'][[1]])
}

    desc <- drives[[i]]['plays'][[1]][[j]]['desc'][[1]]


scoredf <- data.frame(playid, tm, opp, qtr, type, points, time, stringsAsFactors = FALSE)
scoredf$score <- rep(0, nrow(scoredf))
scoredf$score[scoredf$tm == teams[1]] <- cumsum(scoredf$points[scoredf$tm == teams[1]])
scoredf$score[scoredf$tm == teams[2]] <- cumsum(scoredf$points[scoredf$tm == teams[2]])
seconds <- c()
for(m in 1:nrow(scoredf)){
  seconds <- append(seconds, timeConvert(scoredf$time[m], scoredf$qtr[m]))
}

scoredf$seconds <- seconds
  return(scoredf)
}

timeConvert <- function(time, qtr) {
  
  timesplit <- unlist(strsplit(time, split = ":"))
  minute <- 14 - as.numeric(timesplit[1])
  second <- 60 - as.numeric(timesplit[2])
  extraseconds <- (qtr-1) * (15*60)
  totalseconds <- extraseconds + (minute*60) + second
  
  return(totalseconds)
}

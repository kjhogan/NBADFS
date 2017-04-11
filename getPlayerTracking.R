getTrackingData <- function(address) {
  web_page <- readLines(address)
  
  ## regex to strip javascript bits and convert raw to csv format
  x1 <- gsub("[\\{\\}\\]]", "", web_page, perl = TRUE)
  x2 <- gsub("[\\[]", "\n", x1, perl = TRUE)
  x3 <- gsub("\"rowSet\":\n", "", x2, perl = TRUE)
  x4 <- gsub(";", ",", x3, perl = TRUE)
  
  # read the resulting csv with read.table()
  nba <- read.table(textConnection(x4), header = T, sep = ",", skip = 2, stringsAsFactors = FALSE)
  return(nba)
}

updateTrackingData <- function() {
  library(tidyverse)
  addressList <- list(pullup_address = "http://stats.nba.com/js/data/sportvu/pullUpShootData.js", 
                      drives_address = "http://stats.nba.com/js/data/sportvu/drivesData.js", defense_address = "http://stats.nba.com/js/data/sportvu/defenseData.js", 
                      passing_address = "http://stats.nba.com/js/data/sportvu/passingData.js", 
                      touches_address = "http://stats.nba.com/js/data/sportvu/touchesData.js", 
                      speed_address = "http://stats.nba.com/js/data/sportvu/speedData.js", rebounding_address = "http://stats.nba.com/js/data/sportvu/reboundingData.js", 
                      catchshoot_address = "http://stats.nba.com/js/data/sportvu/catchShootData.js", 
                      shooting_address = "http://stats.nba.com/js/data/sportvu/shootingData.js")
  df_list <- lapply(addressList, getTrackingData)
    write_csv(df_list[[1]], 'data/pullups.csv')
    write_csv(df_list[[2]], 'data/drives.csv')
    write_csv(df_list[[3]], 'data/defense.csv')
    write_csv(df_list[[4]], 'data/passing.csv')
    write_csv(df_list[[5]], 'data/touches.csv')
    write_csv(df_list[[6]], 'data/speed.csv')
    write_csv(df_list[[7]], 'data/rebounding.csv')
    write_csv(df_list[[8]], 'data/catchshoot.csv')
    write_csv(df_list[[9]], 'data/shooting.csv')
    return(df_list)
}


YearToSeason2 <- function(x) {
  paste0(x - 1, '-', substring(x, 3, 4))
}

ContentToDF2 <- function(content) {
  data <- content$rowSet
  data <- lapply(data, lapply, function(x) ifelse(is.null(x), NA, x))   # Convert nulls to NAs
  data <- data.frame(matrix(unlist(data), nrow = length(data), byrow = TRUE)) # Turn list to data frame
  
  if (length(content$headers) == ncol(data)) {
    colnames(data) <- content$headers
    
  } else { # Multiple levels of headers
    headers <- lapply(colnames(data), function(x) c())
    
    for (col.level in 1:length(content$headers)) {
      col.names <- content$headers[[col.level]]
      
      if ('columnsToSkip' %in% names(col.names)) {
        start <- col.names$columnsToSkip + 1
      } else {
        start <- 1
      }
      span <- col.names$columnSpan
      
      col.ix <- 1
      for (i in seq(from = start, to = ncol(data), by = span)) {
        for (j in i:(i + span - 1)) {
          headers[[j]] <- c(headers[[j]], col.names$columnNames[[col.ix]][1])
        }
        col.ix <- col.ix + 1
      }
    }
    
    colnames(data) <- sapply(headers, function(x) paste(x, collapse = '.'))
  }
  
  return(data)
}

CleanParam2 <- function(param) {
  
  if (param == 'Basic') {
    return('Base')
    
  } else if (param == '100 Possessions') {
    return('Per100Possessions')
    
  } else if (param == 'Per Game') {
    return('PerGame')
    
  } else if (param == 'Per 36 Minutes') {
    return('Per36')
    
  } else if (param == 'Offensive') {
    return('offensive')
    
  } else if (param == 'Defensive') {
    return('defensive')
    
  } else if (param == 'Player') {
    return('player')
    
  } else if (param == 'Team') {
    return('team')
    
  } else {
    return(param)
  }
}

CHARACTER.COLUMNS2 <- c('GROUP_SET', 'GROUP_ID', 'GROUP_NAME', 'PLAYER_ID', 'PLAYER_NAME', 'TEAM_ID', 'TEAM_NAME', 'TEAM_ABBREVIATION', 
                       'CFID', 'CFPARAMS', 'Team_ID', 'Game_ID', 'GAME_DATE', 'MATCHUP', 'WL',
                       'SHOT_TYPE', 'SHOT_CLOCK_RANGE', 'DRIBBLE_RANGE', 'VS_PLAYER_ID', 'VS_PLAYER_NAME', 'COURT_STATUS',
                       'CLOSE_DEF_DIST_RANGE', 'TOUCH_TIME_RANGE', 'PLAYER_NAME_LAST_FIRST', 'CLOSE_DEF_PERSON_ID', 
                       'PlayerIDSID', 'PlayerFirstName', 'PlayerLastName', 'P', 'TeamIDSID', 'TeamName', 
                       'TeamNameAbbreviation', 'TeamShortName', 'name', 'seasonType', 'GROUP_VALUE',
                       'PLAYER_LAST_TEAM_ID', 'PLAYER_LAST_TEAM_ABBREVIATION', 'PLAYER_POSITION', 'DEFENSE_CATEGORY',
                       'GAME_ID', 'TEAM_CITY', 'START_POSITION', 'COMMENT', 'SEASON_ID', 'GAME_ID')
GetTeamHustleStats2 <- function(year, season.type, per.mode, quarter) {
  library(httr)
  request <- GET(
    "http://stats.nba.com/stats/leaguehustlestatsteam",
    query = list(
      College = "",
      Conference = "",
      Country = "",
      DateFrom = "",
      DateTo = "",
      Division = "",
      DraftPick = "",
      GameScope = "",
      GameSegment = "",
      Height = "",
      LastNGames = 0,
      LeagueID = "00",
      Location = "",
      Month = 0,
      OpponentTeamID = 0,
      Outcome = "",
      PORound = 0,
      PaceAdjust = 'N',
      PerMode = CleanParam(per.mode),
      Period = quarter,
      PlayerExperience = "",
      PlayerPosition = "",
      PlusMinus = "N",
      Rank = "N",
      Season = YearToSeason(year),
      SeasonSegment = "",
      SeasonType = season.type,
      ShotClockRange = "",
      TeamID = 0,
      VsConference = "",
      VsDivision = "",
      Weight = ""
    ),
    add_headers('User-Agent' = 'Mozilla/5.0')
  )
  
  content <- content(request, 'parsed')[[3]][[1]]
  stats <- ContentToDF(content)
  
  # Clean data frame
  char.cols <- which(colnames(stats) %in% CHARACTER.COLUMNS)
  stats[, -char.cols] <- sapply(stats[, -char.cols], as.numeric)
  
  return(stats)
  
}



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

GetTeamHustleStats <- function(year, season.type, per.mode, quarter) {
  
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

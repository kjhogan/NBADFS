#get single day of DFS data joined with spread and total odds
#needs "dfslookup.csv" in working directory
#pinn.odds are official spread to use
get_daily_DFS <- function(date){
  
  
  packages <- #need all of these installed including some from github
    c('dplyr',
      'rvest',
      'dplyr',
      'pipeR',
      'stringr',
      'lubridate',
      'reshape2')
  options(warn = -1)
  lapply(packages, library, character.only = T)

  date <- as.Date(date, "%m-%d-%Y")
  day <- format(date, "%d")
  month <- format(date, "%m")
  year <- format(date, "%Y")
  
url <- paste0("https://basketballmonster.com/dfsdailysummary.aspx?date=",year,"-", month, "-", day)

message(url)
dfodds <- data.frame()
dailysumm <- read_html(url) %>% html_nodes("#ContentPlaceHolder1_UpdatePanel2 table table") %>% html_table(fill=T) %>% data.frame()

dailysumm<- dailysumm[dailysumm$Rank != "Rank",]
dailysumm$Price <- sub("\\$","", dailysumm$Price)
dailysumm$Price <- as.numeric(sub("\\,","", dailysumm$Price))
dailysumm$Ratio <- as.numeric(dailysumm$Ratio)
dailysumm$Value <- as.numeric(dailysumm$Value)
dailysumm$pts <- as.numeric(dailysumm$pts)
dailysumm$reb <- as.numeric(dailysumm$reb)
dailysumm$X3 <- as.numeric(dailysumm$X3)
dailysumm$ast <- as.numeric(dailysumm$ast)
dailysumm$stl <- as.numeric(dailysumm$stl)
dailysumm$blk <- as.numeric(dailysumm$blk)
dailysumm$fg. <- as.numeric(dailysumm$fg.)
dailysumm$fga <- as.numeric(dailysumm$fga)
dailysumm$ft. <- as.numeric(dailysumm$ft.)
dailysumm$fta <- as.numeric(dailysumm$fta)
dailysumm$to <- as.numeric(dailysumm$to)
dailysumm$pf <- as.numeric(dailysumm$pf)
dailysumm$mins <- as.numeric(substr(dailysumm$min, 1,2))
dailysumm$date <- date
dailysumm$Opp <- as.character(gsub('@ ', '', dailysumm$Opp))

if(date <= as.Date("11-16-2016", "%m-%d-%Y") && ("Quincy Acy" %in% dailysumm$Name)){
  dailysumm[dailysumm$Name == "Quincy Acy",]$Team <- "DAL"
}

if(date <= as.Date("1-6-2017", "%m-%d-%Y") && ("Mike Dunleavy" %in% dailysumm$Name)){
  dailysumm[dailysumm$Name == "Mike Dunleavy",]$Team <- "CLE"
}

if(date <= as.Date("1-5-2017", "%m-%d-%Y") && ("Kyle Korver" %in% dailysumm$Name)){
  dailysumm[dailysumm$Name == "Kyle Korver",]$Team <- "ATL"
}

if(date <= as.Date("10-30-2016", "%m-%d-%Y") && ("Ersan Ilyasova" %in% dailysumm$Name)){
  dailysumm[dailysumm$Name == "Ersan Ilyasova",]$Team <- "OKC"
}

if(date <= as.Date("10-29-2016", "%m-%d-%Y") && ("Jerami Grant" %in% dailysumm$Name)){
  dailysumm[dailysumm$Name == "Jerami Grant",]$Team <- "PHI"
}

if("Lance Stephenson" %in% dailysumm$Name){
  dailysumm[dailysumm$Name == "Lance Stephenson",]$Team <- "NOR"
}

if("Anthony Bennett" %in% dailysumm$Name){
  dailysumm[dailysumm$Name == "Anthony Bennett",]$Team <- "BKN"
}

if("Archie Goodwin" %in% dailysumm$Name){
  dailysumm[dailysumm$Name == "Archie Goodwin",]$Team <- "NOR"
}

if("Yogi Ferrell" %in% dailysumm$Name){
  dailysumm[dailysumm$Name == "Yogi Ferrell",]$Team <- "BKN"
}

if("Reggie Williams" %in% dailysumm$Name){
  dailysumm[dailysumm$Name == "Reggie Williams",]$Team <- "NOR"
}

if("RJ Hunter" %in% dailysumm$Name){
  dailysumm[dailysumm$Name == "RJ Hunter",]$Team <- "CHI"
}




spreadurl <- paste0("http://www.sportsbookreview.com/betting-odds/nba-basketball/?date=" ,format(date, "%Y%m%d"))
totalsurl <- paste0("http://www.sportsbookreview.com/betting-odds/nba-basketball/totals/?date=" ,format(date, "%Y%m%d"))
message(spreadurl)
message(totalsurl)
dfspreads <- data.frame()
dftotals <- data.frame()
spreadpage <- read_html(spreadurl) %>% html_nodes('#booksData')
totalspage <- read_html(totalsurl) %>% html_nodes('#booksData')

if (length(spreadpage) > 0){
  teams <- spreadpage %>% html_nodes(".team-name") %>% html_text()
  opener <- spreadpage %>% html_nodes("div[id^= 'eventLineOpener-']") %>% html_text() %>% gsub('\\½', '.5', .)
  pinn <- spreadpage %>% html_nodes("div[id*= '-238-']") %>% html_text()  %>% gsub('\\½', '.5', .)
  
  dfspreads <- data.frame(teams, stringsAsFactors = F)
  dfspreads$opp <- dfspreads$teams
  dfspreads$opp[seq(0,length(dfspreads$opp),2)] <- dfspreads$teams[-seq(0,length(dfspreads$teams),2)]
  dfspreads$opp[-seq(0,length(dfspreads$opp),2)] <- dfspreads$teams[seq(0,length(dfspreads$teams),2)]
  dfspreads$date <- date
  
  dfspreads <- transform(dfspreads, opener = colsplit(opener, pattern = "\\s", names=c("odds","juice"))) 
  dfspreads <- transform(dfspreads, pinn = colsplit(pinn, pattern = "\\s", names=c("odds","juice"))) 
  
  
  dfspreads$pinn.odds <- as.numeric(gsub('\\PK-110','0', dfspreads$pinn.odds))
  dfspreads$opener.odds <- as.numeric(gsub('\\PK-110','0', dfspreads$opener.odds ))
  
  
  dfspreads$opener.juice[is.na(dfspreads$opener.juice)] <- as.numeric("-110")
  dfspreads$pinn.juice[is.na(dfspreads$pinn.juice)] <- as.numeric("-110")
}

if (length(totalspage) > 0){
  teams2 <- totalspage %>% html_nodes(".team-name") %>% html_text()
  opener2 <- totalspage %>% html_nodes("div[id^= 'eventLineOpener-']") %>% html_text() %>% gsub('\\½', '.5', .)
  pinn2 <- totalspage %>% html_nodes("div[id*= '-238-']") %>% html_text()  %>% gsub('\\½', '.5', .)
  
  dftotals <- data.frame(teams2, stringsAsFactors = F)
  dftotals$opp <- dftotals$teams
  dftotals$opp[seq(0,length(dftotals$opp),2)] <- dftotals$teams[-seq(0,length(dftotals$teams2),2)]
  dftotals$opp[-seq(0,length(dftotals$opp),2)] <- dftotals$teams[seq(0,length(dftotals$teams2),2)]
  dftotals$opp <- as.character(dftotals$opp)
  
  dftotals <- transform(dftotals, opener = colsplit(opener2, pattern = "\\s", names=c("odds","juice"))) 
  dftotals <- transform(dftotals, pinn = colsplit(pinn2, pattern = "\\s", names=c("odds","totsjuice"))) 
  
  
  dftotals$pinn.totals <- as.numeric(gsub('\\PK-110','0', dftotals$pinn.odds))
  dftotals$opener.totals <- as.numeric(gsub('\\PK-110','0', dftotals$opener.odds ))
  
  
  dftotals$opener.juice[is.na(dftotals$opener.juice)] <- as.numeric("-110")
  dftotals$pinn.totsjuice[is.na(dftotals$pinn.totsjuice)] <- as.numeric("-110")
  dftotals <- dftotals[,c(1,8,7,6)]
  names(dftotals) <- c("teams", "opener", "totals", "totalsjuice")
}
 
 oddsframe <- left_join(dfspreads, dftotals, c("teams" = "teams"))
  lookup <- read.csv("dfslookup.csv", stringsAsFactors = F)
oddsframe <- left_join(oddsframe, lookup, c("teams" = "Odds_Tm"))



dailysumm <- left_join(dailysumm, oddsframe, c("Team" = "DFS_Tm") )
dailysumm <- left_join(dailysumm[,-c(7,26)], lookup, c("opp" = "Odds_Tm"))
colnames(dailysumm)[ncol(dailysumm)] <- "opp_dfsnm"
 return(dailysumm)
}

#use get_Daily_DFS between two dates
get_NBADFS_btwn <- function(start_date, end_date) {
  all_odds <- data.frame()
  stdate <- as.Date(start_date, "%m-%d-%Y")
  enddate <- as.Date(end_date, "%m-%d-%Y")
  
  dates <- seq(stdate, enddate, by = "day")
  days <- c()
  months <- c()
  years <- c()
  
  
  for (i in 1:length(dates)){
    days <- c(days,format(dates[i], "%d"))
    months <- c(months,format(dates[i], "%m"))
    years <- c(years,format(dates[i], "%Y"))
  }
  
  games <- data.frame(dates, days, months, years)
  bad_dates <- c("11-24-2016", "12-24-2016")
  
  for (i in 1:length(games$day)){
    day <- games$day[i]
    month <- games$month[i]
    year <- games$year[i]
    date <- paste0(month,"-",day,"-",year)
    break_flag <- FALSE
    
    break_flag <- ifelse(date %in% bad_dates, TRUE, FALSE)
    
    if (break_flag == TRUE){
      message("No data for date")
    }
    
    else{
      day_odds <- get_daily_DFS(date)
      all_odds <- rbind(all_odds,day_odds)}
  }
   
  
  return(all_odds)
}

#takes original DK dataframe and returns updated dataframe up to most recent date
update_NBADFS <- function(df) {
  firstdate <- max(df$date.y) + 1
  mostrecentdate <- today()-1
  newdata <- get_NBADFS_btwn(firstdate, mostrecentdate)
  newdata <- mutate(newdata, EVP = (.005733*Price) - 4.987)
  newdata <- mutate(newdata, Plus_EV = Value - EVP)
  updateddf <- rbind(df,newdata)
 
  return(updateddf)
}

#get +/- expected value (.005733 x Salary) - 4.987 vs. Actual Value for position
get_Today_PM <- function(df, pos, PMLength, tnt_teams) {
  

  
  cleandk <- filter(df, Price > 0, Value > 1)

  
  pgs <- c("PG", "PG/SG", "PG/SF")
  sgs <- c("SG", "PG/SG", "SG/SF")
  sfs <- c("SF", "SF/PF", "SG/SF")
  pfs <- c("PF", "PF/C", "SF/PF")
  cs <- c("C", "PF/C")
  positionneed <- c()
  if(pos == "PG") { positionneed <- pgs}
  if(pos == "SG") { positionneed <- sgs}
  if(pos == "SF") { positionneed <- sfs}
  if(pos == "PF") { positionneed <- pfs}
  if(pos == "C") { positionneed <- cs}
  

  cleandk %>% filter(Opp %in% tnt_teams, Pos %in% positionneed, date.y >= (today() - PMLength)) %>% 
    group_by(Opp) %>% summarise(AvgPEV = mean(Plus_EV), Cnt = n()) -> cleandk
   return(cleandk)
}


#reads today's salaries/matchups and joins today's spreads and odds
#requires dksalaries.csv from Draftkings.com in working directory
#updates team abbrevations to match
#needs a lookup to clean up names of players to universal format
get_Today_DK <- function(){
  todaydk <- read.csv("data/dksalaries.csv", stringsAsFactors = FALSE)
  todaydk <- mutate_each(todaydk,funs(toupper),teamAbbrev)
  
  if("NO" %in% todaydk$teamAbbrev){
    todaydk[todaydk$teamAbbrev == "NO",]$teamAbbrev <- "NOR"
  }
  
  if("GS" %in% todaydk$teamAbbrev){
    todaydk[todaydk$teamAbbrev == "GS",]$teamAbbrev <- "GSW"
  }
  
  if("NY" %in% todaydk$teamAbbrev){
    todaydk[todaydk$teamAbbrev == "NY",]$teamAbbrev <- "NYK"
  }
  if("SA" %in% todaydk$teamAbbrev){
    todaydk[todaydk$teamAbbrev == "SA",]$teamAbbrev <- "SAS"
  }
  todayodds <- get_TodayOdds()
  todayodds <- todayodds[,c(1,2,3,6,9,11,12)]
  todaydk <- left_join(todaydk, todayodds, c("teamAbbrev" = "dfsnm"))

  
  return(todaydk)
}

#get current days odds (spread and total)
#requires dfslookup.csv
get_TodayOdds <- function() {
  date <- today()
  spreadurl <- paste0("http://www.sportsbookreview.com/betting-odds/nba-basketball/?date=" ,format(date, "%Y%m%d"))
  totalsurl <- paste0("http://www.sportsbookreview.com/betting-odds/nba-basketball/totals/?date=" ,format(date, "%Y%m%d"))
  message(spreadurl)
  message(totalsurl)
  dfspreads <- data.frame()
  dftotals <- data.frame()
  spreadpage <- read_html(spreadurl) %>% html_nodes('#booksData')
  totalspage <- read_html(totalsurl) %>% html_nodes('#booksData')
  
  if (length(spreadpage) > 0){
    teams <- spreadpage %>% html_nodes(".team-name") %>% html_text()
    opener <- spreadpage %>% html_nodes("div[id^= 'eventLineOpener-']") %>% html_text() %>% gsub('\\½', '.5', .)
    pinn <- spreadpage %>% html_nodes("div[id*= '-238-']") %>% html_text()  %>% gsub('\\½', '.5', .)
    
    dfspreads <- data.frame(teams, stringsAsFactors = F)
    dfspreads$opp <- dfspreads$teams
    dfspreads$opp[seq(0,length(dfspreads$opp),2)] <- dfspreads$teams[-seq(0,length(dfspreads$teams),2)]
    dfspreads$opp[-seq(0,length(dfspreads$opp),2)] <- dfspreads$teams[seq(0,length(dfspreads$teams),2)]
    dfspreads$date <- date
    
    dfspreads <- transform(dfspreads, opener = colsplit(opener, pattern = "\\s", names=c("odds","juice"))) 
    dfspreads <- transform(dfspreads, pinn = colsplit(pinn, pattern = "\\s", names=c("odds","juice"))) 
    
    
    dfspreads$pinn.odds <- as.numeric(gsub('\\PK-110','0', dfspreads$pinn.odds))
    dfspreads$opener.odds <- as.numeric(gsub('\\PK-110','0', dfspreads$opener.odds ))
    
    
    dfspreads$opener.juice[is.na(dfspreads$opener.juice)] <- as.numeric("-110")
    dfspreads$pinn.juice[is.na(dfspreads$pinn.juice)] <- as.numeric("-110")
  }
  
  if (length(totalspage) > 0){
    teams2 <- totalspage %>% html_nodes(".team-name") %>% html_text()
    opener2 <- totalspage %>% html_nodes("div[id^= 'eventLineOpener-']") %>% html_text() %>% gsub('\\½', '.5', .)
    pinn2 <- totalspage %>% html_nodes("div[id*= '-238-']") %>% html_text()  %>% gsub('\\½', '.5', .)
    
    dftotals <- data.frame(teams2, stringsAsFactors = F)
    dftotals$opp <- dftotals$teams
    dftotals$opp[seq(0,length(dftotals$opp),2)] <- dftotals$teams[-seq(0,length(dftotals$teams2),2)]
    dftotals$opp[-seq(0,length(dftotals$opp),2)] <- dftotals$teams[seq(0,length(dftotals$teams2),2)]
    dftotals$opp <- as.character(dftotals$opp)
    
    dftotals <- transform(dftotals, opener = colsplit(opener2, pattern = "\\s", names=c("odds","juice"))) 
    dftotals <- transform(dftotals, pinn = colsplit(pinn2, pattern = "\\s", names=c("odds","totsjuice"))) 
    
    
    dftotals$pinn.totals <- as.numeric(gsub('\\PK-110','0', dftotals$pinn.odds))
    dftotals$opener.totals <- as.numeric(gsub('\\PK-110','0', dftotals$opener.odds ))
    
    
    dftotals$opener.juice[is.na(dftotals$opener.juice)] <- as.numeric("-110")
    dftotals$pinn.totsjuice[is.na(dftotals$pinn.totsjuice)] <- as.numeric("-110")
    dftotals <- dftotals[,c(1,8,7,6)]
    names(dftotals) <- c("teams", "opener", "totals", "totalsjuice")
  }
  
  oddsframe <- left_join(dfspreads, dftotals, c("teams" = "teams"))
  lookup <- read.csv("data/dfslookup.csv", stringsAsFactors = F)
  oddsframe <- left_join(oddsframe, lookup, c("teams" = "Odds_Tm"))
  oddsframe <- left_join(oddsframe, lookup, c("opp" = "Odds_Tm"))
  colnames(oddsframe)[ncol(oddsframe)] <- "opp_dfsnm"
  colnames(oddsframe)[ncol(oddsframe)-1] <- "dfsnm"
  return(oddsframe)
}

#get positional summary by using DK dataframe and one of "PG, SG, SF, PF, C" to get opponent +/- summary
#update to get player +/- as well
get_Today_Pos_Summary <- function(df, position) {
  
  library(DT)
  pgs <- c("PG", "PG/SG", "PG/SF")
  sgs <- c("SG", "PG/SG", "SG/SF")
  sfs <- c("SF", "SF/PF", "SG/SF")
  pfs <- c("PF", "PF/C", "SF/PF")
  cs <- c("C", "PF/C")
  positionneed <- c()
  if(position == "PG") { positionneed <- pgs}
  if(position == "SG") { positionneed <- sgs}
  if(position == "SF") { positionneed <- sfs}
  if(position =="PF") { positionneed <- pfs}
  if(position == "C") { positionneed <- cs}
  
  todaysdk <- get_Today_DK()
  teams <- unique(todaysdk$teamAbbrev)
  todaysdk <- filter(todaysdk, Position %in% positionneed)
  oppPM <- get_Today_PM(df, position, 30, teams)
  todaysdk <- left_join(todaysdk, oppPM, c("opp_dfsnm" = "Opp"))
  return(todaysdk)
  
}

#get positional summary by using DK dataframe and one of "PG, SG, SF, PF, C" to get opponent +/- summary
#update to get player +/- as well
get_Today_SummaryTab <- function(df) {
  
  library(DT)
  pgs <- c("PG", "PG/SG", "PG/SF")
  sgs <- c("SG", "PG/SG", "SG/SF")
  sfs <- c("SF", "SF/PF", "SG/SF")
  pfs <- c("PF", "PF/C", "SF/PF")
  cs <- c("C", "PF/C")
 
  
  # todaysdk <- get_Today_DK()
  # teams <- unique(todaysdk$teamAbbrev)
  PGPM <- get_Today_Pos_Summary(df, "PG") %>% mutate(DKPos = "PG")
  SGPM <- get_Today_Pos_Summary(df, "SG") %>% mutate(DKPos = "SG")
  SFPM <- get_Today_Pos_Summary(df, "SF") %>% mutate(DKPos = "SF")
  PFPM <- get_Today_Pos_Summary(df, "PF") %>% mutate(DKPos = "PF")
  CPM <- get_Today_Pos_Summary(df, "C") %>% mutate(DKPos = "C")
  oppPM <- bind_rows(list(PGPM, SGPM, SFPM, PFPM, CPM))
  return(oppPM)
  
  
}

#get positional boxplots for price range by passing positional summary dataframe, main NBA DK dataframe
#requires getting Today_Pos_Summary for each position
get_Today_Box <- function(todaydf, df, minprice, maxprice = 14000) {
  df %>% filter(Name %in% todaydf$Name, Price >= minprice, Price <= maxprice) %>% ggplot(mapping = aes(x = Name, y = Ratio)) + 
    geom_boxplot() + 
    coord_flip()
}

get_Today_PlayerBox <- function(players,df) {

  df %>% filter(Name %in% players) %>% ggplot(mapping = aes(x = Name, y = Ratio, color = Name)) + 
    geom_boxplot() + 
    coord_flip()
}
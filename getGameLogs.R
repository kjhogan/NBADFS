get_Player_List <- function(season = '2016-17') {
  
  library(tidyverse)
  library(jsonlite)
  library(rvest)
  library(stringr)
  player_info <- read_html(paste0('http://stats.nba.com/stats/commonallplayers?LeagueID=00&Season=',season,'&IsOnlyCurrentSeason=1'))
  
  json_data <- fromJSON(html_text(player_info))
  
  data <-
    json_data$resultSets$rowSet %>%
    data.frame(stringsAsFactors = F) %>%
    tbl_df
  
  names(data) <- json_data$resultSets$headers %>% unlist
  data <- filter(data, !(DISPLAY_FIRST_LAST %in% c("Chris Bosh", "Festus Ezeli", "Nikola Pekovic", "Quincy Pondexter", "Ben Simmons", "Tiago Splitter",
                                                   "Isaiah Taylor", "Patricio Garino", "Marcus Georges-Hunt")))
  return(data)
}

get_Player_Gamelog <- function(player_name, seasoninfo){


id.player <- filter(seasoninfo, DISPLAY_FIRST_LAST == player_name) %>% .$PERSON_ID
url_json <-
  'http://stats.nba.com/stats/playergamelog?LeagueID=00&PlayerID=' %>%
  paste0(id.player, '&Season=2016-17&SeasonType=Regular+Season')

gamelog_json <- read_html(url_json) %>% html_text() %>% fromJSON()
if(gamelog_json$resultSets$rowSet == "NULL") {print(gamelog_json)}
gamelog <- gamelog_json$resultSets$rowSet %>%
  data.frame(stringsAsFactors = F) %>%
  tbl_df
names(gamelog) <- gamelog_json$resultSets$headers %>% unlist


gamelog <- gamelog %>%
  mutate_at(vars(Player_ID,MIN:PLUS_MINUS),funs(as.numeric)
  ) %>%
  separate(
    MATCHUP,
    into = c("SLUG.TEAM", "SLUG.OPP"),
    sep = "@|vs.",
    remove = F
  ) %>%
  mutate(
    IS.WIN = WL %>% str_detect("W"),
    GAME_DATE = GAME_DATE %>% lubridate::mdy() %>% as.Date,
    IS.HOME_GAME = MATCHUP %>% str_detect("vs. "),
    IS.VIDEO_AVAILABLE = VIDEO_AVAILABLE %>% str_detect("1"),
    SLUG.TEAM = SLUG.TEAM %>% str_trim,
    SLUG.OPP = SLUG.OPP %>% str_trim,
    NAME.PLAYER = player_name
   ) %>%
  arrange(GAME_DATE) %>%
  mutate(DAYS.REST = GAME_DATE - dplyr::lag(GAME_DATE),
         DAYS.REST = DAYS.REST - 1)
print(paste0('Retrieved gamelog for ', player_name, '.'))
return(gamelog)
}

get_Season_Gamelog <- function(season = '2016-17') {
  
  player_list <- get_Player_List(season)
  print(paste0('Retrieved player info for ', season,'.'))
  player_names <- player_list$DISPLAY_FIRST_LAST
  all_data <-
    player_names %>%
    purrr::map(
      function(x)
        get_Player_Gamelog(
          player_name = x,
          seasoninfo = player_list
        )
    ) %>%
    compact %>%
    bind_rows
  return(all_data)
}

getTeamStats <- function() {
  
  #tmstats <- fromJSON(html_text(read_html('http://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2016-17&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=')))
  tmstats <- fromJSON(html_text(read_html('http://stats.nba.com/stats/leaguehustlestatsplayer?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2016-17&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&Weight=')))
  stats <- data.frame()
  for(i in 1:485){
    tempdf <- as.data.frame(matrix(unlist(tmstats$resultSets[[1]][[3]][[i]]),ncol = 13, byrow = T))
    stats <- rbind(stats, tempdf)
  }
  names(stats) <- unlist(tmstats$resultSets[[1]][[2]])
  stats <- stats %>% mutate_at(vars(AGE:SCREEN_ASSISTS),funs(as.numeric))
  #teams <- sort(unique(stats$TEAM_NAME))
  # conf <- data.frame(TEAM_NAME = teams, CONF = c("E", "E", "E", "E", 
  #                                                "E", "E", "W", "W", 
  #                                                "E", "W", "W", "E", 
  #                                                "W", "W", "W", "E", 
  #                                                "E", "W", "W", "E", 
  #                                                "W", "E", "E", "W",
  #                                                "W", "W", "W", "E", "W", "E"))
  # stats <- left_join(stats, conf)
  return(stats)
}


statsdf %>% filter(CONF == "E") %>% select(TEAM_NAME,W:W_PCT,NET_RATING, TS_PCT) %>% arrange(desc(W)) %>% formattable(list(
       W = color_tile("white", "lightgreen"),
       W_PCT = ~digits(W_PCT,2),
       area(col = c(NET_RATING)) ~ normalize_bar("lightblue", 0.2),
       TS_PCT = ~percent(TS_PCT, 1)
   )) -> formattable1

statsdf %>% ggplot(mapping = aes(x = OFF_RATING, y = DEF_RATING)) + 
  geom_point(mapping = aes(color = NET_RATING, size = NET_RATING),alpha = .5) + 
  geom_hline(yintercept = mean(statsdf$DEF_RATING), linetype = "dashed") + 
  geom_vline(xintercept = mean(statsdf$OFF_RATING), linetype = "dashed") + 
  geom_point(mapping = aes(size = NET_RATING),shape = 1, color = "black") + 
  scale_y_reverse() + fte_theme() -> plot1


library("htmltools")
library("webshot")    

export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}








##for JS

update_advanced_box <- function(){
  link <- "https://stats.nba.com/stats/teamgamelogs?DateFrom=&DateTo=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlusMinus=N&Rank=N&Season=2018-19&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&VsConference=&VsDivision="
  #download.file(link, "boxscores.json")
  web_page <- readLines("boxscores.json")
  
  ##regex to strip javascript bits and convert raw to csv format
  x1 <- gsub("[\\{\\}\\]]", "", web_page, perl=TRUE)
  x2 <- gsub("[\\[]", "\n", x1, perl=TRUE)
  x3 <- gsub("\"rowSet\":\n", "", x2, perl=TRUE)
  x4 <- gsub(";", ",",x3, perl=TRUE)
  advanced_box_current <-read.table(textConnection(x4), header=T, sep=",", fill=T, skip=2, stringsAsFactors=FALSE)
  rm(x1)
  rm(x2)
  rm(x3)
  rm(x4)
  advanced_box_current <- advanced_box_current %>% mutate(SITE = if_else(grepl("@", MATCHUP), "A", "H"))
  
  away_adv <- advanced_box_current %>% filter(SITE == "A") %>% select(TEAM_ID, TEAM_ABBREVIATION, TEAM_NAME, GAME_ID, TM_TOV_PCT)
  names(away_adv) <- c('OPP_ID', 'OPP_ABBREVIATION', 'OPP_NAME', 'GAME_ID', 'OPP_TOV_PCT')
  
  home_adv <- advanced_box_current %>% filter(SITE == "H") %>% select(TEAM_ID, TEAM_ABBREVIATION, TEAM_NAME, GAME_ID, TM_TOV_PCT)
  names(home_adv) <- c('OPP_ID', 'OPP_ABBREVIATION', 'OPP_NAME', 'GAME_ID', 'OPP_TOV_PCT')
  
  home_with_opp <- advanced_box_current %>% filter(SITE == "H") %>% left_join(away_adv)
  away_with_opp <- advanced_box_current %>% filter(SITE == "A") %>% left_join(home_adv)
  advanced_box_current <- rbind(home_with_opp, away_with_opp) 
  advanced_box_current <- advanced_box_current %>% select(SEASON_YEAR:PACE, SITE, OPP_ID, OPP_ABBREVIATION, OPP_NAME, OPP_TOV_PCT)
  return(advanced_box_current)
}

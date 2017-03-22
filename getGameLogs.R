get_Player_List <- function(season = '2016-17') {
  
  library(tidyverse)
  library(jsonlite)
  library(rvest)
  
  player_info <- html(paste0('http://stats.nba.com/stats/commonallplayers?LeagueID=00&Season=',season,'&IsOnlyCurrentSeason=1'))
  
  json_data <- fromJSON(html_text(player_info))
  
  data <-
    json_data$resultSets$rowSet %>%
    data.frame(stringsAsFactors = F) %>%
    tbl_df
  
  names(data) <- json_data$resultSets$headers %>% unlist
  data <- filter(data, !(DISPLAY_FIRST_LAST %in% c("Chris Bosh", "Festus Ezeli", "Nikola Pekovic", "Quincy Pondexter", "Ben Simmons", "Tiago Splitter",
                                                   "Isaiah Taylor")))
  return(data)
}

get_Player_Gamelog <- function(player_name, seasoninfo){


id.player <- filter(seasoninfo, DISPLAY_FIRST_LAST == player_name) %>% .$PERSON_ID
url_json <-
  'http://stats.nba.com/stats/playergamelog?LeagueID=00&PlayerID=' %>%
  paste0(id.player, '&Season=2016-17&SeasonType=Regular+Season')

gamelog_json <- html(url_json) %>% html_text() %>% fromJSON()
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




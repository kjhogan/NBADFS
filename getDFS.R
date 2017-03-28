get_Daily_FD <- function(gamedate) {
  fdurlbase <- "http://rotoguru1.com/cgi-bin/hyday.pl?"
  d <- day(gamedate)
  m <- month(gamedate)
  y <- year(gamedate)
  fdurl <- paste0(fdurlbase,'mon=',m,'&day=',d,'&year=',y,'&game=fd')
  print(fdurl)
  daydata <- read_html(fdurl) %>% html_nodes('table') %>% .[[9]] %>% 
    html_table(fill = T) %>% 
    filter (X1 %in% c('PG', 'SG', 'SF', 'PF', 'C')) 
  daydata$game.date <- gamedate 
  return(daydata)
}

get_FD_Between <- function(dates) {
  
  all_dfs <- data.frame()
  for(i in 1:length(dates)) {
    dfs <- get_Daily_FD(dates[i])
   all_dfs <- rbind(all_dfs, dfs)
  }
  return(all_dfs)
}
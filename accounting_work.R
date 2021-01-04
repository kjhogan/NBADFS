library(rvest)
library(stringr)
read_html("sba1227.html") %>% html_table() %>% .[[4]] -> sba_bets
names(sba_bets) <- c('User', 'GameDate', 'Sport', 'Description', 'Risk', 'WinLoss', 'Result', 'DatePlaced')
sba_bets %<>%
  filter(Sport == "NBA")%>%
  mutate(Risk = as.numeric(substr(Risk,1,3)),
         DatePlaced = substr(DatePlaced,1,10),
         Type = case_when(str_detect(Description,"o2") ~ "Over",
                          str_detect(Description,"u2") ~ "Under",
                          str_detect(Description, "TOTAL", negate = TRUE) ~ "Side"),
         Result = case_when(WinLoss > 0 ~ "Win",
                            WinLoss < 0 ~ "Loss",
                              WinLoss == 0 ~"Push"))

sba_bets %>% write.csv("sba_accounting.csv")
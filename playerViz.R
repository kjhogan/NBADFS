# prices %>% filter(Salary > 5000) %>% ggplot(aes(x=Salary, y = Game)) + 
#   geom_density_ridges(aes(fill = Game), color = "white") + 
#   scale_y_discrete(expand = c(.01, 0)) + 
#   scale_x_continuous(expand = c(.01, 0)) + 
#   labs(title = "FD Prices") + 
#   theme_joy(font_size = 13, grid = TRUE) + 
#   theme (axis.title.y = element_blank(), legend.position = "none") + 
#   scale_fill_viridis(discrete = TRUE, option = "A", alpha = .7)
# 
# fd2017 %>% filter(salary > 5000) %>% ggplot(aes(x=fdpts, y = reorder(opp, fdpts, median), fill = ..x..)) + 
#   geom_density_ridges_gradient(color = "white") + 
#   scale_y_discrete(expand = c(.01, 0)) + 
#   scale_x_continuous(expand = c(.01, 0)) + 
#   labs(title = "FD Prices") + 
#   theme_joy(font_size = 13, grid = TRUE) +
#   scale_fill_viridis(name = "fdpts", option = "C") +
#   theme (axis.title.y = element_blank(), legend.position = "none") + 
#   facet_wrap(~position)
#   

gl_stat_Ridges <- function(players, stat_choice = "PTS"){
  
  game_logs <- read_csv("NBADFS-master/data/gamelogs.csv")
  game_logs <- game_logs %>% filter(NAME.PLAYER %in% players)
  
  
  plot <- game_logs %>% ggplot(aes_string(x=stat_choice, y = paste0("reorder(NAME.PLAYER,",stat_choice,",median)"))) +
    geom_density_ridges(fill = "skyblue4",alpha = .7, color = "white") +
    scale_y_discrete(expand = c(.01, 0)) +
    scale_x_continuous(expand = c(.01, 0)) +
    labs(title = paste0("Player Comparison: ", stat_choice)) +
    theme_joy(font_size = 13, grid = TRUE) +
    theme (axis.title.y = element_blank(), legend.position = "none") 
  

   variance <- game_logs %>% group_by(NAME.PLAYER) %>% summarise_(Avg = interp(~mean(x), x = as.name(stat_choice)),
                                                                  Median = interp(~median(x), x = as.name(stat_choice)),
                                                                  Variance = interp(~var(x), x = as.name(stat_choice)),
                                                                  StandardDev = interp(~sd(x), x = as.name(stat_choice)))
  return_vals <- list(plot, variance)
  return(return_vals)
}
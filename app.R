library(shiny)

dataf <- read_csv("NBADFS-master/data/gamelogs.csv")


ui <- fluidPage(
  titlePanel("Player Comparison"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("posInput", "Position",
                     choices = unique(dataf$Pos),
                     selected = "PG"),
      sliderInput("mpInput", "Minutes Played", 0, maxMP, c(1000, maxMP)),
      selectizeInput("playerInput", "Players",
                  c('Kyrie Irving', 'Damian Lillard', 'Isaiah Thomas', 'Kemba Walker'),choices = unique(dataf$Player), multiple = TRUE)
    ),
    mainPanel(
      plotOutput("testplot", height = "800px"),
      br(), br()
    )
  )
)

server <- function(input,output) {
  
  output$testplot <- renderPlot({
    dataf %>% filter(MP >= input$mpInput[1], MP <= input$mpInput[2], Pos == input$posInput) %>% 
      mutate(Age = round((Age - mean(Age))/sd(Age), 2)*-1, MP = round((MP - mean(MP))/sd(MP), 2),
             FGA = round((FGA - mean(FGA))/sd(FGA), 2),
             FTA = round((FTA - mean(FTA))/sd(FTA), 2),
             x3PA = round((X3PA - mean(X3PA))/sd(X3PA), 2),  
             AST = round((AST - mean(AST))/sd(AST), 2),
             STL = round((STL - mean(STL))/sd(STL), 2),
             TOV = round((TOV - mean(TOV))/sd(TOV), 2)) -> dataf
    
    ranks <- dataf %>% filter(Player %in% input$playerInput) %>% 
      select(Player, Pos, Tm, Age, MP, FGA, FTA, x3PA, AST, STL, TOV) %>% 
      gather(Age:STL, key = "Stat", value = "Performance")
    
    ranks$Stat <- factor(ranks$Stat, levels = c("Age", "MP", "FGA", "x3PA", "FTA", "AST", "STL"))
    ranks %>% ggplot(aes(x=Stat, y=Performance, fill = Player)) + 
      geom_point(stat='identity', aes(col=Player), size = 3)  +
      geom_segment(aes(y = 0, 
                       x = Stat, 
                       yend = Performance, 
                       xend = Stat, color = Player), size = 2 ) +
      scale_x_discrete(limits = rev(levels(ranks$Stat))) +
      scale_y_continuous(breaks = c(-1,0,1), label = c("Poor", "Average", "Great")) +
      facet_wrap(~Player, ncol=2) +
      labs(x="Stat", y="Performance",
           title="Player Comparison",
           subtitle="Performance vs. average player for various statistics",
           caption="data from BBRef") + 
      theme_ipsum() + 
      theme(legend.position = "none", strip.text = element_text(face = "bold", size = 11), panel.grid.major.y = element_blank()) + 
      scale_color_viridis(discrete = TRUE) +
      coord_flip()
  })
  
}


shinyApp(ui = ui, server = server)
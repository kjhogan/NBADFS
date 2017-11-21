library(shiny)
library(DT)
game_logs <- read_csv("NBADFS-master/data/gamelogs.csv")


ui <- fluidPage(
  titlePanel("Player Comparison"),
  sidebarLayout(
    sidebarPanel(selectizeInput("statInput", "Stats",
                                c('PTS'),choices = c('PTS', 'AST', 'REB', 'FGA', 'STL', 'TOV', 'MIN'), multiple = FALSE),

      selectizeInput("playerInput", "Players",
                  c('Kyrie Irving', 'Damian Lillard', 'Isaiah Thomas', 'Kemba Walker'),choices = unique(game_logs$NAME.PLAYER), multiple = TRUE)
    ),
    mainPanel(
      plotOutput("testplot", height = "500px"),
      br(), br(), DT::dataTableOutput("mytable")
    )
  )
)

server <- function(input,output) {
  
  output$testplot <- renderPlot({
 
    
    game_logs <- game_logs %>% filter(NAME.PLAYER %in% input$playerInput)
    
    game_logs %>% ggplot(aes_string(x=input$statInput, y = paste0("reorder(NAME.PLAYER,",input$statInput,",median)"))) +
      geom_density_ridges(fill = "skyblue4",alpha = .8, color = "black") +
      scale_y_discrete(expand = c(.01, 0)) +
      scale_x_continuous(expand = c(.01, 0)) +
      labs(title = paste0("Player Comparison: ", input$statInput)) +
      theme_joy(font_size = 13, grid = TRUE) +
      theme (axis.title.y = element_blank(), legend.position = "none") 
  })
  
  output$mytable = DT::renderDataTable({
    
    game_logs <- game_logs %>% filter(NAME.PLAYER %in% input$playerInput)
    variance <- game_logs %>% group_by(NAME.PLAYER) %>% summarise_(Avg = interp(~mean(x), x = as.name(input$statInput)),
                                                                   Median = interp(~median(x), x = as.name(input$statInput)),
                                                                   Variance = interp(~var(x), x = as.name(input$statInput)),
                                                                   StandardDev = interp(~sd(x), x = as.name(input$statInput)))
    variance
  })
}


shinyApp(ui = ui, server = server)

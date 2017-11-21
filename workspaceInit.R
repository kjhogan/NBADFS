workspace_Init <- function() {
  packages <- #need all of these installed including some from github
    c('tidyverse',
      'stringr',
      'lubridate',
      'ggplot2',
      'jsonlite',
      'ggridges',
      'ggjoy',
      'viridis',
      'lazyeval')
  lapply(packages, library, character.only = T)
}
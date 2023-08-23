library(shiny)
library(shiny.fluent)

source("ui.R")
source("server.R")

# Main app
shinyApp(ui = ui, server = server)
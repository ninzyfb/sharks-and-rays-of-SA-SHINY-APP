# Load necessary libraries if any
library(shiny)

# Load UI function from ui.R
source("ui.R")

# Load server function from server.R
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)

# code below is to deploy the app to shiny server when it was on shinyapps.io
#library(rsconnect)
#deployApp()
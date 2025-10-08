# CalCOFI Interactive Larva App
# Main app entry point

# load global configuration, data, and functions
source("global.R")

# load UI and server
source("ui.R")
source("server.R")

# run the application
shinyApp(ui = ui, server = server)

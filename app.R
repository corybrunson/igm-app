# Source server
source("server.R")

# Source user interface
# http://hutchinson.belmont.ma.us/tth/manual/sec3.html
source("ui.R")

# Run app!
shinyApp(ui = ui, server = server)

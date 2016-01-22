# Source server
source("app/server.R")

# Source user interface
# TeX to html: http://hutchinson.belmont.ma.us/tth/manual/sec3.html
source("app/ui.R")

# Run app!
shinyApp(ui = ui, server = server)

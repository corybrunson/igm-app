# Install and load packages
for (pkg in c("data.table", "shiny", "shinythemes")) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        stopifnot(require(pkg, character.only = TRUE))
    }
}

# Scrape data if necessary
if (!file.exists("igm.rds")) source("scrape.R")

# Load stored survey data
allDat <- readRDS("igm.rds")

# Assign values -1, 0, 1 to Disagree, Uncertain, Agree
vote.agree <- c(
    "Strongly Disagree" = -1, "Disagree" = -1,
    "Uncertain" = 0,
    "Agree" = 1, "Strongly Agree" = 1
)
allDat[, agree := vote.agree[vote]]

# Assign values 0, 1 to Strongly, ~Strongly (caution: 0 for NA)
allDat[, strong := as.numeric(grepl("Strongly", vote))]

# Standardize confidence
allDat[, stdconf := confidence / mean(confidence, na.rm = TRUE)]

# Number of economists polled
allDat[, count := length(vote), by = list(id, question)]

# Source triangle coordinate functions (used in server script)
source("fun_coord.R")

# Source user interface
# http://hutchinson.belmont.ma.us/tth/manual/sec3.html
source("ui_tabs.R")

# Source server
source("server_tabs.R")

# Run app!
shinyApp(ui = ui, server = server)

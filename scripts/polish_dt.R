# Install and load packages
if(FALSE) {
for (pkg in c("data.table", "shiny", "shinythemes")) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        stopifnot(require(pkg, character.only = TRUE))
    }
}
}
library(data.table)
library(shiny)
library(shinythemes)

# Scrape data if necessary
if (!file.exists("data/igm.rds")) source("scripts/scrape.R")

# Load stored survey data
allDat <- readRDS("data/igm.rds")

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

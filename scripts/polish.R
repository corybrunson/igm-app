# Install and load packages
library(shiny)
library(shinythemes)

# Load stored survey data
load("data/igmpanel.rda")
allDat <- igmpanel
rm(igmpanel)

# Assign values -1, 0, 1 to Disagree, Uncertain, Agree
vote.agree <- c(
    "Strongly Disagree" = -1, "Disagree" = -1,
    "Uncertain" = 0,
    "Agree" = 1, "Strongly Agree" = 1
)
allDat$agree <- vote.agree[as.character(allDat$vote)]

# Factorize vote field
allDat$vote <- factor(allDat$vote, levels = names(vote.agree))

# Assign values 0, 1 to Strongly, ~Strongly (caution: 0 for NA)
allDat$strong <- as.numeric(grepl("Strongly", allDat$vote))

# Standardize confidence
allDat$stdconf <- allDat$confidence / mean(allDat$confidence, na.rm = TRUE)

# Logit-transform confidence values
allDat$logit.conf <- qlogis(allDat$confidence / 11)

# Strength-specific distributions
allDat$strength <- factor(
    ifelse(is.na(allDat$vote), NA,
           ifelse(grepl("[Aa]gree", allDat$vote),
                  ifelse(grepl("^Strongly", allDat$vote), "2", "1"),
                  "0")),
    levels = as.character(0:2)
)
mean.conf <- sapply(levels(allDat$strength), function(lev) {
    mean(allDat[allDat$strength == lev, ]$logit.conf, na.rm = TRUE)
})
sd.conf <- sd(allDat$logit.conf, na.rm = TRUE)

# Standardized confidence (logit-center-inverse logit)
allDat$stdlogitconf <- plogis(
    (allDat$logit.conf - mean.conf[allDat$strength]) / sd.conf
) * 11

# Remove unnecessary values
allDat$strength <- NULL

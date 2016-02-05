### Traditional visualization of IGM Likert data

# Set working directory
setwd("~/Documents/edu/R/igm/igm-app")

# Packages
pkgs <- c("data.table", "reshape2", "likert")
for (pkg in pkgs) {
    if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        stopifnot(require(pkg, character.only = TRUE))
    }
}

# Load data
dat <- readRDS("data/igm.rds")

# Matrix of Likert responses
mat <- dcast(dat[, .(panelist, id, topic, question, vote)],
             panelist ~ topic + question, fill = NA,
             fun.aggregate = unique, value.var = "vote")
rownames(mat) <- mat$panelist
mat[, panelist := NULL]
# Factorize
levs <- c("Strongly Disagree", "Disagree",
          "Uncertain",
          "Agree", "Strongly Agree")
for (j in 1:ncol(mat)) mat[[j]] <- factor(mat[[j]], levels = levs)

# Likertify
lik <- likert(as.data.frame(mat))

# Visualize
plot(lik)


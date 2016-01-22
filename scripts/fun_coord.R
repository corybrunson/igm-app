# Triangle plot coordinates (given vote counts n = a + b + c)
x <- function(vec) {
    s <- vec[1] + vec[2] + vec[3]
    vec[1] <- vec[1] / s; vec[2] <- vec[2] / s; vec[3] <- vec[3] / s
    sqrt(3)/2 * vec[3] - 1/(2*sqrt(3))
}
y <- function(vec) {
    s <- vec[1] + vec[2] + vec[3]
    vec[1] <- vec[1] / s; vec[2] <- vec[2] / s; vec[3] <- vec[3] / s
    (vec[2] - vec[1]) / 2
}

# Consensus calculation
consensus <- function(agree) {
    # Number of certain responses
    len <- length(which(agree != 0 & !is.na(agree)))
    # Concordance minus discordance
    res <- sum(apply(combn(agree, 2), 2, prod), na.rm = TRUE) /
        # Divided by number of pairs
        sum(apply(combn(abs(agree), 2), 2, prod), na.rm = TRUE) *
        # Controlling for minimum possible
        (len - 1) + 1
    res / len
}
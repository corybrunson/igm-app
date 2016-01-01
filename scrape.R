### Scrape IGM poll results

## SETUP

# load packages
pkgs <- c('rvest', 'data.table')
for(pkg in pkgs) {
    if(!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
        stopifnot(require(pkg, character.only = TRUE))
    }
}

## FUNCTIONS

# Expand a survey page ID to its URL
survey.url <- function(id) {
    paste0('http://www.igmchicago.org/igm-economic-experts-panel',
           '/poll-results?SurveyID=SV_', id)
}

# Restrict a survey page URL to its ID
survey.id <- function(url) {
    gsub('^.*SV_([A-Za-z0-9]{15}$)', '\\1', url)
}

# Put last name first
name.alph <- function(name) {
    paste(gsub('^(.*) ([^ ]+)$', '\\2, \\1', name))
}

# Scrape the home page for survey page URLs
get.surveys <- function(url) {
    page <- read_html(url)
    # selectorgadget
    elts <- html_nodes(page, 'h2 a')
    dates <- html_nodes(page, 'h6')
    # end selectorgadget
    data.table(
        topic = elts %>% html_text(),
        id = survey.id(elts %>% html_attr('href')),
        date = as.Date(gsub(
            '^[A-Z][a-z]+, ([A-Z].*[0-9]) [0-9]{1,2}\\:[0-9]{2}(a|p)m$', '\\1',
            dates %>% html_text()
        ), format = '%B %d, %Y')
    )
}

# Collapse NA vote options
pool.na <- function(vote) {
    vote[!grepl('[Aa]gree|Uncertain', vote)] <- NA
    vote
}

# Scrape a survey page for panelist responses
get.responses <- function(url) {
    page <- read_html(url)
    # selectorgadget
    panelist <- html_nodes(page, '.response-name a')
    uni <- html_nodes(page, '.parent-row td:nth-child(2)')
    vote <- html_nodes(page, '#sort0 span')
    conf <- html_nodes(page, '.confCell')
    comm <- html_nodes(page, '.gridComment')
    # end selectorgadget
    data.table(
        panelist = name.alph(gsub('(\\n|\\t)', '', panelist %>% html_text())),
        uni = uni %>% html_text(),
        vote = pool.na(gsub('(\\n|\\t)', '', vote %>% html_text())),
        confidence = as.numeric(conf %>% html_text()),
        comment = gsub('(\\n|\\t)', '', comm %>% html_text())
    )
}

# Combine ".surveyQuestion" and "p" info into vector of question texts
list_questions <- function(ques, p, url) {
    # Extract texts
    ques_text <- as.vector(ques %>% html_text())
    p_text <- if (length(p) > 6) {
        as.vector(p[1:(length(p) - 6)] %>% html_text())
    } else c()
    # Clean texts
    re <- if (length(ques) == 1) "^\\n *([A-Z].*)$" else {
        "^\\n *Question [A-D]:\\n *([A-Z].*)$"
    }
    ques_text <- gsub(re, '\\1', ques_text)
    p_text <- p_text[gsub("\\n", "", p_text) != ""]
    # p must contain exactly as many statements as are missing from ques
    ques_grep <- grep("^\\n *(Question [A-D]:\\n *){0,1}", ques_text)
    if (length(p_text) != length(ques_grep)) {
        print(url)
        stop()
    }
    # More cleaning
    ques_text <- gsub("\\n", " ", ques_text)
    ques_text <- gsub(" *$", "", ques_text)
    # Fill missing ques entries with p
    if (length(ques_grep) > 0) ques_text[ques_grep] <- p_text
    # Return!
    ques_text
}

# Scrape a survey page for panelist responses,
# careful to separate multiple questions
get.responses <- function(url) {
    page <- read_html(url)
    # Count the number of questions
    ques <- html_nodes(page, '.surveyQuestion')
    p <- html_nodes(page, "p")
    ques_text <- list_questions(ques, p, url)
    # Collect responses for each question
    dats <- lapply(0:(length(ques) - 1), function(r) {
        # Collect and clean fields
        panelist <- html_nodes(page, paste0('#sort', r, ' .response-name a'))
        panelist <- name.alph(gsub('(\\n|\\t)', '',
                                   panelist %>% html_text()))
        uni <- html_nodes(page,
                          paste0('#sort', r, ' .parent-row td:nth-child(2)'))
        uni <- uni %>% html_text()
        edit <- html_nodes(page,
                           paste0('#sort', r,
                                  ' .tablesorter-childRow td:nth-child(1)'))
        edit <- edit %>% html_text()
        vote <- html_nodes(page, paste0('#sort', r, ' span'))
        vote <- gsub('(\\n|\\t)', '', vote %>% html_text())
        conf <- html_nodes(page, paste0('#sort', r, ' .confCell'))
        conf <- as.numeric(conf %>% html_text())
        comm <- html_nodes(page, paste0('#sort', r, ' .gridComment'))
        comm <- gsub('(\\n|\\t)', '', comm %>% html_text())
        # Shift cells back when late joiners are added
        late <- grep('^Joined', edit)
        if(length(late) > 0) {
            vote <- vote[-late]
            conf <- conf[-late]
            comm <- comm[-late]
        }
        # Use only revotes when they are cast
        revote <- grep('^Revote', edit)
        if(length(revote) > 0) {
            vote <- vote[-(revote - 1)]
            conf <- conf[-(revote - 1)]
            comm <- comm[-(revote - 1)]
        }
        # Combine into a data.table
        data.table(
            panelist = panelist,
            uni = uni,
            vote = pool.na(vote),
            confidence = conf,
            comment = comm,
            question = if(length(ques) > 1) LETTERS[r + 1] else '',
            statement = ques_text[r + 1]
        )
    })
    # Check that no more responses remain
    panelist <- html_nodes(page,
                           paste0('#sort', length(ques), ' .response-name a'))
    if(length(panelist) > 0)
        stop('Conflicting question and response counts')
    rbindlist(dats)
}

# Convert vote options to numerical scale
numerical.vote <- function(
    vote,
    char.scale = c('Strongly Disagree', 'Disagree', 'Uncertain',
                   'Agree', 'Strongly Agree'),
    num.scale = c(-1.5, -1, 0, 1, 1.5)
) {
    if(length(num.scale) != length(char.scale))
        stop('Scales much have same length')
    # Initialize NA vector
    nv <- rep(NA, length(vote))
    # Assign numerical values to the five meaningful answers
    for(s in 1:length(char.scale)) {
        nv[which(vote == char.scale[s])] <- num.scale[s]
    }
    nv
}

## DO IT

# Get a list of survey page IDs
home.url <- 'http://www.igmchicago.org/igm-economic-experts-panel'
surveys <- get.surveys(home.url)

# Scrape responses to each survey
responses <- lapply(1:nrow(surveys), function(i) {
    dat <- get.responses(survey.url(surveys$id[i]))
    dat[, topic := surveys$topic[i]]
    dat[, date := surveys$date[i]]
    dat[, id := surveys$id[i]]
    dat
})
# IDs with interesting urls
# bg4hVAJ9aPP4YzX
# 9yTmaqlHpp1JzH7

# Bind responses
response.dat <- rbindlist(responses)
# Organize by date, panelist, uni, vote, confidence
setkey(response.dat, date, id, topic, question, uni, panelist, vote, confidence)

# Suffix duplicate topics with parenthesized arabic numerals
uniq.dat <- unique(response.dat[, c('topic', 'id', 'date'), with = FALSE])
dupes <- names(which(table(uniq.dat$topic) > 1))
for(dupe in dupes) {
    w <- which(uniq.dat$topic == dupe)
    o <- order(uniq.dat$date[w])
    for(i in 1:length(o)) {
        response.dat$topic[which(
            response.dat$topic == dupe &
                response.dat$id == uniq.dat$id[w[o[i]]])] <-
            paste0(dupe, ' (', i, ')')
    }
}

# Resolve statement inconsistencies
stopifnot(nrow(unique(response.dat[, .(id, question)])) ==
              nrow(unique(response.dat[, .(id, question, statement)])))

# Add numerical vote field
response.dat[, num.vote := numerical.vote(response.dat$vote)]

# Save RDS-style for Shiny apps
saveRDS(response.dat, file = "igm.rds")


rm(list = ls())

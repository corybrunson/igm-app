# Load and polish data
source("scripts/polish.R")

# Source triangle coordinate functions (used in server script)
source("scripts/fun_coord.R")

library(dplyr)

# Server
server <- function(input, output) {
    
    # Strength-confidence plot
    output$strconf <- renderPlot({
        
        # Retrieve highlighted data
        dat <- allDat
        
        # Confidence variable
        dat$conf_var <- if (input$conf.var == "Raw") {
            dat$confidence
        } else {
            dat$logit.conf
        }
        
        # Confidence distribution across vote options
        boxplot(conf_var ~ factor(vote), data = dat, outline = FALSE,
                xlab = "Vote",
                ylab = if (input$conf.var == "Raw") "Confidence" else
                    "Logit-Confidence")
        points(x = jitter(as.numeric(factor(dat$vote))),
               y = jitter(dat$conf_var),
               pch = 16, cex = .33, col = rgb(0, 0, 0, .33))
        
        # Standard deviations
        mean_sd <- summarise(
            group_by(dat, vote),
            mean = mean(conf_var, na.rm = TRUE),
            sd = sd(conf_var, na.rm = TRUE)
        )
        mean_sd <- mean_sd[order(mean_sd$vote, na.last = NA), ]
        means_sds <- paste0(sprintf("%.1f", round(mean_sd$mean, 1)), " (",
                            sprintf("%.1f", round(mean_sd$sd, 1)), ")")
        
        # Axis
        axis(side = 3, at = 1:5, tck = 0, labels = means_sds)
        mtext(side = 3, line = 3, text = "Mean (std dev)")
    })
    
    # Highlight by key strings
    hlDat <- reactive({
        
        # Obtain "raw" data
        dat <- allDat
        
        # Spotlight panelists
        if (input$panelist == "-") {
            dat$hl.panelist <- FALSE
        } else {
            dat$hl.panelist <- (dat$panelist == input$panelist)
        }
        
        # Spotlight topics
        dat$hl.topic <- if (input$topic == "") {
            TRUE
        } else {
            grepl(input$topic, paste(dat$topic, dat$statement))
        }
        #if (all(dat$hl.topic)) dat$hl.topic <- FALSE
        
        # Group by ID and question
        by_id_question <- group_by(dat, id, question)
        dat <- mutate(by_id_question,
                      hl.panelist = any(hl.panelist),
                      hl.topic = any(hl.topic))
        
        # Return
        dat
    })
    
    # Assign point coordinates
    xyDat <- reactive({
        
        # Retrieve highlighted data
        dat <- hlDat()
        
        # Group by ID and question
        by_id_question <- group_by(dat, id, question)
        
        # Add vs and triangle coordinates to data table
        dat <- mutate(
            by_id_question,
            X = sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                        as.numeric(agree == 0), na.rm = TRUE) /
                sum(1 - input$conf.wt + input$conf.wt * stdconf, na.rm = TRUE),
            Y = consensus(agree * (1 + input$str.wt * strong)),
            #Y = sum(apply(combn(sign(agree), 2), 2, prod), na.rm = TRUE) /
            #    choose(length(which(agree != 0 & !is.na(agree))), 2),
            x = x(c(sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                            (1 + input$str.wt * strong) *
                            as.numeric(agree == -1),
                        na.rm = TRUE),
                    sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                            (1 + input$str.wt * strong) *
                            as.numeric(agree == 1),
                        na.rm = TRUE),
                    sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                            as.numeric(agree == 0), na.rm = TRUE))),
            y = y(c(sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                            (1 + input$str.wt * strong) *
                            as.numeric(agree == -1),
                        na.rm = TRUE),
                    sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                            (1 + input$str.wt * strong) *
                            as.numeric(agree == 1),
                        na.rm = TRUE),
                    sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                            as.numeric(agree == 0), na.rm = TRUE)))
        )
        
        # Return
        dat
    })
    
    # Collapse data over panelists
    uniqDat <- reactive({
        
        # Retrieve weighted data
        dat <- xyDat()
        
        # Tally panelists
        num <- summarise(
            group_by(dat, id, question),
            count = n()
        )
        
        # Only one entry per question
        dat <- unique(subset(dat,
                             select = c(id, date, question, topic, statement,
                                        hl.panelist, hl.topic,
                                        X, Y, x, y)))
        
        # Check row count
        stopifnot(nrow(dat) == nrow(num))
        output$q <- renderText(nrow(dat))
        
        # Merge count into dat
        dat <- merge(dat, num, by = c("id", "question"))
        
        # Subset
        if (any(dat$hl.panelist) & !input$p_subset)
            dat <- dat[hl.panelist == TRUE, ]
        if (!input$t_subset) # & any(dat$hl.topic)
            dat <- dat[hl.topic == TRUE, ]
        #if (all(dat$hl.topic)) dat$hl.topic <- FALSE
        
        dat
    })
    
    # Versus plot range
    range_vs <- reactiveValues(x = c(0, 1), y = c(0, 1))
    
    # Versus plot!
    output$vs <- renderPlot({
        
        # Retrieve uniquified data
        dat <- uniqDat()
        
        # Plotting window
        Xran <- range_vs$x
        Yran <- range_vs$y
        plot(x = c(), y = c(), bty = "n",
             xlim = Xran, ylim = Yran,
             xlab = "Uncertainty", ylab = "Consensus")
        #abline(h = 0, lty = 3, col = rgb(0, 0, 0, sqrt(input$alpha)))
        
        # Points!
        points(x = dat$X, y = dat$Y,
               pch = 19, cex = input$cex.base * sqrt(dat$count),
               col = if (!input$t_subset) {
                   rgb(0, 0, 0, input$alpha)
               } else {
                   rgb(ifelse(dat$hl.topic, 1, 0), 0, 0, input$alpha)
               })
        
        # Circles
        if (!!input$p_subset) {
            points(x = dat$X, y = dat$Y,
                   pch = ifelse(dat$hl.panelist, 1, NA),
                   cex = 1.5 * input$cex.base * sqrt(dat$count),
                   col = rgb(0, 0, 1, sqrt(input$alpha)))
        }
        
        # Legend
        n_questions <- nrow(unique(subset(dat, select = c(id, date, question))))
        legend("topright",
               legend = paste("Showing", n_questions, "survey questions"),
               pch = NA, box.lty = 0)
    })
    
    # Versus plot zoom
    observeEvent(input$plot_dblclick_vs, {
        
        # Assign brush values
        brush_val <- input$plot_brush_vs
        
        # If brush, zoom; otherwise, reset
        if (is.null(brush_val)) {
            range_vs$x <- c(0, 1)
            range_vs$y <- c(0, 1)
        } else {
            range_vs$x <- c(brush_val$xmin, brush_val$xmax)
            range_vs$y <- c(brush_val$ymin, brush_val$ymax)
        }
    })
    
    # Versus plot range
    param_tri <- reactiveValues(x = range(x(c(1, 0, 0)), x(c(0, 0, 1))),
                                y = range(y(c(1, 0, 0)), y(c(0, 1, 0))),
                                asp = 1)
    
    # Triangle plot!
    output$tri <- renderPlot({
        
        # Retrieve uniquified data
        dat <- uniqDat()
        
        # Plotting window
        xran <- param_tri$x
        yran <- param_tri$y
        plot(x = c(), y = c(), axes = FALSE,
             asp = param_tri$asp,
             xlim = xran + .1 * c(-1, 1) * diff(xran),
             ylim = yran + 0 * c(-1, 1) * diff(yran),
             xlab = "", ylab = "")
        
        # Triangular frame
        for (i in 1:3) {
            vec0 <- diag(rep(1, 3))[, i]
            vec1 <- diag(rep(1, 3))[, i %% 3 + 1]
            segments(x0 = x(vec0), x1 = x(vec1),
                     y0 = y(vec0), y1 = y(vec1),
                     lty = 1, lwd = 1,
                     col = rgb(0, 0, 0, sqrt(input$alpha)))
        }
        
        # Corner labels
        text(x = apply(diag(rep(1, 3)), 2, x),
             y = apply(diag(rep(1, 3)), 2, y),
             labels = c("Disagree", "Agree", "Uncertain"),
             pos = 3 + c(-1, -1, 1), cex = 1.2)
        
        # Points!
        points(x = dat$x, y = dat$y,
               pch = 19, cex = input$cex.base * sqrt(dat$count),
               col = if (!input$t_subset) {
                   rgb(0, 0, 0, input$alpha)
               } else {
                   rgb(ifelse(dat$hl.topic, 1, 0), 0, 0, input$alpha)
               })
        
        # Circles
        if (!!input$p_subset) {
            points(x = dat$x, y = dat$y,
                   pch = ifelse(dat$hl.panelist, 1, NA),
                   cex = 1.5 * input$cex.base * sqrt(dat$count),
                   col = rgb(0, 0, 1, sqrt(input$alpha)))
        }
        
        # Legend
        n_questions <- nrow(unique(subset(dat, select = c(id, date, question))))
        legend("topright",
               legend = paste("Showing", n_questions, "survey questions"),
               pch = NA, box.lty = 0)
    })
    
    # Triangle plot zoom
    observeEvent(input$plot_dblclick_tri, {
        
        # Assign brush values
        brush_val <- input$plot_brush_tri
        
        # If brush, zoom; otherwise, reset
        if (is.null(brush_val)) {
            param_tri$x <- range(x(c(1, 0, 0)), x(c(0, 0, 1)))
            param_tri$y <- range(y(c(1, 0, 0)), y(c(0, 1, 0)))
            param_tri$asp <- 1
        } else {
            param_tri$x <- c(brush_val$xmin, brush_val$xmax)
            param_tri$y <- c(brush_val$ymin, brush_val$ymax)
            param_tri$asp <- NA
        }
    })
    
    # Interactive data table
    output$plot_topics <- renderDataTable({
        
        # Which points
        res <- if (input$tab == "vs") {
            if (is.null(input$plot_brush_vs)) {
                nearPoints(uniqDat(), input$plot_click_vs, "X", "Y")
            } else {
                brushedPoints(uniqDat(), input$plot_brush_vs, "X", "Y")
            }
        } else if (input$tab == "tri") {
            if (is.null(input$plot_brush_tri)) {
                nearPoints(uniqDat(), input$plot_click_tri, "x", "y")
            } else {
                brushedPoints(uniqDat(), input$plot_brush_tri, "x", "y")
            }
        } else if (input$tab %in% c("intro", "discuss", "strconf")) {
            data.frame()
        } else {
            uniqDat()
        }
        if (nrow(res) == 0) return()
        
        # Links
        res$date_link <- paste0('<a href="',
                                "http://www.igmchicago.org/",
                                "igm-economic-experts-panel",
                                "/poll-results?SurveyID=SV_",
                                res$id,
                                '" target="_blank">',
                                gsub("^0", "", format(res$date, "%d %b %Y")),
                                '</a>')
        
        # Format
        res[order(res$date),
            .(Date = date_link,
              Topic = topic,
              Question = ifelse(question == "", "-", question),
              Statement = statement)]
        
    }, escape = FALSE, options = list(paging = FALSE, searching = FALSE))
}

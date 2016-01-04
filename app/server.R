# Load and polish data
source("scripts/polish.R")

# Source triangle coordinate functions (used in server script)
source("scripts/fun_coord.R")

# Server
server <- function(input, output) {
    
    # Highlight by key strings
    hlDat <- reactive({
        
        # Obtain "raw" data
        dat <- allDat
        
        # Spotlight panelists
        if (input$panelist == "-") {
            dat[, hl.panelist := FALSE]
        } else {
            dat[, hl.panelist := panelist == input$panelist]
        }
        dat[, hl.panelist := any(hl.panelist), by = list(id, question)]
        
        # Spotlight topics
        dat[, hl.topic := grepl(tolower(input$topic),
                                tolower(paste(topic, statement)))]
        if (all(dat$hl.topic)) dat$hl.topic <- FALSE
        dat[, hl.topic := any(hl.topic), by = list(id, question)]
        
        # Return
        dat
    })
    
    # Assign point coordinates
    xyDat <- reactive({
        
        # Retrieve highlighted data
        dat <- hlDat()
        
        # Add vs coordinates to data table
        dat[, X := sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                           as.numeric(agree == 0), na.rm = TRUE) /
                sum(1 - input$conf.wt + input$conf.wt * stdconf, na.rm = TRUE),
            by = list(id, question)]
        dat[, Y := sum(apply(combn(sign(agree), 2), 2, prod), na.rm = TRUE) /
                choose(length(which(agree != 0 & !is.na(agree))), 2),
            by = list(id, question)]
        
        # Add triangle coordinates to data table
        dat[, x := x(c(sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                               (1 + input$str.wt * strong) *
                               as.numeric(agree == -1),
                           na.rm = TRUE),
                       sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                               (1 + input$str.wt * strong) *
                               as.numeric(agree == 1),
                           na.rm = TRUE),
                       sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                               as.numeric(agree == 0), na.rm = TRUE))),
            by = list(id, question)]
        dat[, y := y(c(sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                               (1 + input$str.wt * strong) *
                               as.numeric(agree == -1),
                           na.rm = TRUE),
                       sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                               (1 + input$str.wt * strong) *
                               as.numeric(agree == 1),
                           na.rm = TRUE),
                       sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                               as.numeric(agree == 0), na.rm = TRUE))),
            by = list(id, question)]
        
        # Return
        dat
    })
    
    # Assign weighting segment coordinates
    wtDat <- reactive({
        
        # Retrieve coordinatized data
        dat <- xyDat()
        
        # If not using segments, ignore strength and confidence scales
        if (!(input$str.tr | input$conf.tr)) dat
        
        # Add endpoint coordinates to data table
        # str.wt = 0, conf.wt = custom
        dat[, x.s0 := x(c(sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                                  as.numeric(agree == -1), na.rm = TRUE),
                          sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                                  as.numeric(agree == 1), na.rm = TRUE),
                          sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                                  as.numeric(agree == 0), na.rm = TRUE))),
            by = list(id, question)]
        dat[, y.s0 := y(c(sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                                  as.numeric(agree == -1), na.rm = TRUE),
                          sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                                  as.numeric(agree == 1), na.rm = TRUE),
                          sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                                  as.numeric(agree == 0), na.rm = TRUE))),
            by = list(id, question)]
        # str.wt = 1, conf.wt = custom
        dat[, x.s1 := x(c(sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                                  (1 + strong) * as.numeric(agree == -1),
                              na.rm = TRUE),
                          sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                                  (1 + strong) * as.numeric(agree == 1),
                              na.rm = TRUE),
                          sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                                  as.numeric(agree == 0), na.rm = TRUE))),
            by = list(id, question)]
        dat[, y.s1 := y(c(sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                                  (1 + strong) * as.numeric(agree == -1),
                              na.rm = TRUE),
                          sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                                  (1 + strong) * as.numeric(agree == 1),
                              na.rm = TRUE),
                          sum((1 - input$conf.wt + input$conf.wt * stdconf) *
                                  as.numeric(agree == 0), na.rm = TRUE))),
            by = list(id, question)]
        # str.wt = custom, conf.wt = 0
        dat[, x.c0 := x(c(sum((1 + input$str.wt * strong) *
                                  as.numeric(agree == -1), na.rm = TRUE),
                          sum((1 + input$str.wt * strong) *
                                  as.numeric(agree == 1), na.rm = TRUE),
                          sum(as.numeric(agree == 0), na.rm = TRUE))),
            by = list(id, question)]
        dat[, y.c0 := y(c(sum((1 + input$str.wt * strong) *
                                  as.numeric(agree == -1), na.rm = TRUE),
                          sum((1 + input$str.wt * strong) *
                                  as.numeric(agree == 1), na.rm = TRUE),
                          sum(as.numeric(agree == 0), na.rm = TRUE))),
            by = list(id, question)]
        # str.wt = custom, conf.wt = 1
        dat[, x.c1 := x(c(sum(stdconf * (1 + input$str.wt * strong) *
                                  as.numeric(agree == -1), na.rm = TRUE),
                          sum(stdconf * (1 + input$str.wt * strong) *
                                  as.numeric(agree == 1), na.rm = TRUE),
                          sum(stdconf * as.numeric(agree == 0), na.rm = TRUE))),
            by = list(id, question)]
        dat[, y.c1 := y(c(sum(stdconf * (1 + input$str.wt * strong) *
                                  as.numeric(agree == -1), na.rm = TRUE),
                          sum(stdconf * (1 + input$str.wt * strong) *
                                  as.numeric(agree == 1), na.rm = TRUE),
                          sum(stdconf * as.numeric(agree == 0), na.rm = TRUE))),
            by = list(id, question)]
        
        # Return
        dat
    })
    
    # Collapse data over panelists
    uniqDat <- reactive({
        
        # Retrieve weighted data
        dat <- wtDat()
        
        # Only one entry per question
        dat <- unique(dat[, .(id, date, question, topic, statement, count,
                              hl.panelist, hl.topic,
                              X, Y,
                              x.s0, y.s0, x.s1, y.s1, x.c0, y.c0, x.c1, y.c1,
                              x, y)])
        output$q <- renderText({ nrow(dat) })
        
        # Subset
        if (any(dat$hl.panelist) & input$p_subset) dat <- dat[hl.panelist == T]
        if (any(dat$hl.topic) & input$t_subset) dat <- dat[hl.topic == T]
        if (all(dat$hl.topic)) dat$hl.topic <- FALSE
        
        dat
    })
    
    # Versus plot!
    output$vs <- renderPlot({
        
        # Retrieve uniquified data
        dat <- uniqDat()
        
        # Plotting window
        Xran <- c(0, 1)
        Yran <- c(-1/5, 1)
        plot(x = c(), y = c(),
             xlim = Xran, ylim = Yran,
             xlab = "Uncertainty", ylab = "Consensus")
        abline(h = 0, lty = 3, col = rgb(0, 0, 0, sqrt(input$alpha)))
        
        # Points!
        points(x = dat$X, y = dat$Y,
               pch = 19, cex = input$cex.base * sqrt(dat$count),
               col = if (input$t_subset) {
                   rgb(0, 0, 0, input$alpha)
               } else {
                   rgb(ifelse(dat$hl.topic, 1, 0), 0, 0, input$alpha)
               })
        
        # Circles
        if (!input$p_subset) {
            points(x = dat$X, y = dat$Y,
                   pch = ifelse(dat$hl.panelist, 1, NA),
                   cex = 1.5 * input$cex.base * sqrt(dat$count),
                   col = rgb(0, 0, 1, sqrt(input$alpha)))
        }
        
        # Legend
        n_questions <- length(unique(dat$question))
        legend("topright",
               legend = paste("Showing", n_questions, "survey questions"),
               pch = NA, box.lty = 0)
    })
    
    # Consensus triangle plot!
    output$triangle <- renderPlot({
        
        # Retrieve uniquified data
        dat <- uniqDat()
        
        # Plotting window
        xran <- range(xsgn * x(c(1, 0, 0)), xsgn * x(c(0, 0, 1)))
        yran <- range(y(c(1, 0, 0)), y(c(0, 1, 0)))
        plot(x = c(), y = c(), asp = 1, axes = FALSE,
             xlim = xran + .1 * c(-1, 1) * diff(xran),
             ylim = yran + 0 * c(-1, 1) * diff(yran),
             xlab = "", ylab = "")
        
        # Triangular frame
        for (i in 1:3) {
            vec0 <- diag(rep(1, 3))[, i]
            vec1 <- diag(rep(1, 3))[, i %% 3 + 1]
            segments(x0 = xsgn * x(vec0), x1 = xsgn * x(vec1),
                     y0 = y(vec0), y1 = y(vec1),
                     lty = 1, lwd = 1,
                     col = rgb(0, 0, 0, sqrt(input$alpha)))
        }
        
        # Corner labels
        text(x = xsgn * apply(diag(rep(1, 3)), 2, x),
             y = apply(diag(rep(1, 3)), 2, y),
             labels = c("Disagree", "Agree", "Uncertain"),
             pos = 3 + xsgn * c(-1, -1, 1), cex = 1.2)
        
        if (FALSE) {
            # Uncertainty trajectory
            if (input$un.tr) {
                segments(x0 = xsgn * dat$x.u0, y0 = dat$y.u0,
                         x1 = xsgn * dat$x.u1, y1 = dat$y.u1,
                         lty = 3,
                         col = rgb(ifelse(dat$hl.topic, 1, 0), 0, 0,
                                   sqrt(input$alpha)))
            }
        }
        
        # Strength trajectory
        if (input$str.tr) {
            segments(x0 = xsgn * dat$x.s0, y0 = dat$y.s0,
                     x1 = xsgn * dat$x.s1, y1 = dat$y.s1,
                     lty = 5,
                     col = if (input$t_subset) {
                         rgb(0, 0, 0, input$alpha)
                     } else {
                         rgb(ifelse(dat$hl.topic, 1, 0), 0, 0,
                             sqrt(input$alpha))
                     })
        }
        
        # Confidence trajectory
        if (input$conf.tr) {
            segments(x0 = xsgn * dat$x.c0, y0 = dat$y.c0,
                     x1 = xsgn * dat$x.c1, y1 = dat$y.c1,
                     lty = 1,
                     col = if (input$t_subset) {
                         rgb(0, 0, 0, input$alpha)
                     } else {
                         rgb(ifelse(dat$hl.topic, 1, 0), 0, 0,
                             sqrt(input$alpha))
                     })
        }
        
        # Points!
        points(x = xsgn * dat$x, y = dat$y,
               pch = 19, cex = input$cex.base * sqrt(dat$count),
               col = if (input$t_subset) {
                   rgb(0, 0, 0, input$alpha)
               } else {
                   rgb(ifelse(dat$hl.topic, 1, 0), 0, 0, input$alpha)
               })
        
        # Circles
        if (!input$p_subset) {
            points(x = xsgn * dat$x, y = dat$y,
                   pch = ifelse(dat$hl.panelist, 1, NA),
                   cex = 1.5 * input$cex.base * sqrt(dat$count),
                   col = rgb(0, 0, 1, sqrt(input$alpha)))
        }
        
        # Legend
        n_questions <- length(unique(dat$question))
        legend("topright", legend = paste("Number of questions:", n_questions),
               pch = NA, box.lty = 0)
    })
    
    output$plot_topics <- renderDataTable({
        
        # Which points
        res <- if (input$tab == "vs") {
            if (is.null(input$plot_brush_vs)) {
                nearPoints(uniqDat(), input$plot_click_vs, "X", "Y")
            } else {
                brushedPoints(uniqDat(), input$plot_brush_vs, "X", "Y")
            }
        } else if (input$tab == "triangle") {
            if (is.null(input$plot_brush)) {
                nearPoints(uniqDat(), input$plot_click, "x", "y")
            } else {
                brushedPoints(uniqDat(), input$plot_brush, "x", "y")
            }
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
        
    }, escape = FALSE)
}

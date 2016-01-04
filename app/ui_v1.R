ui <- fluidPage(
    titlePanel("Situating consensus for the IGM surveys"),
    p("Discuss the", code("app"), "here."),
    fluidRow(
        column(
            3,
            wellPanel(
                h4("Positioning"),
                sliderInput("str.wt",
                            "How much should strong (dis)agreement matter?",
                            0, 1, 0),
                checkboxInput("str.tr", "Strength trajectory"),
                sliderInput("conf.wt",
                            "How much should confidence matter?",
                            0, 1, 0),
                checkboxInput("conf.tr", "Confidence trajectory",
                              value = TRUE)
            ),
            wellPanel(
                h4("Appearance"),
                sliderInput("cex.base", "Size", .1, .5, .3),
                sliderInput("alpha", "Transparency", 0, 1, .2)
            )
        ),
        column(
            9,
            wellPanel(
                fluidRow(
                    column(
                        4,
                        selectInput(inputId = "panelist",
                                    label = "Panelist",
                                    choices = c("-", unique(allDat$panelist)))
                    ),
                    column(
                        1,
                        checkboxInput("p_subset", "Only")
                    ),
                    column(
                        4,
                        textInput("topic", "Topic, e.g. 'gree(k|ce)':")
                    ),
                    column(
                        1,
                        checkboxInput("t_subset", "Only")
                    )
                )
            ),
            plotOutput("triangle", height = "750px",
                       click = "plot_click",
                       hover = hoverOpts(id = "plot_hover",
                                         delayType = "debounce"),
                       brush = brushOpts(id = "plot_brush",
                                         delayType = "debounce")),
            wellPanel(
                h4("Selection"),
                fluidRow(
                    column(2, selectInput(
                        "tool", label = "Tool",
                        choices = c("click", "hover", "brush")
                    )),
                    column(7, tableOutput("plot_topics"))
                )
            )
        )
    )
)

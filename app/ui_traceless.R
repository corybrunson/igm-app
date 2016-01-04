# User interface
ui <- fluidPage(
    theme = shinytheme("cosmo"),
    titlePanel("Situating consensus for the IGM surveys"),
    checkboxInput("show", "Description", value = TRUE),
    conditionalPanel(
        condition = "input.show == true",
        HTML(paste(readLines("text/description.txt"), collapse = ""))
    ),
    fluidRow(
        column(
            3,
            wellPanel(
                #h4("Subsetting"),
                selectInput(inputId = "panelist",
                            label = "Answered by this panelist:",
                            choices = c("-", sort(unique(allDat$panelist)))),
                checkboxInput("p_subset", "Hide others"),
                textInput("topic", "Keyword, e.g. 'gree(k|ce)':"),
                checkboxInput("t_subset", "Hide others")
            ),
            wellPanel(
                #h4("Positioning"),
                sliderInput("str.wt",
                            "Weight strong (dis)agreement",
                            0, 1, 0),
                sliderInput("conf.wt",
                            "Weight confidence",
                            0, 1, 0)
            ),
            wellPanel(
                #h4("Appearance"),
                sliderInput("cex.base", "Point size", .1, .5, .3),
                sliderInput("alpha", "Transparency", 0, 1, .2)
            )
        ),
        column(
            9,
            tabsetPanel(
                id = "tab",
                tabPanel(
                    "Consensus-Uncertainty", value = "vs",
                    checkboxInput("show_vs", "Explanation"),
                    conditionalPanel(
                        condition = "input.show_vs == true",
                        HTML(paste(readLines("text/description_vs.html"),
                                   collapse = ""))
                    ),
                    plotOutput("vs", height = "600px",
                               click = "plot_click_vs",
                               brush = brushOpts(id = "plot_brush_vs",
                                                 delayType = "debounce"))
                ),
                tabPanel(
                    "Agreement-Uncertainty", value = "triangle",
                    checkboxInput("show_triangle", "Explanation"),
                    conditionalPanel(
                        condition = "input.show_triangle == true",
                        HTML(paste(readLines("text/description_triangle.html"),
                                   collapse = ""))
                    ),
                    plotOutput("triangle", height = "750px",
                               click = "plot_click",
                               brush = brushOpts(id = "plot_brush",
                                                 delayType = "debounce"))
                )
            ),
            dataTableOutput("plot_topics")
        )
    )
)

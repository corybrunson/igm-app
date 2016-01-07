# User interface
ui <- fluidPage(
    theme = shinytheme("cosmo"),
    titlePanel("Situating consensus for the IGM surveys"),
    tabsetPanel(
        id = "tab",
        tabPanel(
            "Introduction", value = "intro",
            HTML(paste(readLines("text/description.txt"), collapse = ""))
        ),
        tabPanel(
            "Agreement, Disagreement, and Uncertainty", value = "tri",
            HTML(paste(c(readLines("text/tri_description.txt"),
                         readLines("text/plot_guide.txt")),
                       collapse = "")),
            fluidRow(
                column(3,
                       wellPanel(
                           #h4("Subsetting"),
                           selectInput(
                               inputId = "panelist",
                               label = "Answered by this panelist:",
                               choices = c("-", sort(unique(allDat$panelist)))
                           ),
                           checkboxInput("p_subset", "Hide others"),
                           textInput("topic",
                                     "Keyword in regex, e.g. 'gree(k|ce)':"),
                           checkboxInput("t_subset", "Hide others")
                       ),
                       wellPanel(
                           #h4("Positioning"),
                           sliderInput("str.wt",
                                       "Weight (dis)agreement strength",
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
                column(9,
                       plotOutput("tri", height = "667px",
                                  click = "plot_click_tri",
                                  brush = brushOpts(id = "plot_brush_tri",
                                                    delayType = "debounce",
                                                    resetOnNew = TRUE),
                                  dblclick = "plot_dblclick_tri"),
                       dataTableOutput("dt_tri")
                )
            )
        ),
        tabPanel(
            "Consensus vs Uncertainty", value = "vs",
            HTML(paste(c(readLines("text/vs_description.txt"),
                         readLines("text/plot_guide.txt")),
                       collapse = "")),
            fluidRow(
                column(3,
                       wellPanel(
                           #h4("Subsetting"),
                           selectInput(
                               inputId = "panelist",
                               label = "Answered by this panelist:",
                               choices = c("-", sort(unique(allDat$panelist)))
                           ),
                           checkboxInput("p_subset", "Hide others"),
                           textInput("topic",
                                     "Keyword in regex, e.g. 'gree(k|ce)':"),
                           checkboxInput("t_subset", "Hide others")
                       ),
                       wellPanel(
                           #h4("Positioning"),
                           sliderInput("str.wt",
                                       "Weight (dis)agreement strength",
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
                column(9,
                       plotOutput("vs", height = "400px",
                                  click = "plot_click_vs",
                                  brush = brushOpts(id = "plot_brush_vs",
                                                    delayType = "debounce",
                                                    resetOnNew = TRUE),
                                  dblclick = "plot_dblclick_vs"),
                       dataTableOutput("dt_vs")
                )
            )
        ),
        tabPanel(
            "Discussion", value = "discuss",
            h3("Plot Comparison"),
            HTML(paste(readLines("text/plot_comparison.txt"),
                       collapse = "")),
            h3("Technical Specs"),
            h4("Consensus vs Uncertainty"),
            HTML(paste(readLines("text/vs_description.html"),
                       collapse = "")),
            h4("Agreement, Disagreement, and Uncertainty"),
            HTML(paste(readLines("text/tri_description.html"),
                       collapse = ""))
        ),
        tabPanel(
            "The Questions", value = "dt",
            HTML(paste(readLines("text/dt_description.txt"),
                       collapse = "")),
            fluidRow(
                column(3,
                       wellPanel(
                           #h4("Subsetting"),
                           selectInput(
                               inputId = "panelist",
                               label = "Answered by this panelist:",
                               choices = c("-", sort(unique(allDat$panelist)))
                           ),
                           checkboxInput("p_subset", "Hide others"),
                           textInput("topic",
                                     "Keyword in regex, e.g. 'gree(k|ce)':"),
                           checkboxInput("t_subset", "Hide others")
                       )
                ),
                column(9,
                       dataTableOutput("dt_dt")
                )
            )
        )
    )
)

# User interface
ui <- fluidPage(
    theme = shinytheme("cosmo"),
    #titlePanel("Situating consensus for the IGM surveys"),
    titlePanel("the shape of economic opinion"),
    fluidRow(
        column(3,
               conditionalPanel(
                   condition = paste(paste0("input.tab == '",
                                            c("data", "vs", "tri"),
                                            "'"),
                                     collapse = " || "),
                   wellPanel(
                       #h4("Subsetting"),
                       HTML(paste(readLines("text/guide_filters.txt"),
                                  collapse = "")),
                       selectInput(
                           inputId = "panelist",
                           label = "Answered by:",
                           choices = c("-", sort(unique(allDat$panelist)))
                       ),
                       checkboxInput("p_subset", "Highlight only"),
                       textInput("topic", "Regex, e.g. 'Gree(k|ce)':"),
                       checkboxInput("t_subset", "Highlight only"),
                       HTML(paste(readLines("text/guide_subset.txt"),
                                  collapse = ""))
                   )
               ),
               conditionalPanel(
                   condition = "(input.tab == 'strconf')",
                   wellPanel(
                       radioButtons("conf.var",
                                    "Confidence measure",
                                    choices = c("Raw", "Logit"),
                                    inline = TRUE)
                   )
               ),
               conditionalPanel(
                   condition = "(input.tab == 'vs' || input.tab == 'tri')",
                   wellPanel(
                       #h4("Positioning"),
                       HTML(paste(readLines("text/guide_sliders.txt"),
                                  collapse = "")),
                       sliderInput("str.wt",
                                   "(Dis)agreement strength",
                                   0, 1, 0),
                       sliderInput("conf.wt",
                                   "Confidence",
                                   0, 1, 0)
                   ),
                   wellPanel(
                       #h4("Appearance"),
                       sliderInput("cex.base", "Point size", .1, .5, .3),
                       sliderInput("alpha", "Transparency", 0, 1, .2)
                   )
               )
        ),
        column(9,
               tabsetPanel(
                   id = "tab",
                   tabPanel(
                       "Introduction", value = "intro",
                       HTML(paste(readLines("text/description.txt"),
                                  collapse = ""))
                   ),
                   tabPanel(
                       "Data", value = "data",
                       HTML(paste(readLines("text/data_description.txt"),
                                  collapse = ""))
                   ),
                   tabPanel(
                       "Confidence Plot", value = "strconf",
                       HTML(paste(readLines("text/strconf_description.txt"),
                                  collapse = "")),
                       plotOutput("strconf", height = "500px", width = "700px")
                   ),
                   tabPanel(
                       "Agreement Plot", value = "tri",
                       HTML(paste(c(readLines("text/tri_description.txt"),
                                    readLines("text/plot_guide.txt")),
                                  collapse = "")),
                       plotOutput("tri", height = "750px",
                                  click = "plot_click_tri",
                                  brush = brushOpts(id = "plot_brush_tri",
                                                    delayType = "debounce",
                                                    resetOnNew = TRUE),
                                  dblclick = "plot_dblclick_tri")
                   ),
                   tabPanel(
                       "Consensus Plot", value = "vs",
                       HTML(paste(c(readLines("text/vs_description.txt"),
                                    readLines("text/plot_guide.txt")),
                                  collapse = "")),
                       plotOutput("vs", height = "500px", width = "700px",
                                  click = "plot_click_vs",
                                  brush = brushOpts(id = "plot_brush_vs",
                                                    delayType = "debounce",
                                                    resetOnNew = TRUE),
                                  dblclick = "plot_dblclick_vs")
                   ),
                   tabPanel(
                       "Discussion", value = "discuss",
                       h3("Plot Comparison"),
                       HTML(paste(readLines("text/plot_comparison.txt"),
                                  collapse = "")),
                       h3("Technical Specs"),
                       h4("Agreement, Disagreement, and Uncertainty"),
                       HTML(paste(readLines("text/tri_description.html"),
                                  collapse = "")),
                       h4("Consensus vs Uncertainty"),
                       HTML(paste(readLines("text/vs_description.html"),
                                  collapse = ""))
                   )
               ),
               hr(),
               dataTableOutput("plot_topics")
        )
    )
)

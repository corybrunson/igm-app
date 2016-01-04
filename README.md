Scatterplotting agreement and uncertainty among the IGM panel survey responses
=====

This R Shiny App produces two dynamic scatterplots of different measures of uncertainty and agreement in the responses collected from academic economists by the [Initiative on Global Markets](http://www.igmchicago.org/) at the University of Chicago Booth School of Business. It should be live [here](https://corybrunson.shinyapps.io/igm-app).

I've tweaked the scrape script several times as i've discovered new errors in the results. If you find any errors, please let me know—or feel free to correct them and send a pull request. I'll scrape the results and redeploy the app at least semesterly.

## Acknowledgments

I borrowed some insight from the Python code at [Chris Said's `economist_poll` repo] [2]. Also invaluable have been Hadley Wickam's [`rvest` package] [3] and the [SelectorGadget] [4] Chrome extension.

[2]: https://github.com/csaid/economist_poll
[3]: http://blog.rstudio.org/2014/11/24/rvest-easy-web-scraping-with-r/
[4]: http://selectorgadget.com/

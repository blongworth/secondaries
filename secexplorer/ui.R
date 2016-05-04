library(shiny)
library(ggvis)

# Secondary explorer shiny app.
# Loads an R data file of secondary standard data and plots

shinyUI(fluidPage(
  titlePanel("Secondary explorer"),
  fluidRow(
    column(3,
      wellPanel(
        h4("Filter"),
        radioButtons("system", "System",
                     c("USAMS" = "USAMS", "CFAMS" = "CFAMS", "Both" = "both"),
                     selected = "USAMS"),
        radioButtons("stdType", "Sample Type",
                     c("Primaries" = 1, "Secondaries" = 2, "Both" = 3),
                     selected = 3),
        radioButtons("lab", "Graphite Lab",
                     c("SPL" = 1, "Watson" = 2, "All" = 3),
                     selected = 1),
        dateRangeInput('date',
                       label = 'Date Range',
                       start = Sys.Date() - 90, 
                       end = Sys.Date(),
                       max = Sys.Date()),
        sliderInput("size", "Graphite Size (umol)",
                    1, 500, value = c(40,300)),
        sliderInput("fm", "Fm Range",
                    0, 2, value = c(.1,1.6), step = 0.05),
        radioButtons("splits", "Split targets",
                     c("Split" = 1, "Unsplit" = 2, "Both" = 3),
                     selected = 3),
        textInput("Name", "Sample name contains (e.g., C-2)")
      )
    ),
    column(9,
      ggvisOutput("plot1"),
      wellPanel(
        selectInput("xvar", "X-axis variable", axis_vars, selected = "rep_err"),
        selectInput("yvar", "Y-axis variable", axis_vars, selected = "normFm")
      ),
      wellPanel(
        htmlOutput("stdData")
      )
    )
  )
))

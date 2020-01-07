library(shiny)
library(ggvis)
library(plotly)

# Secondary explorer shiny app.
# Loads an R data file of secondary standard data and plots

shinyUI(fluidPage(
  titlePanel("Secondary explorer"),
  fluidRow(
    column(3,
      wellPanel(
        h3("Filters"),
        radioButtons("dataSource",
                    label = "Data Source", 
                    c("intcal", "standards"),
                    selected = "standards"),
        radioButtons("system", "System",
                     c("USAMS" = "USAMS", "CFAMS" = "CFAMS", "Both" = "both"),
                     selected = "both"),
        radioButtons("stdType", "Sample Type",
                     c("Primaries" = 1, "Secondaries" = 2, "Both" = 3),
                     selected = 2),
        radioButtons("lab", "Graphite Lab",
                     c("SPL" = 1, "Watson" = 2, "All" = 3),
                     selected = 1),
        checkboxGroupInput("process", "Process",
                     choices = list("Hydro" = "HY", "Gas" = "GS", 
                                    "Water" = "WS", "Organic" = "OC"),
                     selected = list("HY", "GS", "OC")),
        dateRangeInput('date',
                       label = 'Date Range',
                       start = Sys.Date() - 90, 
                       end = Sys.Date(),
                       max = Sys.Date()),
        sliderInput("size", "Graphite Size (umol)",
                    0, 350, value = c(40,300)),
        sliderInput("fm", "Fm Range",
                    0, 1.6, value = c(.1,1.6), step = 0.05),
        radioButtons("splits", "Split targets",
                     c("Split" = 1, "Unsplit" = 2, "Both" = 3),
                     selected = 3),
        h3("Outlier removal"),
        checkboxInput("filt", "Remove outliers", value = TRUE),
        sliderInput("sigsel", "Max sigma", 0, 20, value = 10),
        sliderInput("nfm", "Max norm Fm", 0, 0.1, value = 0.10),
        sliderInput("fme", "Max reported error", 0, 0.02, value = 0.01),
        textInput("Name", "Sample name contains (e.g., C-2)")
      )
    ),
    column(9,
           plotlyOutput("plot", height = "600px"),
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

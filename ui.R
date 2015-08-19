library(shiny)
library(ggvis)

# For dropdown menu
# actionLink <- function(inputId, ...) {
#   tags$a(href='javascript:void',
#          id=inputId,
#          class='action-button',
#          ...)
# }

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
        #checkboxInput("splits", label = "Include split samples?", value = TRUE),
        radioButtons("splits", "Split targets",
                     c("Split" = 1, "Unsplit" = 2, "Both" = 3),
                     selected = 3),
#         checkboxGroupInput("splits", label = h3("Number of Targets (Splits)"), 
#                            choices = list("1" = 1, "2" = 2, "3" = 3, "4" = 4),
#                            selected = c(1,2,3,4),
#                            inline = TRUE),
#         selectInput("rec", "Receipt Number",
#           c("All", "Action", "Adventure", "Animation", "Biography", "Comedy",
#             "Crime", "Documentary", "Drama", "Family", "Fantasy", "History",
#             "Horror", "Music", "Musical", "Mystery", "Romance", "Sci-Fi",
#             "Short", "Sport", "Thriller", "War", "Western")
#         ),
        textInput("Name", "Sample name contains (e.g., C-2)")
      ),
      wellPanel(
        selectInput("xvar", "X-axis variable", axis_vars, selected = "merr"),
        selectInput("yvar", "Y-axis variable", axis_vars, selected = "normFm")
      )
    ),
    column(9,
      ggvisOutput("plot1"),
      wellPanel(
        htmlOutput("stdData")
      )
    )
  )
))

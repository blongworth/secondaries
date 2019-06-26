#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Load Libraries
library(tidyverse)
library(amstools)
library(shiny)

#Load QC data from file
load("../qcData.rda")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Primary Standard Performance Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
		dateRangeInput('date',
			       label = 'Date Range',
			       start = Sys.Date() - 360, 
			       end = Sys.Date(),
			       max = Sys.Date())
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
#Filter data
out.s <- std %>% 
  filter(primary == TRUE,
         rec_num %in% c(113385, 113386, 133143, 133144, 149822), #look at normalizing ox-I
         gf_co2_qty > 40, gf_co2_qty < 200, #Select size range
         tp_date_pressed >= input$date[1],
         tp_date_pressed <= input$date[2],
         lab == "OSG", #Use only samples from the SPL
         is.na(q_flag), #Check for q_flag
         sigma < 10, sigma > -10, #Select reasonable sigmas
         normFm < 0.02, normFm > -0.02, #Select reasonable Fm
         frep_err < 0.10
         ) 

#Make summary table
wheelsum <- out.s %>% 
  group_by(system, wheel) %>%
  summarize(
    date = mean(tp_date_pressed),
    fm.s = sd(f_modern),
    sig = mean(sigma),
    sig.s = sd(sigma),
    rep_err.m = mean(rep_err),
    intrerr = intrErr(sd(f_modern), mean(rep_err)),
    N = length(f_modern)) %>%
    arrange(system, as.Date(date))

ggplot(data=wheelsum, aes(x=date, y=fm.s, color=system)) + 
  geom_point() + geom_smooth() +
  ggtitle("SD of OX-I by wheel")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


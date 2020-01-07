library(ggvis)
library(dplyr)
library(plotly)
library(RColorBrewer)

#Load Secondaries data frame
load("../qcData.rda")

if (names(dev.cur()) != "null device") dev.off()
pdf(NULL)

# color map for plots
colors <- brewer.pal(3, "Set2")
color_map <- c(CFAMS=colors[1], USAMS=colors[2])

shinyServer(function(input, output, clientData, session) {

  secData <- reactive({
    switch(input$dataSource,
           "qc" = qc,
           "intcal" = out,
           "standards" = std)
  })
  
  # Filter the secondaries
  secondaries <- reactive({

    # Apply filters
    m <- secData() %>%
      filter(
        tp_date_pressed >= input$date[1],
        tp_date_pressed <= input$date[2],
        gf_co2_qty >= input$size[1],
        gf_co2_qty <= input$size[2],
        fm_consensus >= input$fm[1],
        fm_consensus <= input$fm[2]
      )

    #Filter primaries secondaries
    if (input$stdType == 1) { 
      m <- m %>% filter(primary == TRUE)
    }
    if (input$stdType == 2) { 
      m <- m %>% filter(primary == FALSE)
    }
    #Filter by graphite lab
    if (input$lab == 1) { 
      m <- m %>% filter(lab == "OSG")
    }
    if (input$lab == 2) { 
      m <- m %>% filter(lab == "WAT")
    }
    # Filter by process type
    #if (input$process != "all") {
      m <- m %>% filter(process %in% input$process)
    #}
    # filter by system
    if (input$system != "both") {
      sys <- paste0(input$system)
      m <- m %>% filter(system == sys)
    }
    # filter by secondary
    if (!is.null(input$Name) && input$Name != "") {
      m <- m %>% filter(grepl(input$Name, name))
    }
    # Splits
    if (input$splits == 1) { 
      m <- m %>% filter(splits > 1)
    }
    if (input$splits == 2) { 
      m <- m %>% filter(splits == 1)
    }
    if (input$filt) {
      m <- m %>% filter(abs(sigma) < input$sigsel,
                        abs(normFm) < input$nfm, 
                        rep_err < input$fme)
    }

    as.data.frame(m)

  })

  output$plot <- renderPlotly({
    ## build graph with ggplot syntax
    p <- ggplot(secondaries(),
                aes(x = get(input$xvar),
                    y = get(input$yvar),
                    color = system,
                    text = paste("Type:", name, "<br>", 
                                 "OSG:", osg_num, "<br>",
                                 "Wheel:", wheel))) +
      theme_bw() +
      geom_point() +
      labs(x = input$xvar, y = input$yvar) +
      scale_color_manual(values = color_map)
      
    ggplotly(p) 

  })

  output$stdData <- renderUI({ 
    
    z <- secondaries()
    count <- nrow(z)
    n <- paste0("Number of samples selected ", count)
    
    s <- sprintf("Mean sigma of selected is %.4f SD %.4f", 
                 mean(z$sigma), 
                 sd(z$sigma))
    
    f <- sprintf("Mean normalized Fm of selected is %.4f SD %.4f", 
                 mean(z$normFm), 
                 sd(z$normFm))
    
    e <- sprintf("Mean relative reported error of selected is %.4f SD %.4f", 
                 mean(z$frep_err), 
                 sd(z$frep_err))
    
    fly <- sprintf("Fraction of flyers (greater than 3 sigma): %.3f",
                    sum(abs(z$sigma) > 3) / count)
    
    HTML(paste(n, s, f, e, fly, sep = '<br/>'))
    
  })
})


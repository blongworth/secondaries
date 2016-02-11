library(ggvis)
library(dplyr)

#Load Secondaries data frame
load("./qcData.rda")

out <- filter(out, 
                is.na(q_flag), #Check for q_flag
                sigma < 10, sigma > -10, #Select reasonable sigmas
                normFm < 0.02, normFm > -0.02, #Select reasonable Fm
                frep_err < 0.10
)

shinyServer(function(input, output, session) {

  # Filter the secondaries
  secondaries <- reactive({
    # Due to dplyr issue #318, we need temp variables for input values
    system <- input$system
    mindate <- input$date[1]
    maxdate <- input$date[2]
    minsize <- input$size[1]
    maxsize <- input$size[2]
    minfm <- input$fm[1]
    maxfm <- input$fm[2]
    
    # Apply filters
    m <- out %>%
      filter(
        as.Date(tp_date_pressed) >= as.Date(mindate),
        as.Date(tp_date_pressed) <= as.Date(maxdate),
        gf_co2_qty >= minsize,
        gf_co2_qty <= maxsize,
        fm_exp >= minfm,
        fm_exp <= maxfm
      ) 
    #Filter primaries secondaries
    if (input$stdType == 1) { 
      m <- m %>% filter(primary == TRUE) #rec_num == 113385
    }
    if (input$stdType == 2) { 
      m <- m %>% filter(primary == FALSE)
    }
    #Filter by graphite lab
    if (input$lab == 1) { 
      m <- m %>% filter(lab_name == "OSG")
    }
    if (input$lab == 2) { 
      m <- m %>% filter(lab_name == "WAT")
    }
    
    # filter by system
    if (input$system != "both") {
      sys <- paste0(input$system)
      m <- m %>% filter(system == sys)
    }
    # filter by secondary
    if (!is.null(input$Name) && input$Name != "") {
      #Name <- paste0("%", input$Name, "%")
      m <- m %>% filter(grepl(input$Name, name))
    }
    # Splits
    if (input$splits == 1) { 
      m <- m %>% filter(splits > 1)
    }
    if (input$splits == 2) { 
      m <- m %>% filter(splits == 1)
    }
    
    m <- as.data.frame(m)

#     # Add column which says whether the movie won any Oscars
#     # Be a little careful in case we have a zero-row data frame
#     m$has_oscar <- character(nrow(m))
#     m$has_oscar[m$Oscars == 0] <- "No"
#     m$has_oscar[m$Oscars >= 1] <- "Yes"
    m
  })

#   # Function for generating tooltip text
  secondary_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$tp_num)) return(NULL)

    # Pick out the secondary with this ID
    stds <- isolate(secondaries())
    std <- stds[stds$tp_num == x$tp_num, ]

    paste0("<b>", std$name, "</b><br>",
           std$tp_num, "<br>",
           std$tp_date_pressed, "<br>",
           std$wheel_id, "<br>",
           sprintf("%.4f", std$f_modern)
    )
  }

  # A reactive expression with the ggvis plot
  vis <- reactive({
    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]

    # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
    # but since the inputs are strings, we need to do a little more work.
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))

    secondaries %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.2, fillOpacity.hover := 0.5,
        stroke = ~system, key := ~tp_num) %>%
      add_tooltip(secondary_tooltip, "hover") %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name) %>%
      #add_legend("stroke", title = "Won Oscar", values = c("Yes", "No")) %>%
      #scale_nominal("stroke", domain = c("Yes", "No"),
      #  range = c("orange", "#aaa")) %>%
      set_options(width = 500, height = 500)
  })

  vis %>% bind_shiny("plot1")

  #output$n_secondaries <- renderText({ nrow(secondaries()) })

  output$stdData <- renderUI({ 
    
    z <- secondaries()
    
    n <- paste0("Number of samples selected ", nrow(z))
    
    s <- sprintf("Mean sigma of selected is %.4f SD %.4f", 
                 mean(z$sigma), 
                 sd(z$sigma))
    
    f <- sprintf("Mean normalized Fm of selected is %.4f SD %.4f", 
                 mean(z$normFm), 
                 sd(z$normFm))
    
    e <- sprintf("Mean relative reported error of selected is %.4f SD %.4f", 
                 mean(z$frep_err), 
                 sd(z$frep_err))
    
    HTML(paste(n, s, f, e, sep = '<br/>'))
    
  })
})

#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  dataset <- reactive({
    fname <- "short_summary_20190826.csv"
    df <- readr::read_csv(fname)
    df$price_m2 <- df$price / df$area
    return(df)
  })
  
  filtered_dataset <- reactive({
    df <- dataset()
    
    if(input$district != "ALL")
    {
      df <- df %>% dplyr::filter(district == input$district)
    }
    
    df <- df %>%
      dplyr::filter(PropType == input$prop_type) %>%
      dplyr::filter(Sale == input$is_sale)
    
    # if less then 10 datapoints, do not display data
    return(df)
  })
  
  #city_menu <- list of cities as function of the district
  #parish_menu <- list of parishes as function of the city
  
  output$HistogramPrice_m2 <- renderPlot({
    title_str <- "Price/m2 distribution"
    target_col <- "price_m2"
    xaxis_name <- "Price/Area (€/m2)"
    df <- filtered_dataset()
    q <- as.numeric(input$truncation) / 100.0
    quantiles <- quantile(df$price_m2, probs = c(q, 1 - q))
    ggplot(df, aes_string(x=target_col)) +
      geom_histogram(bins=input$granularity) +
      scale_x_continuous(trans='log10', limits=quantiles) +
      ggtitle(title_str) +
      xlab(xaxis_name)
  })
  
  output$HistogramPrice <- renderPlot({
    title_str <- "Price distribution"
    target_col <- "price"
    xaxis_name <- "Price (€)"
    df <- filtered_dataset()
    q <- as.numeric(input$truncation) / 100.0
    quantiles <- quantile(df$price, probs = c(q, 1 - q))
    ggplot(df, aes_string(x=target_col)) +
      geom_histogram(bins=input$granularity) +
      scale_x_continuous(trans='log10', limits=quantiles) +
      ggtitle(title_str) +
      xlab(xaxis_name)
  })
  
  output$HistogramArea <- renderPlot({
    title_str <- "Area distribution"
    target_col <- "area"
    xaxis_name <- "Area (m2)"
    df <- filtered_dataset()
    q <- as.numeric(input$truncation) / 100.0
    quantiles <- quantile(df$area, probs = c(q, 1 - q))
    ggplot(df, aes_string(x=target_col)) +
      geom_histogram(bins=input$granularity) +
      scale_x_continuous(trans='log10', limits=quantiles) +
      ggtitle(title_str) +
      xlab(xaxis_name)
  })
  
  output$ParetoPrice <- renderPlot({
  })
  
  output$ParetoArea <- renderPlot({
  })
  
})

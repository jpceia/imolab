library(ggthemes)
library(Cairo)
options(shiny.usecairo=TRUE)



# ----------------------------------------------------------------------------------------
#                                         SERVER
# ----------------------------------------------------------------------------------------


shinyServer(function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  observe({
    city_list <- c("")
    df <- city_meta() %>% filter(code1 == input$district)
    
    if(nrow(df) > 0) {
      city_list <- df$Dicofre
      names(city_list) <- df$Designacao
    }

    updateSelectInput(session, "city", choices=city_list)
    updateSelectInput(session, "parish", choices=c(""))
  })
  
  
  observe({
    parish_list <- c("")
    df <- parish_meta %>% filter(code1 == input$city)
    
    if(nrow(df) > 0) {
      parish_list <- df$Dicofre
      names(parish_list) <- df$Designacao
    }
    
    updateSelectInput(session, "parish", choices=parish_list)
  })
  
  filtered_dataset_cat <- reactive({

    df <- dataset %>%
      filter(PropType %in% input$prop_type) %>%
      filter(Sale == input$is_sale) %>%
      filter(district %in% district_meta$Dicofre)
    
    validate(
      need(nrow(df) > MIN_DATAPOINTS, "Not enough datapoints")
    )
    
    return(df)
  })
  
  filtered_dataset <- reactive({
    
    df <- filtered_dataset_cat()
    
    if(input$district != "")
    {
      if(input$city != "")
      {
        if(input$parish != "")
        {
          df <- df %>% dplyr::filter(freg == input$parish)
        }
        else
        {
          df <- df %>% dplyr::filter(city == input$city)
        }
      }
      else
      {
        df <- df %>% dplyr::filter(district == input$district)
      }
    }
    
    validate(
      need(nrow(df) > MIN_DATAPOINTS, "Not enough datapoints")
    )
    
    return(df)
  })
  
  # ----------------------------------------------------------------------------------------
  #                                     NUMERICAL SECTION
  # ----------------------------------------------------------------------------------------
  
  output$HistogramPrice_m2 <- renderHighchart({
    df <- filtered_dataset()
    hc_hist(df, "price_m2", "EUR/m2", "Price/m2 distribution", input$truncation)
  })
  
  output$tablePrice_m2 <- renderTable({
    df <- filtered_dataset()
    quantiles <- c(.01, .05, .1, .25, .4, .5, .75, .9, .95, .99)
    values <- quantile(pull(df, "price_m2"), probs=quantiles, na.rm=TRUE)
    table <- data.frame("price/m2"=values)
    row.names(table) <- paste(quantiles * 100, "%", sep="")
    return (t(table))
  }, align="c", digits=0)
  
  output$HistogramPrice <- renderHighchart({
    df <- filtered_dataset()
    hc_hist(df, "price", "EUR", "Price distribution", input$truncation)
  })
  
  output$tablePrice <- renderTable({
    df <- filtered_dataset()
    quantiles <- c(.01, .05, .1, .25, .4, .5, .75, .9, .95, .99)
    values <- quantile(pull(df, "price"), probs=quantiles, na.rm=TRUE)
    data.frame(quantile=paste(quantiles * 100, "%", sep=""), "price"=values)
  }, align="c", digits=0)
  
  
  output$HistogramArea <- renderHighchart({
    df <- filtered_dataset()
    hc_hist(df, "area", "m2", "Area distribution", input$truncation)
  })
  
  output$tableArea <- renderTable({
    df <- filtered_dataset()
    quantiles <- c(.01, .05, .1, .25, .4, .5, .75, .9, .95, .99)
    values <- quantile(pull(df, "area"), probs=quantiles, na.rm=TRUE)
    data.frame(quantile=paste(quantiles * 100, "%", sep=""), area=values)
  }, align="c", digits=0)
  
  
  # -------------------------------------- SCATTERPLOT -------------------------------------
  
  output$ScatterPriceArea <- renderPlot({
    df <- filtered_dataset()
    max_row <- 5000
    if(nrow(df) > max_row)
    {
      set.seed(0)
      df <- df[sample(nrow(df), max_row), ]
    }
    
    q <- as.numeric(input$truncation) / 100.0
    quantiles_area <- quantile(df$area, probs = c(q, 1 - q))
    quantiles_price <- quantile(df$price, probs = c(q, 1 - q))
    
    ggplot(df, aes(x=area, y=price)) +
      geom_jitter(shape=21) + 
      geom_smooth(method=lm, se=TRUE, fullrange=TRUE) +
      scale_x_continuous(limits=quantiles_area) + # trans='log10', 
      scale_y_continuous(limits=quantiles_price) +
      ggtitle("Area distribution") +
      xlab("Area (m2)") +
      ylab("Price (Eur)")
  })


  # ----------------------------------------------------------------------------------------
  #                                   CATEGORIES SECTION
  # ----------------------------------------------------------------------------------------
  
  output$categoryText <- renderText(input$category)
  
  output$CategoriesBoxPlot <- renderPlot({
    
    cat_col <- input$category
    
    df <- filtered_dataset()
    df <- df[!is.na(df[[cat_col]]), ]
    
    validate(
      need(nrow(df) > MIN_DATAPOINTS, "Filtering too narrow: not enough datapoints")
    )
    
    q <- as.numeric(input$truncation) / 100.0
    quantiles <- quantile(df$price_m2, probs = c(q, 1 - q))
    
    ggplot(df) +
      geom_boxplot(aes_string(x=cat_col, y="price_m2", fill=cat_col), size=.5) +
      scale_x_discrete(drop=FALSE) +
      scale_y_continuous(trans='log10', limits=quantiles) +
      theme(legend.position="none") +
      coord_flip()
  })
  
  output$CategoriesCount <- renderPlot({

    cat_col <- input$category
    
    df <- filtered_dataset()
    df <- df[!is.na(df[[cat_col]]), ]
    
    validate(
      need(nrow(df) > MIN_DATAPOINTS, "Filtering too narrow: not enough datapoints")
    )
    
    ggplot(df) +
      geom_bar(aes_string(x=cat_col, fill=cat_col), color="black") +
      scale_x_discrete(drop=FALSE) +
      theme(legend.position="none") +
      coord_flip()
  })
  
  
  output$tableCategories <- renderTable({
    
  })
  
  # ----------------------------------------------------------------------------------------
  #                                    DATA SOURCES SECTION
  # ----------------------------------------------------------------------------------------
  
  output$rawDataTable <- DT::renderDataTable(
    dataset %>%
      select(-district, -city, -freg) %>%
      rename(district=district_name, city=city_name, parish=parish_name),
    filter = 'top', options = list(scrollX = TRUE))
  
  output$pivotTable <- renderRpivotTable({
     rpivotTable(
       dataset,
       rows = "district_name",
       cols = c("Sale", "PropType"),
       aggregatorName = "Median",
       vals = "price_m2",
       rendererName = "Col Heatmap") 
  })
  
  
  # ----------------------------------------------------------------------------------------
  #                                     VALUATION SECTION
  # ----------------------------------------------------------------------------------------
  
  output$valuationOutput <- renderHighchart({
    
    highchart() %>%
         hc_chart(type = "waterfall") %>% 
         hc_xAxis(categories = c("area", "location", "condition", "rooms", "energy_certificate")) %>% 
         hc_add_series(c(10,19.4,21.1, 14.4, 6.5), showInLegend = FALSE)
  })
})

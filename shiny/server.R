library(ggthemes)
library(Cairo)
options(shiny.usecairo=TRUE)



# ----------------------------------------------------------------------------------------
#                                         SERVER
# ----------------------------------------------------------------------------------------


shinyServer(function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  observe({
    city_list <- c(NULL)
    df <- city_meta %>% filter(code1 == input$district)
    
    if (nrow(df) > 0) {
      city_list <- df$Dicofre
      names(city_list) <- df$Designacao
    }
    
    updateSelectInput(session, "city", choices = city_list)
    updateSelectInput(session, "parish", choices = c(NULL))
  })
  
  
  observe({
    parish_list <- c(NULL)
    df <- parish_meta %>% filter(code1 == input$city)
    
    if (nrow(df) > 0) {
      parish_list <- df$Dicofre
      names(parish_list) <- df$Designacao
    }
    
    updateSelectInput(session, "parish", choices = parish_list)
  })
  
  filtered_dataset_cat <- reactive({

    df <- dataset %>%
      filter(PropType %in% input$prop_type) %>%
      filter(Sale == input$is_sale) %>%
      filter(district %in% district_meta$Dicofre)
    
    validate(need(nrow(df) > MIN_DATAPOINTS, "Not enough datapoints"))
    
    return(df)
  })
  
  filtered_dataset <- reactive({
    
    df <- filtered_dataset_cat()
    
    if (!is.empty(input$district)) {
      if (!is.empty(input$city)) {
        if (!is.empty(input$parish))
          df <- df %>% filter(freg == input$parish)
        else
          df <- df %>% filter(city == input$city)
      }
      else
        df <- df %>% filter(district == input$district)
    }

    
    validate(need(nrow(df) > MIN_DATAPOINTS, "Not enough datapoints"))
    
    return(df)
  })
  
  # ----------------------------------------------------------------------------------------
  #                                     NUMERICAL SECTION
  # ----------------------------------------------------------------------------------------
  
  # --------------------------------------- HIGHCHARTS -------------------------------------

  output$HistogramPrice_m2 <- renderHighchart(
    filtered_dataset() %>% hc_hist("price_m2", "EUR/m2", "", input$truncation)
  )
  
  output$HistogramPrice <- renderHighchart(
    filtered_dataset() %>% hc_hist("price", "EUR", "", input$truncation)
  )
  
  output$HistogramArea <- renderHighchart(
    filtered_dataset() %>% hc_hist("area", "m2", "", input$truncation)
  )
  
  
  # -------------------------------------- SCATTERPLOT -------------------------------------
  
  output$ScatterPriceArea <- renderPlot({
    df <- filtered_dataset()
    max_row <- 5000
    if (nrow(df) > max_row)
    {
      set.seed(0)
      df <- df[sample(nrow(df), max_row),]
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
  
  # -------------------------------------- FORMATTABLE -------------------------------------
  
  output$tableQuantiles <- renderFormattable({
    df <-filtered_dataset() 
    probs <- c(0.95, 0.90, 0.75, 0.50, 0.25, 0.10, 0.05)
    table <- data.frame(
      quantile = percent(probs, 0),
      price = currency(quantile(df$price, probs = probs), "", 0),
      area = currency(quantile(df$area, probs = probs), "", 0),
      price_m2 = currency(quantile(df$price_m2, probs = probs), "", 0)
    )
    row.names(table) <- NULL
    
    formattable(
      table,
      align = c("c", "r", "r", "r"),
      list(
        quantile =  formatter("span", style = ~style(color = "grey", font.weight = "bold")),
        price = normalize_bar("lightpink", 0.2),
        area = normalize_bar("lightpink", 0.2),
        price_m2 = normalize_bar("lightblue", 0.2)
      )
    )
  })


  # ----------------------------------------------------------------------------------------
  #                                   CATEGORIES SECTION
  # ----------------------------------------------------------------------------------------
  
  output$categoryText <- renderText(input$category)
  
  output$CategoriesBoxPlot <- renderPlot({
    
    cat_col <- input$category
    
    df <- filtered_dataset()
    df <- df[!is.na(df[[cat_col]]), ]
    
    validate(need(
      nrow(df) > MIN_DATAPOINTS,
      "Filtering too narrow: not enough datapoints"
    ))
    
    q <- as.numeric(input$truncation) / 100.0
    quantiles <- quantile(df$price_m2, probs = c(q, 1 - q))
    
    ggplot(df) +
      geom_boxplot(aes_string(x = cat_col, y = "price_m2", fill = cat_col), size = 0.5) +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(trans = 'log10', limits = quantiles) +
      theme(legend.position = "none") +
      coord_flip()
  })
  
  output$CategoriesCount <- renderPlot({

    cat_col <- input$category
    
    df <- filtered_dataset()
    df <- df[!is.na(df[[cat_col]]), ]
    
    validate(need(
      nrow(df) > MIN_DATAPOINTS,
      "Filtering too narrow: not enough datapoints"
    ))
    
    ggplot(df) +
      geom_bar(aes_string(x = cat_col, fill = cat_col),
               color = "black",
               size = 0.5) +
      scale_x_discrete(drop = FALSE) +
      theme(legend.position = "none") +
      coord_flip()
  })
  
  output$tableCategories <- renderFormattable({
    filtered_dataset() %>%
      group_by_at(vars(one_of(input$category))) %>%
      summarize(
        count=n(price_m2),
        "25%"=currency(quantile(price_m2, probs=0.25), "", 2),
        median=currency(quantile(price_m2, probs=0.50), "", 2),
        "75%"=currency(quantile(price_m2, probs=0.75), "", 2)) %>%
      drop_na() %>%
      formattable(
        align = c("l", "r", "r", "r", "r"),
        list(
          area(col = input$category) ~ formatter(
            "span", style = ~style(color = "grey", font.weight = "bold")),
          count = normalize_bar("pink", 0.2),
          median = normalize_bar("lightblue", 0.2)
      ))
  })
  
  # ----------------------------------------------------------------------------------------
  #                                    DATA SOURCES SECTION
  # ----------------------------------------------------------------------------------------
  
  output$rawDataTable <- DT::renderDataTable(
    dataset %>%
      select(-district, -city, -freg),# %>%
      #rename(district=district_name, city=city_name, parish=parish_name),
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
  
  observe({
    city_list <- c(NULL)
    df <- city_meta %>% filter(code1 == input$district_val)
    
    if (nrow(df) > 0) {
      city_list <- df$Dicofre
      names(city_list) <- df$Designacao
    }
    
    updateSelectInput(session, "city_val", choices = city_list)
    updateSelectInput(session, "parish_val", choices = c(" "))
  })
  
  
  observe({
    parish_list <- c(NULL)
    df <- parish_meta %>% filter(code1 == input$city_val)
    
    if (nrow(df) > 0) {
      parish_list <- df$Dicofre
      names(parish_list) <- df$Designacao
    }
    
    updateSelectInput(session, "parish_val", choices = parish_list)
  })
  

  observeEvent(input$calculate_val, {
    updateNavbarPage(session = session, inputId = "valuation_tabs", selected = "Simulation Results")
  })
  
  
  output$valuationResult <- renderText({
    
    df <- list(
      Sale = input$is_sale_val,
      PropType = input$prop_type_val,
      
      district = input$district_val,
      city = input$city_val,
      freg = input$parish_val,
      
      energy_certificate = input$energy_certificate_val,
      condition = input$condition_val,
      rooms = input$rooms_val,
      bathrooms = input$bathrooms_val,
      
      area = input$net_area_val,
      gross_area = input$gross_area_val,
      terrain_area = input$terrain_area_val
    )
    
    df <- data.frame(lapply(df, function(x) ifelse(is.null(x), NA, x)))
    df <- df %>% tbl_df() %>%
      mutate(
        area = as.double(area),
        gross_area = as.double(gross_area),
        terrain_area = as.double(terrain_area)
      )

    X <- get_features(df, match_tables)

    pred_price_m2 <- 10 ^ (predict(xgb$price_m2, xgb.DMatrix(data = as.matrix(X))))[1]
    pred_price <- 10 ^ (predict(xgb$price, xgb.DMatrix(data = as.matrix(X))))[1]
    
    paste(
      c("Price_m2:", as.character(pred_price_m2), "\n",
        "Price:", as.character(pred_price_m2 * input$net_area_val), "\n",
        "Price:", as.character(pred_price)
      )
    )
  })
  
  output$valuationOutput <- renderHighchart({
    
    highchart() %>%
      hc_chart(type = "waterfall") %>%
      hc_xAxis(categories = c("area", "location", "condition", "rooms", "energy_certificate")) %>%
      hc_add_series(c(10, 19.4, 21.1, 14.4, 6.5), showInLegend = FALSE)
  })
})

library(ggthemes)
library(Cairo)
options(shiny.usecairo=TRUE)



# ----------------------------------------------------------------------------------------
#                                         SERVER
# ----------------------------------------------------------------------------------------


shinyServer(function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  rv <- reactiveValues(
    location_code = NULL,
    location_type = "Country")
  
  observeEvent(input$district, {
    
    if(is.empty(input$district))
    {
      rv$location_code <- NULL
      rv$location_type <- "Country"
      
      city_list <- c(NULL)
    }
    else
    {
      rv$location_code <- input$district
      rv$location_type <- "District"
      
      df <- city_meta %>% filter(code1 == input$district)
      city_list <- df$Dicofre
      names(city_list) <- df$Designacao
    }
    
    updateSelectInput(session, "city", choices = city_list)
    updateSelectInput(session, "parish", choices = c(NULL))
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$city, {

    if(is.empty(input$city))
    {
      rv$location_code <- input$district
      rv$location_type <- "District"
      
      parish_list <- c(NULL)
    }
    else
    {
      rv$location_code <- input$city
      rv$location_type <- "City"
      
      df <- parish_meta %>% filter(code1 == input$city)
      parish_list <- df$Dicofre
      names(parish_list) <- df$Designacao
    }

    updateSelectInput(session, "parish", choices = parish_list)
  }, ignoreInit = TRUE)
  
  observeEvent(input$parish, {
    
    if(is.empty(input$parish))
    {
      rv$location_code <- input$city
      rv$location_type <- "City"
    }
    else
    {
      rv$location_code <- input$parish
      rv$location_type <- "Parish"
    }
  }, ignoreInit = TRUE)
  
  

  
  filtered_dataset <- reactive({
    
    df <- dataset %>%
      filter(PropType %in% input$prop_type) %>%
      filter(Sale == input$is_sale)
    
    df <- switch(
      rv$location_type,
      Country = df %>% filter(district %in% district_meta$Dicofre),
      District = df %>% filter(district == rv$location_code),
      City = df %>% filter(city == rv$location_code),
      Parish = df %>% filter(freg == rv$location_code),
      stop("Invalid data")
    )

    validate(need(nrow(df) > MIN_DATAPOINTS, "Not enough datapoints"))
    
    return(df)
  })
  
  
  # ----------------------------------------------------------------------------------------
  #                                     NUMERICAL SECTION
  # ----------------------------------------------------------------------------------------
  
  # -------------------------------------- HIGHCHARTS --------------------------------------

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
  
  F_catBoxPlot <- function(cat_col, target_col = "price_m2") {
    
    df <- filtered_dataset()
    df <- df[!is.na(df[[cat_col]]), ]
    
    validate(need(
      nrow(df) > MIN_DATAPOINTS,
      "Filtering too narrow: not enough datapoints"
    ))
    
    q <- as.numeric(input$truncation) / 100.0
    quantiles <- quantile(df[[target_col]], probs = c(q, 1 - q))
    
    ggplot(df) +
      geom_boxplot(aes_string(x = cat_col, y = target_col, fill = cat_col), size = 0.5) +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(trans = 'log10', limits = quantiles) +
      theme(legend.position = "none") +
      coord_flip()
  }
  
  F_catCount <- function(cat_col) {
    
    df <- filtered_dataset()
    df <- df[!is.na(df[[cat_col]]), ]
    
    validate(need(
      nrow(df) > MIN_DATAPOINTS,
      "Filtering too narrow: not enough datapoints"
    ))
    
    ggplot(df, aes_string(x = cat_col, fill = cat_col)) +
      geom_bar(color = "black", size = 0.5) +
      geom_text(stat='count', aes(label=..count..),
                label.size = 2, hjust=-0.3, check_overlap = TRUE) +
      scale_x_discrete(drop = FALSE) +
      theme(legend.position = "none") +
      coord_flip()
  }
  
  F_catTable <- function(cat_col) {
    filtered_dataset() %>%
      group_by_at(vars(one_of(cat_col))) %>%
      summarize(
        count=n(price_m2),
        "25%"=currency(quantile(price_m2, probs=0.25), "", 2),
        median=currency(quantile(price_m2, probs=0.50), "", 2),
        "75%"=currency(quantile(price_m2, probs=0.75), "", 2)) %>%
      drop_na() %>% map_df(rev) %>%
      formattable(
        align = c("l", "r", "r", "r", "r"),
        list(
          area(col = cat_col) ~ formatter(
            "span", style = ~style(color = "grey", font.weight = "bold")),
          count = normalize_bar("pink", 0.2),
          median = normalize_bar("lightblue", 0.2)
        ))
  }
  
  
  output$categoryText <- renderText(input$category)
  
  output$EnergyCertificateBoxPlot <- renderPlot(F_catBoxPlot("energy_certificate", "price_m2"))
  output$EnergyCertificateCount <- renderPlot(F_catCount("energy_certificate"))
  output$EnergyCertificateTable <- renderFormattable(F_catTable("energy_certificate"))
  
  output$ConditionBoxPlot <- renderPlot(F_catBoxPlot("condition", "price_m2"))
  output$ConditionCount <- renderPlot(F_catCount("condition"))
  output$ConditionTable <- renderFormattable(F_catTable("condition"))
  
  output$RoomsBoxPlot <- renderPlot(F_catBoxPlot("rooms", "price_m2"))
  output$RoomsCount <- renderPlot(F_catCount("rooms"))
  output$RoomsTable <- renderFormattable(F_catTable("rooms"))
  
  output$BathroomsBoxPlot <- renderPlot(F_catBoxPlot("bathrooms", "price_m2"))
  output$BathroomsCount <- renderPlot(F_catCount("bathrooms"))
  output$BathroomsTable <- renderFormattable(F_catTable("bathrooms"))
  
  
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
  

  
  
  output$valuationResult <- renderFormattable({
    
    row <- list(
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
    
    row <- data.frame(lapply(row, function(x) ifelse(is.null(x), NA, x)))
    row <- row %>% tbl_df() %>%
      mutate(
        area = as.double(area),
        gross_area = as.double(gross_area),
        terrain_area = as.double(terrain_area)
      )
    
    ### DROPDOWN
    
    df <- row
    
    drop_cols <- c(
      "bathrooms",
      "rooms",
      "energy_certificate",
      "condition",
      "terrain_area",
      "gross_area",
      #"area",
      "freg",
      "city",
      "district"
    )
    
    drop_cols_final <- NULL
    for(c in drop_cols)
    {
      if(!is.na(df[1, c]))
      {
        row[, c] <- NA
        drop_cols_final <- rbind(drop_cols_final, c)
        df <- rbind(df, row)
      }
    }
    
    drop_cols_final <- rev(rbind(drop_cols_final, "Property and Deal Type"))
    
    
    df <- df %>% map_df(rev)
    
    X <- get_features(df, match_tables)

    pred_price_m2 <- 10 ^ (predict(xgb$price_m2, xgb.DMatrix(data = as.matrix(X))))
    #pred_price <- 10 ^ (predict(xgb$price, xgb.DMatrix(data = as.matrix(X))))
    
    data.frame(
      Characteristic = drop_cols_final,
      price_m2 = currency(pred_price_m2, "", 0),
      price = currency(pred_price_m2 * input$net_area_val, "", 0),
      impact = accounting((pred_price_m2 - lag(pred_price_m2)) * input$net_area_val),
      row.names = NULL
    ) %>%
      formattable(
        align = c("l", "r", "r", "r"),
        list(
          area(col = "Characteristic") ~ formatter(
            "span", style = ~style(color = "grey", font.weight = "bold")),
          price_m2 = normalize_bar("pink", 0.2),
          impact = formatter(
            "span", style = x ~ style(color = ifelse(x < 0, "red", "green")))
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

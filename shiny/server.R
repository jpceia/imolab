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
  
  
  
  observeEvent(input$territory_map_shape_click$id, {
    code <- input$territory_map_shape_click$id
    modify_code <- TRUE
    
    if(is.empty(code))
    {
      rv$location_type <- "Country"
    }
    else
    {
      len <- stringr::str_length(code)
      if(len <= 2) # district
      {
        rv$location_type <- "District"
        df <- city_meta %>% filter(code1 == code)
        city_list <- df$Dicofre
        names(city_list) <- df$Designacao
        updateSelectInput(session, "district", selected = code)
        updateSelectInput(session, "city", choices = city_list)
        updateSelectInput(session, "parish", choices = c(NULL))
      }
      else if(len <= 4) # city
      {
        n_points <- nrow(filtered_dataset() %>% filter(city_code == code))
        if(n_points < MIN_DATAPOINTS)
        {
          modify_code <- FALSE
        }
        else
        {
          rv$location_type <- "City"
          df <- parish_meta %>% filter(code1 == code)
          parish_list <- df$Dicofre
          names(parish_list) <- df$Designacao
          updateSelectInput(session, "city", selected = code)
          updateSelectInput(session, "parish", choices = parish_list) 
        }
      }
      else if(len <= 6) # parish
      {
        n_points <- nrow(filtered_dataset() %>% filter(parish_code == code))
        if(n_points < MIN_DATAPOINTS)
        {
          modify_code <- FALSE
        }
        else
        {
          rv$location_type <- "Parish"
          updateSelectInput(session, "parish", selected = code) 
        }
      }
    }
    
    if(modify_code)
    {
      rv$location_code <- code
    }
    
  }, ignoreInit = TRUE)
  
  
  filtered_dataset <- reactive({
    
    df <- dataset %>%
      filter(PropType %in% input$prop_type) %>%
      filter(DealType == input$deal_type)
    
    df <- switch(
      rv$location_type,
      Country = df %>% filter(district_code %in% district_meta$Dicofre),
      District = df %>% filter(district_code == rv$location_code),
      City = df %>% filter(city_code == rv$location_code),
      Parish = df %>% filter(parish_code == rv$location_code),
      stop("Invalid data")
    )

    validate(need(nrow(df) >= MIN_DATAPOINTS, "Not enough datapoints"))
    
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
    df <- filtered_dataset() 
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
        quantile =  formatter(
          "span", style = ~formattable::style(color = "grey", font.weight = "bold")),
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
      nrow(df) >= MIN_DATAPOINTS,
      "Filtering too narrow: not enough datapoints"
    ))
    
    q <- as.numeric(input$truncation) / 100.0
    quantiles <- quantile(df[[target_col]], probs = c(q, 1 - q))
    
    df %>% ggplot(
      aes_string(
        x = cat_col,
        y = target_col,
        fill = cat_col
      )) +
      stat_boxplot(geom = "errorbar", width = 0.2) +
      geom_boxplot(outlier.alpha = 0.5) +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(trans = 'log10', limits = quantiles) +
      theme(legend.position = "none") +
      coord_flip()
  }
  
  F_catCount <- function(cat_col) {
    
    df <- filtered_dataset()
    df <- df[!is.na(df[[cat_col]]), ]
    
    validate(need(
      nrow(df) >= MIN_DATAPOINTS,
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
            "span", style = ~formattable::style(color = "grey", font.weight = "bold")),
          count = normalize_bar("pink", 0.2),
          median = normalize_bar("lightblue", 0.2)
        ))
  }
  
  
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
  #                                     TERRITORY SECTION
  # ----------------------------------------------------------------------------------------
  
  
  output$territory_tab <- renderUI({
    if(rv$location_type != "Parish")
    {
      html <- fluidRow(
        box(
          column(6, leafletOutput("territory_map")  %>% withSpinner(type=SPINNER_TYPE)),
          column(6, plotOutput("territory_boxplot")  %>% withSpinner(type=SPINNER_TYPE)),
          width = 12
        ),
        box(
          column(12, formattableOutput("territory_table")),
          width = 12
        )
      )
    }
    else
    {
      html <- fluidRow(
        box(
          leafletOutput("parish_map")  %>% withSpinner(type=SPINNER_TYPE),
          width = 12
        ),
        box(
          DT::dataTableOutput("filtered_table") %>% withSpinner(type=SPINNER_TYPE),
          width = 12
        )
      )
    }
    return(html)
  })
  
  
  output$territory_map <- renderLeaflet({
    
    # validate(need(rv$location_type != "Parish", "Invalid location"))
    df <- filtered_dataset()
    code <- rv$location_code

    switch (
      rv$location_type,
      Country = {
        df <- df %>%
          group_by(district_code) %>%
          summarize(
            count = n(price_m2),
            price_m2 = median(price_m2)
          ) %>%
          filter(count >= MIN_DATAPOINTS)
        
        df_map <- country_map_sh
        df_map$price_m2 <- df[match(df_map$id, df$district_code), ]$price_m2
      },
      District = {
        df <- df %>%
          filter(district_code == code) %>%
          group_by(city_code) %>%
          summarize(
            count = n(price_m2),
            price_m2 = median(price_m2)
          ) %>%
          filter(count >= MIN_DATAPOINTS)
        
        df_map <- district_map_sh %>% filter(CCA_1 == code)
        
        df_map$price_m2 <- df[match(df_map$CCA_2, df$city_code), ]$price_m2
      },
      City = {
        df <- df %>%
          filter(city_code == code) %>%
          group_by(parish_code) %>%
          summarize(
            count = n(price_m2),
            price_m2 = median(price_m2)
          ) %>%
          filter(count >= MIN_DATAPOINTS)
        
        df_map <- city_map_sh %>% filter(CCA_2 == code)
        df_map$price_m2 <- df[match(df_map$CCA_3, df$parish_code), ]$price_m2
      }, 
      {
        return(NULL) #stop("Invalid data")
      }
    )


    df_map %>%
      leaflet(options = leafletOptions(
        zoomControl = FALSE,
        attributionControl = FALSE,
        dragging = FALSE,
        scrollWheelZoom = FALSE)) %>%
      addTiles() %>%
      addPolygons(
        color = "#444444", weight = 1, smoothFactor = 0.5, label = ~name, layerId = ~id,
        opacity = 1.0, fillOpacity = 0.75, fillColor = ~colorQuantile("Blues", price_m2)(price_m2),
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))
  })
  
  
  output$parish_map <- renderLeaflet({
    
    validate(need(rv$location_type == "Parish", ""))
    df_map <- city_map_sh %>% filter(CCA_3 == rv$location_code)
    df <- filtered_dataset() %>% 
      group_by(latitude, longitude) %>%
      summarize(price_m2 = round(median(price_m2), 2)) %>%
      ungroup()
    
    pts <- df %>% sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)
    df <- df[sf::st_contains(df_map, pts)[[1]], ]

    df_map %>%
      leaflet(options = leafletOptions(
        zoomControl = FALSE,
        attributionControl = FALSE
        #dragging = FALSE
      )) %>%
      addTiles() %>%
      addPolygons(
        color = "#444444", weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 0.25, fillColor = "cornflowerblue") %>%
      addCircleMarkers(
        data = df,
        radius = 5, color = ~colorQuantile(c("red", "green"), price_m2)(price_m2),
        lng = ~longitude, lat = ~latitude,
        popup = ~htmltools::htmlEscape(paste(price_m2, "Eur/m2")),
        stroke = FALSE, fillOpacity = 0.75
      )
  })
  
  
  output$territory_boxplot <- renderPlot({
    
    df <- filtered_dataset()
    q <- as.numeric(input$truncation) / 100.0
    quantiles <- quantile(df$price_m2, probs = c(q, 1 - q))

    switch (
      rv$location_type,
        Country = {
          df %>%
            drop_na(district_code) %>%
            ggplot() +
            geom_boxplot(
              aes(
                x = fct_reorder(district, price_m2, .fun = median),
                y = price_m2
              ),
              outlier.shape = NA,
              fill = "cornflowerblue", 
              alpha = 0.8,
              size = 0.5)
        },
        District = {
          df %>%
            drop_na(city_code) %>%
            ggplot() +
            geom_boxplot(
              aes(
                x = reorder(city, price_m2, FUN = median, order=TRUE),
                y = price_m2
              ),
              outlier.shape = NA,
              fill = "cornflowerblue", 
              alpha = 0.8,
              size = 0.5)
        },
        City = {
          df %>%
            drop_na(parish_code) %>%
            ggplot() +
            geom_boxplot(
              aes(
                x = str_wrap(reorder(parish, price_m2, FUN = median, order=TRUE), 30),
                y = price_m2
              ),
              outlier.shape = NA,
              fill="cornflowerblue", 
              alpha = 0.8,
              size = 0.5)
        },
        Parish = {
          validate(FALSE, "unavailable data")
        }
    ) +
      #scale_x_discrete(drop = FALSE) +
      scale_y_continuous(trans = 'log10', limits = quantiles) +
      theme(axis.title.y=element_blank(), #axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      theme(legend.position = "none") +
      coord_flip()
  })
  
  output$territory_table <- renderFormattable({
    
    cat_col <- switch(rv$location_type,
                      Country = "district",
                      District = "city",
                      City = "parish",
                      Parish = NULL)
    
    df <- filtered_dataset()
    switch (rv$location_type,
            Country = df %>% drop_na(district_code) %>% group_by(district),
            District = df %>% drop_na(city_code) %>% group_by(city),
            City = df %>% drop_na(parish_code) %>% group_by(parish),
            Parish = {
              validate(FALSE, "unavailable data")
            }
    ) %>%
      summarize(
        count=n(price_m2),
        "25%"=currency(quantile(price_m2, probs=0.25), "", 2),
        median=currency(quantile(price_m2, probs=0.50), "", 2),
        "75%"=currency(quantile(price_m2, probs=0.75), "", 2)) %>%
      formattable(
        align = c("l", "r", "r", "r", "r"),
        list(
          area(col = cat_col) ~ formatter(
            "span", style = ~formattable::style(color = "grey", font.weight = "bold")),
          count = normalize_bar("pink", 0.2),
          median = normalize_bar("lightblue", 0.2)
        )
      )
  })
  
  output$filtered_table <- DT::renderDataTable(
    filtered_dataset() %>% select(-district_code, -city_code, -parish_code),
    filter = 'top', options = list(scrollX = TRUE)
  )
  
  
  # ----------------------------------------------------------------------------------------
  #                                    DATA SOURCES SECTION
  # ----------------------------------------------------------------------------------------
  
  output$rawDataTable <- DT::renderDataTable(
    dataset %>% select(
      -district_code,
      -city_code,
      -parish_code,
      -latitude,
      -longitude),
    filter = 'top', options = list(scrollX = TRUE))
  
  output$pivotTable <- renderRpivotTable({
    rpivotTable(
      dataset,
      rows = "district",
      cols = c("DealType", "PropType"),
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
      DealType = input$deal_type_val,
      PropType = input$prop_type_val,
      
      district_code = input$district_val,
      city_code = input$city_val,
      parish_code = input$parish_val,
      
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
      "parish_code",
      "city_code",
      "district_code"
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
            "span", style = ~formattable::style(color = "grey", font.weight = "bold")),
          price_m2 = normalize_bar("pink", 0.2),
          impact = formatter(
            "span", style = x ~ formattable::style(color = ifelse(x < 0, "red", "green")))
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

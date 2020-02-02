library(ggthemes)
library(Cairo)
options(shiny.usecairo=TRUE)



# ----------------------------------------------------------------------------------------
#                                         SERVER
# ----------------------------------------------------------------------------------------


shinyServer(function(input, output, session) {
  
  session$allowReconnect(TRUE)
  
  rv <- reactiveValues(
    location_code = NULL,
    location_type = "District")
  
  observeEvent(input$district, {
    
    if(is.empty(input$district))
    {
      rv$location_code <- NULL
      rv$location_type <- "District"
      
      municipality_list <- c(NULL)
    }
    else
    {
      rv$location_code <- input$district
      rv$location_type <- "Municipality"
      
      df <- municipality_sh %>% filter(CCA_1 == input$district)
      municipality_list <- df$id
      names(municipality_list) <- df$name
    }
    
    updateSelectInput(session, "municipality", choices = municipality_list)
    updateSelectInput(session, "parish",       choices = c(NULL))
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$municipality, {

    if(is.empty(input$municipality))
    {
      rv$location_code <- input$district
      rv$location_type <- "Municipality"
      
      parish_list <- c(NULL)
    }
    else
    {
      rv$location_code <- input$municipality
      rv$location_type <- "Parish"
      
      df <- parish_sh %>% filter(CCA_2 == input$municipality)
      parish_list <- df$id
      names(parish_list) <- df$name
    }

    updateSelectInput(session, "parish", choices = parish_list)
  }, ignoreInit = TRUE)
  
  observeEvent(input$parish, {
    
    if(is.empty(input$parish))
    {
      rv$location_code <- input$municipality
      rv$location_type <- "Parish"
    }
    else
    {
      rv$location_code <- input$parish
      rv$location_type <- "StatisticalSection"
    }
  }, ignoreInit = TRUE)
  
  
  
  observeEvent(input$territory_map_shape_click$id, {
    code <- input$territory_map_shape_click$id
    modify_code <- TRUE
    
    if(is.empty(code))
    {
      rv$location_type <- "District"
    }
    else
    {
      len <- stringr::str_length(code)
      if(len <= 2) # district
      {
        rv$location_type <- "Municipality"
        df <- municipality_sh %>% filter(CCA_1 == code)
        municipality_list <- df$id
        names(municipality_list) <- df$name
        updateSelectInput(session, "district", selected = code)
        updateSelectInput(session, "municipality", choices = municipality_list)
        updateSelectInput(session, "parish", choices = c(NULL))
      }
      else if(len <= 4) # municipality
      {
        n_points <- nrow(filtered_dataset() %>% filter(MunicipalityID == code))
        if(n_points < MIN_DATAPOINTS)
        {
          modify_code <- FALSE
        }
        else
        {
          rv$location_type <- "Parish"
          df <- parish_sh %>% filter(CCA_2 == code)
          parish_list <- df$id
          names(parish_list) <- df$name
          updateSelectInput(session, "municipality", selected = code)
          updateSelectInput(session, "parish",       choices = parish_list) 
        }
      }
      else if(len <= 6) # parish
      {
        n_points <- nrow(filtered_dataset() %>% filter(ParishID == code))
        if(n_points < MIN_DATAPOINTS)
        {
          modify_code <- FALSE
        }
        else
        {
          rv$location_type <- "StatisticalSection"
          updateSelectInput(session, "parish", selected = code) 
        }
      }
    }
    
    if(modify_code)
    {
      rv$location_code <- code
    }
    
  }, ignoreInit = TRUE)
  
  filtered_dataset_noprop <- reactive({
    df <- dataset %>%
      filter(Deal == input$deal)
    
    df <- switch(
      rv$location_type,
      District           = df %>% filter(DistrictID  %in% district_sh$CCA_1),
      Municipality       = df %>% filter(DistrictID     == rv$location_code),
      Parish             = df %>% filter(MunicipalityID == rv$location_code),
      StatisticalSection = df %>% filter(ParishID       == rv$location_code),
      stop("Invalid data")
    )
    
    validate(need(nrow(df) >= MIN_DATAPOINTS, MIN_DATAPOINTS_MSG))
    
    return(df)
  })
  
  
  filtered_dataset <- reactive({
    
    df <- filtered_dataset_noprop() %>%
        filter(`Property Type` %in% input$prop_type)
    
    validate(need(nrow(df) >= MIN_DATAPOINTS, MIN_DATAPOINTS_MSG))
    
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
    filtered_dataset() %>% hc_hist("Price", "EUR", "", input$truncation)
  )
  
  output$HistogramArea <- renderHighchart(
    filtered_dataset() %>% hc_hist("Area", "m2", "", input$truncation)
  )
  
  # -------------------------------------- FORMATTABLE -------------------------------------
  
  output$tableQuantiles <- renderFormattable({
    df <- filtered_dataset() 
    probs <- c(0.95, 0.90, 0.75, 0.50, 0.25, 0.10, 0.05)
    table <- data.frame(
      quantile = percent(probs, 0),
      Price = currency(quantile(df$Price, probs = probs), "", 0),
      Area = currency(quantile(df$Area, probs = probs), "", 0),
      price_m2 = currency(quantile(df$price_m2, probs = probs), "", 0)
    )
    row.names(table) <- NULL
    
    formattable(
      table,
      align = c("c", "r", "r", "r"),
      list(
        quantile =  formatter(
          "span", style = ~formattable::style(color = "grey", font.weight = "bold")),
        Price = normalize_bar("lightpink", 0.2),
        Area = normalize_bar("lightpink", 0.2),
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
    df <- df[!is.na(df[[target_col]]), ]
    
    validate(need(
      nrow(df) >= MIN_DATAPOINTS,
      MIN_DATAPOINTS_MSG
    ))
    
    q <- 0.001 #as.numeric(input$truncation) / 100.0
    quantiles <- quantile(df[[target_col]], probs = c(q, 1 - q))
    df <- df[between(df[[target_col]], quantiles[1], quantiles[2]), ]
    
    if (is.numeric(df[[cat_col]]))
    {
      df[[cat_col]] <- as.factor(df[[cat_col]])
    }
    
    cat_var <- rlang::sym(cat_col)
    target_var <- rlang::sym(target_col)
    
    df %>% ggplot(
      aes(
        x = !!cat_var,
        y = !!target_var,
        fill = !!cat_var
      )) +
      stat_boxplot(
        geom = "errorbar",
        width = 0.2
      ) +
      geom_boxplot(outlier.shape = NA) +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(trans = 'log10') +
      theme(
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none"
      ) +
      coord_flip()
  }
  
  F_catCount <- function(cat_col, target_col = "price_m2") {
    
    df <- filtered_dataset()
    df <- df[!is.na(df[[cat_col]]), ]
    df <- df[!is.na(df[[target_col]]), ]
    
    validate(need(
      nrow(df) >= MIN_DATAPOINTS,
      MIN_DATAPOINTS_MSG
    ))
    
    if (is.numeric(df[[cat_col]]))
    {
      df[[cat_col]] <- as.factor(df[[cat_col]])
    }
    
    cat_var <- rlang::sym(cat_col)
    
    ggplot(df, aes(
        x = !!cat_var,
        fill = !!cat_var
      )) +
      geom_bar(color = "black", lwd = 0.5) +
      geom_text(stat = 'count', aes(label=..count..),
                hjust=-0.3, check_overlap = TRUE) +
      scale_x_discrete(drop = FALSE) +
      theme(
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none"
      ) +
      coord_flip()
  }
  
  F_catTable <- function(cat_col,  target_col = "price_m2") {
    
    df <- filtered_dataset()
    df <- df[!is.na(df[[cat_col]]), ]
    df <- df[!is.na(df[[target_col]]), ]
    
    validate(need(
      nrow(df) >= MIN_DATAPOINTS,
      MIN_DATAPOINTS_MSG
    ))
    
    target <- rlang::sym(target_col)
    
    df %>%
      group_by_at(vars(one_of(cat_col))) %>%
      summarize(
        count = n(),
        "25%" = currency(quantile(!!target, probs=0.25), "", 2),
        median= currency(quantile(!!target, probs=0.50), "", 2),
        "75%" = currency(quantile(!!target, probs=0.75), "", 2)
      ) %>%
      arrange(-row_number()) %>%
      drop_na() %>%
      formattable(
        align = c("l", "r", "r", "r", "r"),
        list(
          area(col = cat_col) ~ formatter(
            "span", style = ~formattable::style(color = "grey", font.weight = "bold")),
          count = normalize_bar("pink", 0.2),
          median = normalize_bar("lightblue", 0.2)
        ))
  }
  
  output$CategoryTextTargetName <- renderText(target_name(input$target_col))
  
  output$EnergyCertificateBoxPlot <- renderPlot(F_catBoxPlot("Energy Certificate", input$target_col))
  output$EnergyCertificateCount <- renderPlot(F_catCount("Energy Certificate", input$target_col))
  output$EnergyCertificateTable <- renderFormattable(F_catTable("Energy Certificate", input$target_col))
  
  output$ConditionBoxPlot <- renderPlot(F_catBoxPlot("Condition", input$target_col))
  output$ConditionCount <- renderPlot(F_catCount("Condition", input$target_col))
  output$ConditionTable <- renderFormattable(F_catTable("Condition", input$target_col))
  
  output$BedroomsBoxPlot <- renderPlot(F_catBoxPlot("Bedrooms", input$target_col))
  output$BedroomsCount <- renderPlot(F_catCount("Bedrooms", input$target_col))
  output$BedroomsTable <- renderFormattable(F_catTable("Bedrooms", input$target_col))
  
  output$BathroomsBoxPlot <- renderPlot(F_catBoxPlot("Bathrooms", input$target_col))
  output$BathroomsCount <- renderPlot(F_catCount("Bathrooms", input$target_col))
  output$BathroomsTable <- renderFormattable(F_catTable("Bathrooms", input$target_col))
  
  output$ConstructionDecadeBoxPlot <- renderPlot(F_catBoxPlot("Construction Decade", input$target_col))
  output$ConstructionDecadeCount <- renderPlot(F_catCount("Construction Decade", input$target_col))
  output$ConstructionDecadeTable <- renderFormattable(F_catTable("Construction Decade", input$target_col))

  
  # ----------------------------------------------------------------------------------------
  #                               CORRELATION EXPLORER SECTION
  # ----------------------------------------------------------------------------------------
  
  output$CorrelationTextTargetName <- renderText({
    target1 <- target_name(input$target1)
    target2 <- target_name(input$target2)
    paste(target1, "vs", target2, ", by", input$agg_level)
  })
  
  output$CorrelationPlot <- renderHighchart({
    agg_col <- input$agg_level
    target1_col <- input$target1
    target2_col <- input$target2
    target1 <- rlang::sym(target1_col)
    target2 <- rlang::sym(target2_col)

    if(input$agg_prop_type)
    {
      js <- "
        function () {
          return '  <strong>' + this.point.%s + '</strong><br>' + 
                 '<strong>' + '%s:</strong> ' + this.point.x + '<br>' + 
                 '<strong>' + '%s:</strong> ' + this.point.y; }"
      df <- filtered_dataset() %>%
        group_by_at(vars(one_of(agg_col))) %>%
        summarize(
          count = n(),
          !!target1_col := median(!!target1),
          !!target2_col := median(!!target2)
        ) %>%
        filter(count >= MIN_DATAPOINTS) %>%
        hchart("scatter", hcaes(!!target1, !!target2)) %>%
        hc_tooltip(formatter=JS(sprintf(js, agg_col, target1_col, target2_col)))
    }
    else
    {
      js <- "
        function () {
          return '  <strong>' + this.point.%s + '</strong>' +
                 '<span style=\"color:' + this.series.color + '\">\u25CF</span> ' + this.series.name + '<br><br>' + 
                 '<strong>' + '%s:</strong> ' + this.point.x + '<br>' + 
                 '<strong>' + '%s:</strong> ' + this.point.y; }"
      filtered_dataset() %>%
        group_by_at(vars(one_of(c(agg_col, "Property Type")))) %>%
        summarize(
          count = n(),
          !!target1_col := median(!!target1),
          !!target2_col := median(!!target2)
        ) %>%
        filter(count >= MIN_DATAPOINTS) %>%
        hchart("scatter", hcaes(!!target1, !!target2, group = `Property Type`)) %>%
        hc_tooltip(formatter=JS(sprintf(js, agg_col, target1_col, target2_col)))
    }
  })
  
  
  # ----------------------------------------------------------------------------------------
  #                                     TERRITORY SECTION
  # ----------------------------------------------------------------------------------------
  
  output$TerritoryTextTargetName <- renderText(target_name(input$target_col))
  
  output$territory_tab <- renderUI({
    if(rv$location_type != "StatisticalSection")
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
    
    #validate(need(rv$location_type != "StatisticalSection", "Invalid location"))
    code <- rv$location_code
    target_col <- input$target_col
    target <- rlang::sym(target_col)
    
    df <- filtered_dataset()
    df <- df[!is.na(df[[target_col]]), ]

    switch (
      rv$location_type,
      District = {
        df <- df %>%
          group_by(DistrictID) %>%
          summarize(
            count = n(),
            value = median(!!target)
          ) %>%
          filter(count >= MIN_DATAPOINTS)
        
        df_map <- district_sh
        df_map$value <- df[match(df_map$id, df$DistrictID), ]$value
      },
      Municipality = {
        df <- df %>%
          filter(DistrictID == code) %>%
          group_by(MunicipalityID) %>%
          summarize(
            count = n(),
            value = median(!!target)
          ) %>%
          filter(count >= MIN_DATAPOINTS)
        
        df_map <- municipality_sh %>% filter(CCA_1 == code)
        df_map$value <- df[match(df_map$CCA_2, df$MunicipalityID), ]$value
      },
      Parish = {
        df <- df %>%
          filter(MunicipalityID == code) %>%
          group_by(ParishID) %>%
          summarize(
            count = n(),
            value = median(!!target)
          ) %>%
          filter(count >= MIN_DATAPOINTS)
        
        df_map <- parish_sh %>% filter(CCA_2 == code)
        df_map$value <- df[match(df_map$CCA_3, df$ParishID), ]$value
      }, 
      {
        validate(FALSE, "")
      }
    )

    df_map %>%
      leaflet(options = leafletOptions(
        #zoomControl = FALSE,
        #dragging = FALSE,
        attributionControl = FALSE,
        scrollWheelZoom = FALSE
      )) %>%
      addTiles() %>%
      addPolygons(
        color = "#444444", weight = 1, smoothFactor = 0.5, label = ~name, layerId = ~id,
        opacity = 1.0, fillOpacity = 0.75, fillColor = ~qpal("Blues", value),
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))
  })
  
  
  output$parish_map <- renderLeaflet({
    
    target_col <- input$target_col
    
    validate(need(rv$location_type == "StatisticalSection", ""))
    validate(need(target_col != "xYield", "xYield target not allowed for parish level view"))
    
    target <- rlang::sym(target_col)
    
    df <- filtered_dataset()
    df <- df[!is.na(df[[target_col]]), ]
    
    df_map <- parish_sh %>% filter(CCA_3 == rv$location_code)
    
    df <- df %>% 
      group_by(Latitude, Longitude) %>%
      summarize(value = round(median(!!target), 2)) %>%
      ungroup()
    
    pts <- df %>% sf::st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326)
    df <- df[sf::st_contains(df_map, pts)[[1]], ]
    
    unit_str <- switch(
      target_col,
      "price_m2" = "Eur/m2",
      "Area" = "m2",
      "Construction Decade" = ""
    )

    df_map %>%
      leaflet(options = leafletOptions(
        #zoomControl = FALSE,
        #dragging = FALSE,
        attributionControl = FALSE,
        scrollWheelZoom = TRUE
      )) %>%
      addTiles() %>%
      addPolygons(
        color = "#444444", weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 0.25, fillColor = "cornflowerblue") %>%
      addCircleMarkers(
        data = df,
        radius = 5, color = ~qpal(c("red", "green"), value),
        lng = ~Longitude, lat = ~Latitude,
        popup = ~htmltools::htmlEscape(paste(value, unit_str)),
        stroke = FALSE, fillOpacity = 0.75
      )
  })
  
  
  output$territory_boxplot <- renderPlot({
    
    cat_col <- rv$location_type
    validate(need(cat_col != "StatisticalSection", ""))
    
    target_col <- input$target_col
    target <- rlang::sym(target_col)
    
    df <- filtered_dataset()
    df <- df[!is.na(df[[target_col]]), ]
    df <- df[!is.na(df[[cat_col]]), ]
    df$tmp <- stringr::str_wrap(df[[cat_col]], 25)
    
    q <- 0.001 #as.numeric(input$truncation) / 100.0
    quantiles <- quantile(df[[target_col]], probs = c(q, 1 - q))
    df <- df[between(df[[target_col]], quantiles[1], quantiles[2]), ]
    
    median_val <- median(df[[target_col]])
    
    df %>% ggplot(
      aes(
        x = reorder(tmp, !!target, FUN = median),
        y = !!target)
      ) +
      stat_boxplot(
        geom = "errorbar",
        width = 0.2
      ) +
      geom_boxplot(
        outlier.shape = NA,
        #fill = "#8DBEDA",
      ) +
      scale_y_continuous(trans = 'log10') +
      theme(
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.position = "none"
      ) +
      geom_hline(
        yintercept = median_val,
        linetype = "dashed", 
        color = "red",
        size = 1) +
      coord_flip()
  })
  
  output$territory_table <- renderFormattable({
    
    cat_col <- rv$location_type
    validate(need(cat_col != "StatisticalSection", ""))
    
    target_col <- input$target_col
    target <- rlang::sym(target_col)
    
    df <- filtered_dataset()
    df <- df[!is.na(df[[target_col]]), ]
    df <- df[!is.na(df[[cat_col]]), ]
    
    df %>%
      group_by_at(vars(one_of(cat_col))) %>%
      summarize(
        count = n(),
        "25%" = currency(quantile(!!target, probs=0.25), "", 2),
        median= currency(quantile(!!target, probs=0.50), "", 2),
        "75%" = currency(quantile(!!target, probs=0.75), "", 2)
      ) %>%
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
    filtered_dataset() %>% select(
      `Property Type`,
      Price,
      price_m2,
      Area,
      `Gross Area`,
      `Terrain Area`,
      Bedrooms,
      Bathrooms,
      `Energy Certificate`,
      `Construction Year`,
      Condition),
    filter = 'top', options = list(scrollX = TRUE)
  )

  # ----------------------------------------------------------------------------------------
  #                                     VALUATION SECTION
  # ----------------------------------------------------------------------------------------
  
  callModule(server_valuation, "mod_valuation")
  

  # ----------------------------------------------------------------------------------------
  #                                      SEARCH SECTION
  # ----------------------------------------------------------------------------------------
  
  callModule(server_search, "mod_search")
})

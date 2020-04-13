library(ggthemes)
library(Cairo)
options(shiny.usecairo=TRUE)



# ----------------------------------------------------------------------------------------
#                                         SERVER
# ----------------------------------------------------------------------------------------


shinyServer(function(input, output, session) {
  
  session$allowReconnect(TRUE)
  
  rv <- reactiveValues(code = NULL)
  
  observeEvent(input$district, {
    rv$code <- input$district
  }, ignoreInit = TRUE)
  
  observeEvent(input$municipality, {
    rv$code <- ifelse(is.empty(input$municipality), input$district, input$municipality)
  }, ignoreInit = TRUE)
  
  observeEvent(input$parish, {
    rv$code <- ifelse(is.empty(input$parish), input$municipality, input$parish)
  }, ignoreInit = TRUE)
  
  observeEvent(input$territory_map_shape_click$id, {
    rv$code <- input$territory_map_shape_click$id
  })
  
  output$geomenu <- renderUI({
    html <- switch(
      location_type(rv$code),
      country = {
        tags$div(
          selectizeInput("district", "Location", district_list,
                         options = list(
                           placeholder = 'District',
                           onInitialize = I('function() { this.setValue(""); }')
                         )
          )
        )
      },
      district = {
        district_code <- rv$code
        
        tmp <- municipality_sh %>% filter(CCA_1 == district_code)
        municipality_list <- setNames(tmp$CCA_2, tmp$name)
        
        tags$div(
          selectizeInput("district", "Location", district_list,
                         selected = district_code
          ),
          selectizeInput("municipality", NULL, municipality_list,
                         options = list(
                           placeholder = 'Municipality',
                           onInitialize = I('function() { this.setValue(""); }')
                         )
          )
        )
      },
      municipality = {
        municipality_code <- rv$code
        district_code <-  stringr::str_sub(municipality_code, end = -3)
        
        tmp <- parish_sh %>% filter(CCA_2 == municipality_code)
        parish_list <- setNames(tmp$CCA_3, tmp$name)
        
        tmp <- municipality_sh %>% filter(CCA_1 == district_code)
        municipality_list <- setNames(tmp$CCA_2, tmp$name)
        
        tags$div(
          selectizeInput("district", "Location", district_list,
                         selected = district_code
          ),
          selectizeInput("municipality", NULL, municipality_list,
                         selected = municipality_code
          ),
          selectizeInput("parish", NULL, parish_list,
                         options = list(
                           placeholder = 'Parish',
                           onInitialize = I('function() { this.setValue(""); }')
                         )
          )
        )
      },
      parish = {
        parish_code <- rv$code
        municipality_code <- stringr::str_sub(parish_code, end = -3)
        district_code <-  stringr::str_sub(municipality_code, end = -3)
        
        tmp <- parish_sh %>% filter(CCA_2 == municipality_code)
        parish_list <- setNames(tmp$CCA_3, tmp$name)
        
        tmp <- municipality_sh %>% filter(CCA_1 == district_code)
        municipality_list <- setNames(tmp$CCA_2, tmp$name)
        
        tags$div(
          selectizeInput("district", "Location", district_list,
                         selected = district_code
          ),
          selectizeInput("municipality", NULL, municipality_list,
                         selected = municipality_code
          ),
          selectizeInput("parish", NULL, parish_list,
                         selected = parish_code
          )
        )
      }
    )
    
    return(html)
  })
  
  filtered_dataset <- reactive({
    
    df <- dataset[
      (Deal == input$deal) &
      (Property.Type %in% input$prop_type)
    ]
    
    code <- rv$code
    
    df <- switch(
      location_type(code),
      country = {
        df[DistrictID  %in% district_sh$CCA_1]
      },
      district = {
        df[DistrictID == code]
      },
      municipality = {
        df[MunicipalityID == code]
      },
      parish = {
        df[ParishID == code]
      }
    )

    shiny::validate(need(nrow(df) >= MIN_DATAPOINTS, MIN_DATAPOINTS_MSG))
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
    
    df <- filtered_dataset()[
      !is.na(get(cat_col)) &
      !is.na(get(target_col))
    ]
    
    if (is.numeric(df[[cat_col]]))
    {
      df[[cat_col]] <- as.factor(df[[cat_col]])
    }
    
    df[
      ,
      .(
        min = quantile(get(target_col), probs = 0.05),
        low = quantile(get(target_col), probs = 0.25),
        mid = quantile(get(target_col), probs = 0.50),
        top = quantile(get(target_col), probs = 0.75),
        max = quantile(get(target_col), probs = 0.95)
      ),
      cat_col
    ][
      !is.na(get(cat_col))
    ] %>% ggplot(
      aes_string(
        x = cat_col,
        fill = cat_col,
        ymin = 'min',
        lower = 'low',
        middle = 'mid', 
        upper = 'top', 
        ymax = 'max'
      )
    ) +
      geom_errorbar(width = 0.2) +
      geom_boxplot(stat = "identity") +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(trans = 'log10') +
      theme(
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        legend.position = "none"
      ) +
      coord_flip()
  }
  
  F_catCount <- function(cat_col, target_col = "price_m2") {
    
    df <- filtered_dataset()[
      !is.na(get(cat_col)) &
      !is.na(get(target_col))
    ]
    
    if (is.numeric(df[[cat_col]]))
    {
      df[[cat_col]] <- as.factor(df[[cat_col]])
    }
    
    df[
      ,
      .(count = .N),
      cat_col
    ][
      !is.na(get(cat_col))
    ] %>%
    ggplot(aes_string(
        x = cat_col,
        fill = cat_col,
        y = 'count',
        label = 'count'
      )) +
      geom_bar(stat = "identity", color = "black", lwd = 0.5) +
      geom_text(stat = "identity", hjust = -0.3, check_overlap = TRUE) +
      scale_x_discrete(drop = FALSE) +
      theme(
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_blank(),
        legend.position = "none"
      ) +
      coord_flip()
  }
  
  F_catTable <- function(cat_col,  target_col = "price_m2") {
    
    filtered_dataset()[
      !is.na(get(cat_col)) & !is.na(get(target_col)),
      .(
        count = .N,
        "25%" = currency(quantile(get(target_col), probs=0.25), "", 2),
        median= currency(quantile(get(target_col), probs=0.50), "", 2),
        "75%" = currency(quantile(get(target_col), probs=0.75), "", 2)
      ),
      cat_col
    ][
      rev(order(get(cat_col)))
    ] %>%
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
  
  output$EnergyCertificateBoxPlot <- renderPlot(F_catBoxPlot("Energy.Certificate", input$target_col))
  output$EnergyCertificateCount <- renderPlot(F_catCount("Energy.Certificate", input$target_col))
  output$EnergyCertificateTable <- renderFormattable(F_catTable("Energy.Certificate", input$target_col))
  
  output$ConditionBoxPlot <- renderPlot(F_catBoxPlot("Condition", input$target_col))
  output$ConditionCount <- renderPlot(F_catCount("Condition", input$target_col))
  output$ConditionTable <- renderFormattable(F_catTable("Condition", input$target_col))
  
  output$BedroomsBoxPlot <- renderPlot(F_catBoxPlot("Bedrooms", input$target_col))
  output$BedroomsCount <- renderPlot(F_catCount("Bedrooms", input$target_col))
  output$BedroomsTable <- renderFormattable(F_catTable("Bedrooms", input$target_col))
  
  output$BathroomsBoxPlot <- renderPlot(F_catBoxPlot("Bathrooms", input$target_col))
  output$BathroomsCount <- renderPlot(F_catCount("Bathrooms", input$target_col))
  output$BathroomsTable <- renderFormattable(F_catTable("Bathrooms", input$target_col))
  
  output$ConstructionDecadeBoxPlot <- renderPlot(F_catBoxPlot("Construction.Decade", input$target_col))
  output$ConstructionDecadeCount <- renderPlot(F_catCount("Construction.Decade", input$target_col))
  output$ConstructionDecadeTable <- renderFormattable(F_catTable("Construction.Decade", input$target_col))

  
  # ----------------------------------------------------------------------------------------
  #                               CORRELATION EXPLORER SECTION
  # ----------------------------------------------------------------------------------------
  
  output$corr_level_input <- renderUI({

    switch(
      location_type(rv$code),
      country = {
        levels <- c("District", "Municipality")
        selected <- "District"
      },
      district = {
        levels <- c("Municipality", "Parish")
        selected <- "Municipality"
      },
      municipality = {
        levels <- c("Parish")
        selected <- "Parish"
      },
      parish = {
        levels <- c("Parish")
        selected <- "Parish"
      }
    )
    
    selectizeInput("agg_level", "Aggregation Level",
                   levels, selected = selected)
  })
  
  output$CorrelationTextTargetName <- renderText({
    target1 <- target_name(input$target1)
    target2 <- target_name(input$target2)
    paste(target1, "vs", target2, ", by", input$agg_level)
  })
  
  output$CorrelationPlot <- renderHighchart({
    agg_col <- input$agg_level
    target1 <- input$target1
    target2 <- input$target2
    
    if(input$agg_prop_type)
    {
      js <- "
        function () {
          return '  <strong>' + this.point.%s + '</strong><br>' + 
                 '<strong>' + '%s:</strong> ' + this.point.x + '<br>' + 
                 '<strong>' + '%s:</strong> ' + this.point.y; }"
      filtered_dataset()[
        ,
        as.list(setNames(
          c(
            .N,
            median(get(target2), na.rm = TRUE),
            median(get(target1), na.rm = TRUE)
          ),
          c("count", "X", "Y")
        )),
        agg_col
      ][
        count >= MIN_DATAPOINTS
      ] %>%
        hchart("scatter", hcaes(X, Y)) %>%
        hc_xAxis(title = list(text = target2)) %>%
        hc_yAxis(title = list(text = target1)) %>%
        hc_tooltip(formatter=JS(sprintf(js, agg_col, target2, target1)))
    }
    else
    {
      js <- "
        function () {
          return '  <strong>' + this.point.%s + '</strong>' +
                 '<span style=\"color:' + this.series.color + '\">\u25CF</span> ' + this.series.name + '<br><br>' + 
                 '<strong>' + '%s:</strong> ' + this.point.x + '<br>' + 
                 '<strong>' + '%s:</strong> ' + this.point.y; }"
      filtered_dataset()[
        ,
        as.list(setNames(
          c(
            .N,
            median(get(target2), na.rm = TRUE),
            median(get(target1), na.rm = TRUE)
          ),
          c("count", "X", "Y")
        )),
        c(agg_col, "Property.Type")
      ][
        count >= MIN_DATAPOINTS
      ] %>%
        hchart("scatter", hcaes(X, Y, group = Property.Type)) %>%
        hc_xAxis(title = list(text = target2)) %>%
        hc_yAxis(title = list(text = target1)) %>%
        hc_tooltip(formatter=JS(sprintf(js, agg_col, target2, target1)))
    }
  })
  
  
  # ----------------------------------------------------------------------------------------
  #                                     TERRITORY SECTION
  # ----------------------------------------------------------------------------------------
  
  output$TerritoryTextTargetName <- renderText(target_name(input$target_col))
  
  output$territory_tab <- renderUI({
    if(location_type(rv$code) != "parish")
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
          leafletOutput("territory_map")  %>% withSpinner(type=SPINNER_TYPE),
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
    
    target_col <- input$target_col
    
    loc_type <- location_type(rv$code)
    
    df <- filtered_dataset()
    df <- df[
      !is.na(get(target_col))
    ]
    
    map <- leaflet(options = leafletOptions(
      attributionControl = FALSE,
      scrollWheelZoom = FALSE)
    ) %>%
      addTiles()

    switch (
      loc_type,
      country = {
        df <- df[
          ,
          .(
            count = .N,
            value = median(get(target_col), rm.na = TRUE)
          ),
          DistrictID
        ][
          count >= MIN_DATAPOINTS
        ]
        
        df_map <- district_sh
        df_map$id <- df_map$CCA_1
        df_map$value <- df[match(df_map$CCA_1, df$DistrictID), ]$value
      },
      district = {
        df <- df[
          DistrictID == rv$code,
          .(
            count = .N,
            value = median(get(target_col), rm.na = TRUE)
          ),
          MunicipalityID
        ][
          count >= MIN_DATAPOINTS
        ]
        
        df_map <- municipality_sh %>% filter(CCA_1 == rv$code)
        df_map$id <- df_map$CCA_2
        df_map$value <- df[match(df_map$CCA_2, df$MunicipalityID), ]$value
      },
      municipality = {
        df <- df[
          MunicipalityID == rv$code,
          .(
            count = .N,
            value = median(get(target_col), rm.na = TRUE)
          ),
          ParishID
        ][
          count >= MIN_DATAPOINTS
        ]
        
        df_map <- parish_sh %>% filter(CCA_2 == rv$code)
        df_map$id <- df_map$CCA_3
        df_map$value <- df[match(df_map$CCA_3, df$ParishID), ]$value
      },
      parish = {
        
        coords <- df[
          ,
          .(
            value = round(median(get(target_col), na.rm = TRUE), 2)
          ),
          .(Latitude, Longitude)
        ][
          !is.na(value)
        ]
        
        
        df_map <- parish_sh %>% filter(CCA_3 == rv$code)
        
        pts <- coords %>% sf::st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326)
        coords <- coords[sf::st_contains(df_map, pts)[[1]]]
        
        unit_str <- switch(
          target_col,
          "price_m2" = "Eur/m2",
          "Area" = "m2",
          "Construction.Decade" = ""
        )
        
        map <- map %>%
          addPolygons(
            data = df_map,
            color = "#444444", weight = 1, smoothFactor = 0.5,
            opacity = 1.0, fillOpacity = 0.25, fillColor = "cornflowerblue") %>%
          addCircleMarkers(
            data = coords,
            radius = 3, color = ~qpal(c("red", "green"), value),
            lng = ~Longitude, lat = ~Latitude,
            popup = ~htmltools::htmlEscape(paste(value, unit_str)),
            stroke = 0.5, fill = FALSE, fillOpacity = 1,
            group = "Historical"
          )
      },
      {
        shiny::validate(FALSE, "")
      }
    )
    
    return(map)
  })
  
  
  output$territory_boxplot <- renderPlot({
    
    cat_col <- switch(
      location_type(rv$code),
      country = "District",
      district = "Municipality",
      municipality = "Parish",
      shiny::validate(FALSE, "")
    )
    
    df <- filtered_dataset()
    
    target_col <- input$target_col
    median_val <- median(df[[target_col]])
    
    df[
      !is.na(get(target_col)) & !is.na(get(cat_col)),
      .(
        min = quantile(get(target_col), probs = 0.05),
        low = quantile(get(target_col), probs = 0.25),
        mid = quantile(get(target_col), probs = 0.50),
        top = quantile(get(target_col), probs = 0.75),
        max = quantile(get(target_col), probs = 0.95)
      ),
      .(label = stringr::str_wrap(get(cat_col), 25))
    ][
      !is.na(label)
    ] %>%
      ggplot(
        aes(
          x = reorder(label, mid, FUN = median),
          ymin = min,
          lower = low,
          middle = mid,
          upper = top, 
          ymax = max
      )
    ) +
      geom_errorbar(width = 0.2) +
      geom_boxplot(stat = "identity") +
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
    
    cat_col <- switch(
      location_type(rv$code),
      country = "District",
      district = "Municipality",
      municipality = "Parish",
      shiny::validate(FALSE, "")
    )
    
    target_col <- input$target_col
    target <- rlang::sym(target_col)
    
    filtered_dataset()[
      !is.na(get(target_col)) &
      !is.na(get(cat_col)),
      .(
        count = .N,
        "25%" = currency(quantile(get(target_col), probs=0.25), "", 2),
        median= currency(quantile(get(target_col), probs=0.50), "", 2),
        "75%" = currency(quantile(get(target_col), probs=0.75), "", 2)
      ),
      cat_col
    ][
      order(get(cat_col))
    ] %>%
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
    filtered_dataset()[
      ,
      .(
        Property.Type,
        Price,
        price_m2,
        Area,
        Gross.Area,
        Terrain.Area,
        Bedrooms,
        Bathrooms,
        Energy.Certificate,
        Construction.Year,
        Condition
      )
    ],
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

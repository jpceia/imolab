library(ggthemes)
library(Cairo)
options(shiny.usecairo=TRUE)



# ----------------------------------------------------------------------------------------
#                                         SERVER
# ----------------------------------------------------------------------------------------


shinyServer(function(input, output, session) {
  
  session$allowReconnect(TRUE)
  
  rv <- reactiveValues(
    df = NULL,
    code = NULL)
  
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
  
  observe({
    
    df <- dataset[
      (Deal == input$deal) &
      (Property.Type %in% input$prop_type)
    ]
    
    code <- rv$code
    
    rv$df <- switch(
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
  })
  
  
  # ----------------------------------------------------------------------------------------
  #                                     NUMERICAL SECTION
  # ----------------------------------------------------------------------------------------
  
  # -------------------------------------- HIGHCHARTS --------------------------------------

  output$HistogramPrice_m2 <- renderHighchart(
    rv$df %>% hc_hist("price_m2", "EUR/m2", "", input$truncation)
  )
  
  output$HistogramPrice <- renderHighchart(
    rv$df %>% hc_hist("Price", "EUR", "", input$truncation)
  )
  
  output$HistogramArea <- renderHighchart(
    rv$df %>% hc_hist("Area", "m2", "", input$truncation)
  )
  
  # -------------------------------------- FORMATTABLE -------------------------------------
  
  output$tableQuantiles <- renderFormattable({
    probs <- c(0.95, 0.90, 0.75, 0.50, 0.25, 0.10, 0.05)
    table <- data.frame(
      quantile = percent(probs, 0),
      Price = currency(quantile(rv$df$Price, probs = probs), "", 0),
      Area = currency(quantile(rv$df$Area, probs = probs), "", 0),
      price_m2 = currency(quantile(rv$df$price_m2, probs = probs), "", 0)
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
    
    df <- rv$df[
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
    
    df <- rv$df[
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
    
    rv$df[
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
      rv$df[
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
      rv$df[
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
  
  territory_quantiles <- reactive({
    
    cat_col <- switch(
      location_type(rv$code),
      country = "DistrictID",
      district = "MunicipalityID",
      municipality = "ParishID",
      parish = NULL,
      shiny::validate(FALSE, "")
    )
    
    target_col <- input$target_col

    df <- rv$df[
      ,
      .(
        count = .N,
        min = quantile(get(target_col), probs = 0.05, na.rm = TRUE),
        low = quantile(get(target_col), probs = 0.25, na.rm = TRUE),
        mid = quantile(get(target_col), probs = 0.50, na.rm = TRUE),
        top = quantile(get(target_col), probs = 0.75, na.rm = TRUE),
        max = quantile(get(target_col), probs = 0.95, na.rm = TRUE)
      ),
      cat_col
    ]
    
    if(!is.null(cat_col)) {
      setnames(df, cat_col, "label")
      df <- df[!is.na(label)]
    }
    
    return(df)
  })
  
  observeEvent(input$zoom_out, {
    if(!is.empty(rv$code))
      rv$code <- stringr::str_sub(rv$code, end = -3)
  })
  
  output$territory_map <- renderLeaflet({
    leaflet(options = leafletOptions(
      attributionControl = FALSE,
      scrollWheelZoom = FALSE
    )) %>%
      addTiles() %>%
      addPolygons(
        data = district_sh,
        color = "#444444", weight = 1, smoothFactor = 0.5, label = NA, layerId = ~id,
        opacity = 1.0, 
        highlightOptions = highlightOptions(
          color = "white",
          weight = 2,
          bringToFront = TRUE)
      ) %>%
      addControl(
        actionButton("zoom_out", "Zoom Out", style = 'font-weight: bold; font-size: small'),
        position = "topright")
  })
  
  observeEvent(c(
      rv$df,
      input$target_col,
      input$sidebarmenu
    ), {
    
    need(input$sidebarmenu == "territoryTab", "")
    
    code <- rv$code
    loc_type <- location_type(code)
    is_parish <- loc_type == "parish"
    
    proxy <- leafletProxy("territory_map")
    
    df_territory <- territory_quantiles()
    
    map_sh <- switch(
      loc_type,
      country = district_sh,
      district = municipality_sh[municipality_sh$CCA_1 == code, ],
      municipality = parish_sh[parish_sh$CCA_2 == code, ],
      parish = parish_sh[parish_sh$CCA_3 == code, ]
    )
    
    map_sh$label <- df_territory[match(map_sh$id, df_territory$label), ]$mid
    
    fillOpacity <- ifelse(is_parish, 0.25, 0.75)
    weight <- ifelse(is_parish, 2, 1)
    
    proxy %>%
      clearShapes() %>%
      clearMarkers() %>%
      addPolygons(
        data = map_sh,
        smoothFactor = 0.5, opacity = 0,
        fillOpacity = fillOpacity, fillColor = ~qpal("Blues", label),
        group = "Polygons"
      ) %>%
      addPolygons(
        data = map_sh,
        color = "#444444", weight = weight, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 0,
        label = ~name, layerId = ~id,
        highlightOptions = highlightOptions(
          color = "white",
          weight = weight + 1,
          bringToFront = TRUE
        )
      )
    
    bb <- sf::st_bbox(map_sh)
    proxy %>% fitBounds(
      lat1 = bb[[2]],
      lng1 = bb[[1]],
      lat2 = bb[[4]],
      lng2 = bb[[3]])
    
    if(loc_type %in% c("municipality", "parish"))
    {
      pts <- rv$df[,  median(get(input$target_col), na.rm = TRUE), by = .(Latitude, Longitude)]
      pts <- pts[!is.na(V1)]
      
      if(nrow(pts) > SAMPLING_THRESHOLD)
        coord <- pts[sample(.N, SAMPLING_THRESHOLD)]
      
      radius <- ifelse(is_parish, 8, 4)
      
      proxy %>% addCircleMarkers(
        data = pts,
        radius = radius, 
        fillColor = ~qpal(c("green", "red"), V1),
        lng = ~Longitude, lat = ~Latitude,
        stroke = 0, fill = TRUE, fillOpacity = 1,
        group = "points"
      )
    }
  })

  # ----------------------------------------------------------------------------------------
  #                                     VALUATION SECTION
  # ----------------------------------------------------------------------------------------
  
  callModule(server_valuation, "mod_valuation")
  

  # ----------------------------------------------------------------------------------------
  #                                      SEARCH SECTION
  # ----------------------------------------------------------------------------------------
  
  callModule(server_search, "mod_search")
})

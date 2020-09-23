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
  
  observeEvent(input$map_shape_click$id, {
    rv$code <- input$map_shape_click$id
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
    rv$df %>% hc_hist("Net.Area", "m2", "", input$truncation)
  )
  
  # -------------------------------------- FORMATTABLE -------------------------------------
  
  output$tableQuantiles <- renderFormattable({
    probs <- c(0.95, 0.90, 0.75, 0.50, 0.25, 0.10, 0.05)
    table <- data.frame(
      quantile = percent(probs, 1),
      Price = currency(quantile(rv$df$Price, probs = probs), "", 0),
      Net.Area = currency(quantile(rv$df$Net.Area, probs = probs), "", 0),
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
        Net.Area = normalize_bar("lightpink", 0.2),
        price_m2 = normalize_bar("lightblue", 0.2)
      )
    )
  })


  # ----------------------------------------------------------------------------------------
  #                                   CATEGORIES SECTION
  # ----------------------------------------------------------------------------------------
  
  F_catBoxPlot <- function(cat_col, target_col = "price_m2") {
    
    target_name <- target_col
    target_suffix <- ""
    target_decimals <- 2
    switch(
      target_col,
      price_m2={
        target_name = "Price/m\u00b2"
        target_suffix = " \u20ac/m\u00b2"
      },
      Net.Area={
        target_suffix = " \u00b2"
      })
    
    df <- rv$df[
      !is.na(base::get(cat_col)) &
      !is.na(base::get(target_col))
    ]
    
    if (is.numeric(df[[cat_col]]))
    {
      df[[cat_col]] <- as.factor(df[[cat_col]])
    }
    
    if(median(df[[target_col]], na.rm=TRUE) >= 100)
      target_decimals <- 0
    
    df <- df[
      ,
      .(
        low    = quantile(base::get(target_col), probs = 0.10),
        q1     = quantile(base::get(target_col), probs = 0.25),
        median = quantile(base::get(target_col), probs = 0.50),
        q3     = quantile(base::get(target_col), probs = 0.75),
        high   = quantile(base::get(target_col), probs = 0.90)
      ),
      cat_col
    ][
      order(-rank(base::get(cat_col)))
    ]
    
    highchart() %>%
      hc_add_series(
        df, "boxplot",
        color="black", fillColor="skyblue", whiskerLength='25%',
        pointPadding=-0.2,
        name=target_name,
        hcaes(x=!!rlang::sym(cat_col))) %>%
      hc_tooltip(valueDecimals=target_decimals, valueSuffix=target_suffix) %>%
      hc_xAxis(type="category",
               title=list(
                 text=paste("<b>", cat_col, "</b>", sep=""),
                 useHTML=TRUE
               )
      ) %>%
      hc_yAxis(floor=0,
               title=list(
                 text=paste("<b>", target_name, "</b>", sep=""),
                 useHTML=TRUE
                )
      ) %>%
      hc_legend(enabled=FALSE) %>%
      hc_chart(inverted=TRUE) %>%
      hc_title(text=paste("<b><small>", target_name, "by", cat_col, "</small></b>", sep=" "),
               useHTML=TRUE)
  }
  
  F_catCount <- function(cat_col, target_col = "price_m2") {
    
    df <- rv$df[
      !is.na(base::get(cat_col)) &
      !is.na(base::get(target_col))
    ]
    
    if (is.numeric(df[[cat_col]]))
    {
      df[[cat_col]] <- as.factor(df[[cat_col]])
    }
    
    df <- df[
      ,
      .(count = .N),
      cat_col
    ][
      order(-rank(base::get(cat_col)))
    ]
    
    highchart() %>%
      hc_add_series(
        df, "column",
        color="skyblue", borderColor="black",
        pointPadding=-0.2,
        name="Frequency",
        hcaes(x=!!rlang::sym(cat_col), y=count)) %>%
      hc_xAxis(type="category") %>%
      hc_yAxis(min=0, title=list(text="<b>count</b>", useHTML=TRUE)) %>%
      hc_legend(enabled=FALSE) %>%
      hc_chart(inverted=TRUE) %>%
      hc_title(text=paste("<b><small>Frequency by", cat_col, "</small></b>", sep=" "),
               align="center", useHTML=TRUE)
  }
  
  F_catTable <- function(cat_col,  target_col = "price_m2") {
    
    rv$df[
      !is.na(base::get(cat_col)) & !is.na(base::get(target_col)),
      .(
        count = .N,
        "25%" = currency(quantile(base::get(target_col), probs=0.25), "", 2),
        median= currency(quantile(base::get(target_col), probs=0.50), "", 2),
        "75%" = currency(quantile(base::get(target_col), probs=0.75), "", 2)
      ),
      cat_col
    ][
      rev(order(base::get(cat_col)))
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
  
  output$EnergyCertificateBoxPlot <- renderHighchart(F_catBoxPlot("Energy.Certificate", input$target_col))
  output$EnergyCertificateCount <- renderHighchart(F_catCount("Energy.Certificate", input$target_col))
  output$EnergyCertificateTable <- renderFormattable(F_catTable("Energy.Certificate", input$target_col))
  
  output$ConditionBoxPlot <- renderHighchart(F_catBoxPlot("Condition", input$target_col))
  output$ConditionCount <- renderHighchart(F_catCount("Condition", input$target_col))
  output$ConditionTable <- renderFormattable(F_catTable("Condition", input$target_col))
  
  output$TipologyBoxPlot <- renderHighchart(F_catBoxPlot("Tipology", input$target_col))
  output$TipologyCount <- renderHighchart(F_catCount("Tipology", input$target_col))
  output$TipologyTable <- renderFormattable(F_catTable("Tipology", input$target_col))
  
  output$BathroomsBoxPlot <- renderHighchart(F_catBoxPlot("Bathrooms", input$target_col))
  output$BathroomsCount <- renderHighchart(F_catCount("Bathrooms", input$target_col))
  output$BathroomsTable <- renderFormattable(F_catTable("Bathrooms", input$target_col))
  
  output$ConstructionDecadeBoxPlot <- renderHighchart(F_catBoxPlot("Construction.Decade", input$target_col))
  output$ConstructionDecadeCount <- renderHighchart(F_catCount("Construction.Decade", input$target_col))
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
        levels <- c("Parish", "Point")
        selected <- "Parish"
      },
      parish = {
        levels <- c("Point")
        selected <- "Point"
      }
    )
    
    selectizeInput("agg_level", "Aggregation Level",
                   levels, selected = selected)
  })
  
  output$corr_agg_prop_input <- renderUI({
    agg_col <- input$agg_level
    shiny::req(!is.empty(agg_col))
    
    if(agg_col != "Point") {
      checkboxInput("agg_prop_type", "Aggregate different property types")
    }
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
    
    shiny::req(!is.empty(agg_col))
    
    if(agg_col == "Point")
    {
      js <- "
      function () {
        return '<span style=\"color:' + this.series.color + '\">\u25CF</span> ' + this.series.name + '<br><br>' + 
               '<strong>' + '%s:</strong> ' + this.point.x + '<br>' + 
               '<strong>' + '%s:</strong> ' + this.point.y; }"
      
      df <- rv$df[
        ,
        list(
          X = base::get(target2),
          Y = base::get(target1),
          Property.Type = Property.Type
        ),
      ]
      
      hc_args <- list(x = quote(X), y = quote(Y), group = quote(Property.Type))
      js <- sprintf(js, target2, target1)
    }
    else if(input$agg_prop_type)
    {
      js <- "
      function () {
        return '  <strong>' + this.point.%s + '</strong><br>' + 
                 '<strong>' + '%s:</strong> ' + this.point.x + '<br>' + 
               '<strong>' + '%s:</strong> ' + this.point.y; }"
      
      df  <- rv$df[
        ,
        list(
          count = .N,
          X = median(base::get(target2), na.rm = TRUE),
          Y = median(base::get(target1), na.rm = TRUE)
        ),
        agg_col
      ][
        count >= MIN_DATAPOINTS
      ][
        !is.na(base::get(agg_col))
      ]
      
      hc_args <- list(x = quote(X), y = quote(Y))
      js <- sprintf(js, agg_col, target2, target1)
    }
    else
    {
      js <- "
      function () {
        return '  <strong>' + this.point.%s + '</strong>' +
               '<span style=\"color:' + this.series.color + '\">\u25CF</span> ' + this.series.name + '<br><br>' + 
               '<strong>' + '%s:</strong> ' + this.point.x + '<br>' + 
               '<strong>' + '%s:</strong> ' + this.point.y; }"
      
      df <- rv$df[
        ,
        list(
          count = .N,
          X = median(base::get(target2), na.rm = TRUE),
          Y = median(base::get(target1), na.rm = TRUE)
        ),
        c(agg_col, "Property.Type")
      ][
        count >= MIN_DATAPOINTS
      ][
        !is.na(base::get(agg_col))
      ]
      
      hc_args <- list(x = quote(X), y = quote(Y), group = quote(Property.Type))
      js <- sprintf(js, agg_col, target2, target1)
    }
    
    # filter nans in X and Y
    probs <- c(0.25, 0.50, 0.75)
    Q_X <- quantile(df$X, probs = probs, na.rm = TRUE)
    Q_Y <- quantile(df$Y, probs = probs, na.rm = TRUE)
    N_SIGMA <- 5 / 1.349
    
    df <- df[
      !is.na(X) &&
      !is.na(Y) &&
      (X > Q_X[[2]] - (Q_X[[3]] - Q_X[[1]]) * N_SIGMA) &&
      (X > Q_X[[2]] - (Q_X[[3]] - Q_X[[1]]) * N_SIGMA) &&
      (Y > Q_Y[[2]] - (Q_Y[[3]] - Q_Y[[1]]) * N_SIGMA) &&
      (Y < Q_Y[[2]] + (Q_Y[[3]] - Q_Y[[1]]) * N_SIGMA)
    ]
    
    MAX_POINTS <- 500
    if(nrow(df) > MAX_POINTS)
      df <- df[sample(.N, MAX_POINTS)]
    
    
    df %>%
      hchart("scatter", do.call(hcaes, hc_args)) %>%
      hc_xAxis(title = list(text = target2)) %>%
      hc_yAxis(title = list(text = target1)) %>%
      hc_tooltip(formatter=JS(js))

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
      shiny::req(FALSE)
    )
    
    target_col <- input$target_col

    df <- rv$df[
      ,
      .(
        count = .N,
        min = quantile(base::get(target_col), probs = 0.05, na.rm = TRUE),
        low = quantile(base::get(target_col), probs = 0.25, na.rm = TRUE),
        mid = quantile(base::get(target_col), probs = 0.50, na.rm = TRUE),
        top = quantile(base::get(target_col), probs = 0.75, na.rm = TRUE),
        max = quantile(base::get(target_col), probs = 0.95, na.rm = TRUE)
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
  
  observeEvent(input$map_draw_new_feature, {
    # print("New Feature")
    # print(input$map_draw_new_feature)
    pts <- rv$df %>% sf::st_as_sf(coords = c('Longitude', 'Latitude'))
    coords <- input$map_draw_new_feature$geometry$coordinates[[1]]
    p <- sf::st_polygon(list(t(sapply(coords, function(x) sapply(x, cbind)))))
    filt <- p %>% sf::st_contains(pts)
    target_col <- input$target_col
    
    stats <- rv$df[
      filt[[1]],
      .(
        min = quantile(base::get(target_col), probs = 0.05, na.rm = TRUE),
        low = quantile(base::get(target_col), probs = 0.25, na.rm = TRUE),
        mid = quantile(base::get(target_col), probs = 0.50, na.rm = TRUE),
        top = quantile(base::get(target_col), probs = 0.75, na.rm = TRUE),
        max = quantile(base::get(target_col), probs = 0.95, na.rm = TRUE)
      ),
      ]
    
    css <- "
      #draw {
        display: block;
        border: 1px solid black;
        border-radius: 0px;
        opacity: 1.0;
        font-size: x-small;
      }
      
      #draw strong {
        display: inline-block;
        width: 80px;
        text-align: right;
        margin: 0px 6px 0px 0px;
      }
    "
    
    target <- input$target_col
    
    leafletProxy("map") %>%
      addControl(
        tags$div(
          tags$style(type = "text/css", css),
          tags$div(tags$strong("Lower Quartile"), format_value(target, stats$low)),
          tags$div(tags$strong("Median"),         format_value(target, stats$mid)),
          tags$div(tags$strong("Upper Quartile"), format_value(target, stats$top)),
        ),
        layerId = "draw",
        position = "topright"
      )
  })
  
  output$map <- renderLeaflet({
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
        layerId = "rleaflet_control",
        actionButton("zoom_out", "Zoom Out", style = "
          font-weight: bold;
          font-size: small;
          border-radius: 0px;
          border: 1px solid black;"
        ),
        position = "topright")
  })
  
  observeEvent(c(
      rv$df,
      input$target_col,
      input$sidebarmenu
    ), {
    
    need(input$sidebarmenu == "territoryTab", "")
    
    code <- rv$code
    target <- input$target_col
    loc_type <- location_type(code)
    is_parish <- loc_type == "parish"
    
    proxy <- leafletProxy("map")
    
    df_territory <- territory_quantiles()
    
    map_sh <- switch(
      loc_type,
      country = district_sh,
      district = municipality_sh[municipality_sh$CCA_1 == code, ],
      municipality = parish_sh[parish_sh$CCA_2 == code, ],
      parish = parish_sh[parish_sh$CCA_3 == code, ]
    )
    
    map_sh$value <- df_territory[match(map_sh$id, df_territory$label), ]$mid
    
    fillOpacity <- ifelse(is_parish, 0.25, 0.75)
    
    proxy %>% clearShapes()
    proxy %>% clearMarkers()
    proxy %>%
      addPolygons(
        data = map_sh,
        group = "Polygons",
        smoothFactor = 0.5, opacity = 0,
        fillOpacity = fillOpacity,
        fillColor = ~qpal("Blues", value)
      )
    
    if(is_parish) {
      proxy %>% addPolygons(
        data = map_sh, layerId = ~id,
        color = "#444444", weight = 3, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 0
      )
    }
    else {
      proxy %>% addPolygons(
        data = map_sh, layerId = ~id,
        color = "#444444", weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 0,
        label = ~map2(name, value, function(name, value) htmltools::HTML(
          paste(sep = "", "<b>", name, "</b><br/>", format_value(target, value))
        )),
        labelOptions = labelOptions(),
        highlightOptions = highlightOptions(
          color = "white", weight = 2,
          bringToFront = TRUE
        )
      )
    }
    
    proxy %>% removeControl(layerId = 'draw')
    proxy %>% removeDrawToolbar(clearFeatures = TRUE)
    
    bb <- sf::st_bbox(map_sh)
    proxy %>% fitBounds(
      lat1 = bb[[2]],
      lng1 = bb[[1]],
      lat2 = bb[[4]],
      lng2 = bb[[3]])
    
    if(loc_type %in% c("municipality", "parish"))
    {
      pts <- rv$df[,  median(base::get(target), na.rm = TRUE), by = .(Latitude, Longitude)]
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
      
      proxy %>%
        addDrawToolbar(
          polylineOptions = FALSE,
          circleOptions = FALSE,
          rectangleOptions = FALSE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          # editOptions = editToolbarOptions(),
          singleFeature = TRUE
        )
    }
  })
  
  
  output$territory_boxplot <- renderPlot({
    
    median_val <- median(rv$df[[input$target_col]])  # must be converted to reactive value
    
    territory_quantiles() %>%
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
    
    territory_quantiles() %>%
      select(count, low, mid, top) %>%
      rename("25%" = low, "median" = mid, "75%" = top) %>%
      formattable(
        align = c("l", "r", "r", "r", "r"),
        list(
          area(col = 'label') ~ formatter(
            "span", style = ~formattable::style(color = "grey", font.weight = "bold")),
          count = normalize_bar("pink", 0.2),
          median = normalize_bar("lightblue", 0.2)
        )
      )
  })

  # ----------------------------------------------------------------------------------------
  #                                     APPRAISAL SECTION
  # ----------------------------------------------------------------------------------------
  
  callModule(server_appraisal, "mod_appraisal")
  

  # ----------------------------------------------------------------------------------------
  #                                      SEARCH SECTION
  # ----------------------------------------------------------------------------------------
  
  callModule(server_search, "mod_search")
})

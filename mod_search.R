
ui_search <- function(id)
{
  ns <- NS(id)
  tabItem(
    tabName = ns("investmentSearchTab"),
    #tags$h2(tags$strong('Investment Search')),
    fluidRow(
      box(
        title = tags$strong("Inputs"),
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = FALSE,
        br(),
        fluidRow(
          column(4,
                 numericInput(ns("max_investment"), "Budget",
                              200000, min=0),
                 selectizeInput(ns("objective"), "Objective",
                                c("Buy", "Rent", "Buy-to-Let")),
                 selectizeInput(ns("prop_types"), "Property Types",
                                choices = setNames(prop_types_ids, prop_types),
                                multiple = TRUE,
                                selected = c(1, 2))
          ),
          column(4,
                 selectizeInput(ns("condition"), "Condition",
                                choices = condition_codes,
                                multiple = TRUE,
                                selected = NULL),
                 numericInput(ns("maxArea"), "Maximum Area", 150, min=0)
          ),
          column(4,
                 htmlOutput(ns("geomenu")),
                 actionButton(ns("location_button"),
                              "Select location",
                              icon=icon("globe")),
                 bsModal(ns("location_modal"), "Property Location",
                         ns("location_button"),
                         leafletOutput(ns("map")),
                         size = "large")
          ),
        ),
        fluidRow(
          column(4,
                 actionButton(
                   ns("search"),
                   "Search",
                   icon = icon("search"),
                   style = "color: #ffffff; background-color: #474949"
                 ),
                 offset = 1
          )
        ),
        width = 12
      )
    ),
    fluidRow(
      box(
        DT::dataTableOutput(ns("searchOutput")) %>% withSpinner(type=SPINNER_TYPE),
        width = 12
      )
    )
  )
}

server_search  <- function(input, output, session) {
  
  rv <- reactiveValues(
    code = NULL,
    queryOutput = data.frame(
      'Url' = NULL,
      'Agency' = NULL,
      'Title' = NULL,
      'Net Area' = NULL,
      'Price' = NULL,
      'Predicted Price' = NULL
    )
  )

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
  
  output$geomenu <- renderUI(
    switch(
      location_type(rv$code),
      country = {
        tags$div(
          selectizeInput(session$ns("district"), "Location", district_list,
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
          selectizeInput(session$ns("district"), "Location", district_list,
                         selected = district_code
          ),
          selectizeInput(session$ns("municipality"), NULL, municipality_list,
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
          selectizeInput(session$ns("district"), "Location", district_list,
                         selected = district_code
          ),
          selectizeInput(session$ns("municipality"), NULL, municipality_list,
                         selected = municipality_code
          ),
          selectizeInput(session$ns("parish"), NULL, parish_list,
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
          selectizeInput(session$ns("district"), "Location", district_list,
                         selected = district_code
          ),
          selectizeInput(session$ns("municipality"), NULL, municipality_list,
                         selected = municipality_code
          ),
          selectizeInput(session$ns("parish"), NULL, parish_list,
                         selected = parish_code
          )
        )
      }
    )
  )
  
  observeEvent(input$zoom_out, {
    if(!is.empty(rv$code))
      rv$code <- stringr::str_sub(rv$code, end = -3)
  })
  
  # map of the parish to select the coordinates
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(
      attributionControl = FALSE,
      scrollWheelZoom = FALSE
    )) %>%
      addTiles() %>%
      addPolygons(
        data = district_sh, layerId = ~id,
        label = ~name,
        fillColor = "orange", fillOpacity = 0.75,
        color = "#444444", opacity = 1,
        weight = 1, smoothFactor = 0.5,
        highlightOptions = highlightOptions(
          color = "white",
          weight = 2,
          bringToFront = TRUE)
      ) %>%
      addControl(
        layerId = "rleaflet_control",
        actionButton(session$ns("zoom_out"), "Zoom Out", style = "
          font-weight: bold;
          font-size: small;
          border-radius: 0px;
          border: 1px solid black;"
        ),
        position = "topright"
      )
  })
  
  observeEvent(c(
    rv$code,
    input$location_button
  ), {
    code <- rv$code
    loc_type <- location_type(code)
    proxy <- leafletProxy("map")
    
    map_sh <- switch(
      loc_type,
      country = district_sh,
      district = municipality_sh[municipality_sh$CCA_1 == code, ],
      municipality = parish_sh[parish_sh$CCA_2 == code, ],
      parish = parish_sh[parish_sh$CCA_3 == code, ]
    )
    
    proxy %>% clearShapes()
    proxy %>% clearMarkers()
    proxy %>%
      addPolygons(
        data = map_sh, group = "Polygons",
        smoothFactor = 0.5, opacity = 0,
        fillOpacity = 0.75, fillColor = "orange"
      )
    
    proxy %>% addPolygons(
      data = map_sh, layerId = ~id, label = ~name,
      color = "#444444", weight = 1, smoothFactor = 0.5,
      opacity = 1.0, fillOpacity = 0,
      highlightOptions = highlightOptions(
        color = "white", weight = 2,
        bringToFront = TRUE
      )
    )
    
    bb <- sf::st_bbox(map_sh)
    proxy %>% fitBounds(
      lat1 = bb[[2]],
      lng1 = bb[[1]],
      lat2 = bb[[4]],
      lng2 = bb[[3]])
  })
  
  
  searchResult <- eventReactive(input$search, {
    
    rank_query <- ''
    filter_query <- ''
    if(input$objective == "Buy")
    {
      rank_query <- '(LOG10(%f / `Net Area`) - LOG10(`pred_Q1-price_m2`))'
      pred_query <- '`pred-price_m2` * `Net Area`'
      target_var <- 'Price'
      roi_query  <- 'ROUND(`pred-price_m2` * `Net Area` / Price - 1, 4) * 100
                     AS ROI'
      filt_query <- 'Deal = 1'
    }
    else if(input$objective == "Rent")
    {
      rank_query <- '(LOG10(%f / `Net Area`) - LOG10(`pred_Q1-price_m2`))'
      pred_query <- '`pred-rent_m2` * `Net Area`'
      target_var <- 'Rent' 
      roi_query  <- 'ROUND(Price / (`pred-rent_m2` * `Net Area`) - 1, 4) * 100
                     AS Discount'
      filt_query <- 'Deal = 0'
    }
    else if(input$objective == "Buy-to-Let")
    {
      rank_query <- '(LOG10(%f / `Net Area`) - LOG10(`pred_Q1-rent_m2`))'
      pred_query <- '`pred-rent_m2` * `Net Area`'
      target_var <- 'Rent' 
      roi_query <- 'ROUND(12 * `pred-rent_m2` * `Net Area` / Price, 4) * 100
                    AS Yield'
      filt_query <- 'Deal = 1'
    }
    else
    {
      stop("Invalid Field")
    }
    
    rank_query <- sprintf(rank_query, input$max_investment)
    
    filt_query <- c(
      filt_query,
      paste('Price <', as.character(input$max_investment))
    )
    
    if(length(input$prop_types) > 0)
    {
      if(("1" %in% input$prop_types) & !("2" %in% input$prop_types))
      {
        filt_query <- c(filt_query, "Apartment = 1")
      }
      else if(!("1" %in% input$prop_types) & ("2" %in% input$prop_types))
      {
        filt_query <- c(filt_query, "Apartment = 0")
      }
    }
    
    if(length(input$condition) > 0)
    {
      filt_query <- c(
        filt_query,
        paste("`Condition` IN (",
              paste("'", input$condition, "'", sep="", collapse=","),
              ")", sep=' ')
      )
    }
    
    if(!is.empty(input$maxArea))
    {
      filt_query <- c(filt_query, sprintf("`Net Area` <= %s", input$maxArea))
    }
    
    if(!is.empty(input$district))
    {
      filt_query <- c(filt_query, sprintf("DistrictID = %s", input$district))
    }
    
    if(!is.empty(input$municipality))
    {
      filt_query <- c(filt_query, sprintf("MunicipalityID = %s", input$municipality))
    }
    
    if(!is.empty(input$parish))
    {
      filt_query <- c(filt_query, sprintf("ParishID = %s", input$parish))
    }
    
    filt_query <- paste(filt_query, collapse = " AND ")
    
    query <- sprintf(
      "SELECT
          url AS Link,
          title AS Title,
          `Net Area`,
          Price,
          ROUND(%s, 0) AS `Predicted %s`,
          %s
       FROM imovirtual_residential
       WHERE %s
       ORDER BY %s
       LIMIT 100", pred_query, target_var, roi_query, filt_query, rank_query)
    
    print(query)
    
    conn <- dbConnect(MySQL(), user=DB_USER, password=DB_PWD, host=DB_HOST, dbname=DB_NAME, port=DB_PORT)
    df <- dbGetQuery(conn, query)
    dbDisconnect(conn)
    
    if(length(df$Link) > 0)
      df$Link <- paste0("<a href='", df$Link, "' target='_blank'>link</a>")
    
    return(df)
  })
  
  
  output$searchOutput <- DT::renderDataTable(
    searchResult(),
    escape = FALSE,
    filter = 'top',
    options = list(scrollX = TRUE))
  
  

}

APPRAISAL_URL <- "#{APPRAISAL_URL}"


ui_appraisal <- function(id)
{
  ns <- NS(id)
  tabItem(
    tabName = ns("appraisalTab"),
    fluidRow(
      box(
        title = tags$strong("Inputs"),
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = FALSE,
        fluidRow(
          column(4,
                 selectizeInput(ns("prop_type"), "Property Type",
                                setNames(prop_types_ids, prop_types), selected = "Apartment"),
                 radioButtons(ns("deal"), NULL,
                              choices = setNames(c(1, 0), c("Sale", "Rent")),
                              inline = TRUE),
                 selectInput(ns("energy_certificate"),
                             "Energy Certificate", NULL,
                             choices = energy_certificate_levels),
                 selectInput(ns("condition"),
                             "Condition",
                             selected = "usado",
                             choices = setNames(condition_codes, condition_levels)),
                 numericInput(ns("construction_year"),
                              "Construction Year", NULL, min=0, max=2020)
          ),
          column(4,
                 numericInput(ns("net_area"),     "Net Area",      100, min=0, step=1),
                 numericInput(ns("gross_area"),   "Gross Area",   NULL, min=0, step=1),
                 numericInput(ns("terrain_area"), "Terrain Area", NULL, min=0, step=1),
                 numericInput(ns("bedrooms"),     "#Bedrooms",    NULL, min=0, max=10, step=1),
                 numericInput(ns("bathrooms"),    "#Bathrooms",   NULL, min=0, max=4, step=1)
          ),
          column(4,
                 selectizeInput(ns("attrs"), "Other Attributes",
                                other_attrs, multiple = TRUE
                 ),
                 htmlOutput(ns("geomenu")),
                 actionButton(ns("location_button"),
                              "Select location",
                              icon=icon("globe")),
                 bsModal(ns("location_modal"), "Property Location",
                         ns("location_button"),
                         leafletOutput(ns("map")),
                         size = "large")
          )
        ),
        fluidRow(
          column(12,
                 actionButton(
                   ns("calculate"),
                   "Simulate",
                   icon = icon("random"),
                   style = "color: #ffffff; background-color: #474949"
                 )
          )
        ),
        width = 12
      )
    ),
    fluidRow(htmlOutput(ns("appraisalOutput")))
  )
}

server_appraisal <- function(input, output, session) {
  
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
  
  observeEvent(rv$code, {
    
    code <- rv$code
    loc_type <- location_type(code)
    is_parish <- loc_type == "parish"
    proxy <- leafletProxy("map")
    
    map_sh <- switch(
      loc_type,
      country = district_sh,
      district = municipality_sh[municipality_sh$CCA_1 == code, ],
      municipality = parish_sh[parish_sh$CCA_2 == code, ],
      parish = parish_sh[parish_sh$CCA_3 == code, ]
    )
    
    fillOpacity <- ifelse(is_parish, 0.25, 0.75)
    weight <- ifelse(is_parish, 3, 2)

    proxy %>% clearShapes()
    proxy %>% clearMarkers()
    proxy %>%
      addPolygons(
        data = map_sh, group = "Polygons",
        smoothFactor = 0.5, opacity = 0,
        fillOpacity = fillOpacity, fillColor = "orange"
      )
    
    proxy %>% addPolygons(
      data = map_sh, layerId = ~id,
      color = "#444444", weight = 1, smoothFactor = 0.5,
      opacity = 1.0, fillOpacity = 0,
      label = ~htmltools::HTML(paste(sep="", "<b>", name, "</b>")),
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
  
  
  # performs the actual appraisal
  appraisalResult <- eventReactive(input$calculate, {
    shiny::validate(
      need(!is.empty(input$district),     label = "District"),
      need(!is.empty(input$municipality), label = "Municipality"),
      need(!is.empty(input$net_area),     label = "Area")
    )
    
    body <- list(
      Deal = input$deal,
      `Property Type` = input$prop_type,
      
      DistrictID = input$district,
      MunicipalityID = input$municipality,
      ParishID = input$parish,
      
      Latitude = NA,
      Longitude = NA,
      
      Bedrooms = input$bedrooms,
      Bathrooms = input$bathrooms,
      Condition = input$condition,
      Area = input$net_area,
      `Gross Area` = input$gross_area,
      `Terrain Area` = input$terrain_area,
      
      `Energy Certificate` = input$energy_certificate,
      `Construction Year` = input$construction_year
    )
    
    for(c in unlist(other_attrs, use.names = FALSE))
    {
      body[[c]] <- 1.0 * (c %in% input$attrs)
    }
    
    body <- jsonlite::toJSON(list(body), auto_unbox = TRUE)
    res <- httr::POST(APPRAISAL_URL, body=body, encode="json")
    
    print("Request:")
    print(body)
    
    print("Response:")
    print(content(res))

    appraisal <- list()
    appraisal$price_m2 <- jsonlite::fromJSON(content(res)$data)$`0`$`pred-price_m2`
    appraisal$price <- appraisal$price_m2 * input$net_area
    
    return(appraisal)
  }, ignoreInit = TRUE)
  
  
  # renderUI
  output$appraisalOutput <- renderUI({
    res <- appraisalResult()
    
    box(
      title = tags$h2("Simulation Results"),
      #status = "primary",
      solidHeader = TRUE,
      #collapsible = TRUE,
      #collapsed = TRUE,
      fluidRow(
        box(
          fluidRow(
            valueBox(
              paste(round(res$price, 2), "\u20ac"),
              "Predicted price",
              color = "green",
              icon = icon("euro-sign")
            ),
            valueBox(
              paste(round(res$price_m2, 2), "\u20ac"),
              "Predicted price / m2",
              color = "light-blue",
              icon = icon("home")
            )
          ),
          solidHeader = FALSE,
          width = 12
        )
      ),
      width = 12
    )
  })
}
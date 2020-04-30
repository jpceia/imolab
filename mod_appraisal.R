
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
                 selectizeInput(ns("district"), "Location", district_list,#,  district
                                size = 3,
                                options = list(
                                  placeholder = 'District',
                                  onInitialize = I('function() { this.setValue(""); }')
                                )),
                 selectizeInput(ns("municipality"), NULL, c(" "),
                                options = list(
                                  placeholder = 'Municipality',
                                  onInitialize = I('function() { this.setValue(""); }')
                                )),
                 selectizeInput(ns("parish"), NULL, c(" "),
                                multiple = FALSE,
                                options = list(
                                  placeholder = 'Parish',
                                  onInitialize = I('function() { this.setValue(""); }')
                                )),
                 actionButton(ns("coordinates_button"),
                              "Select coordinates",
                              icon=icon("globe")),
                 
                 bsModal(ns("coordinates_map_modal"), "Property Location",
                         ns("coordinates_button"),
                         leafletOutput(ns("coordinates_map")),
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
  
  # updates the municipality list, after selecting a new district
  observeEvent(input$district, {
    municipality_list <- c(NULL)
    df <- municipality_sh %>% filter(CCA_1 == input$district)
    
    if (nrow(df) > 0) {
      municipality_list <- df$CCA_2
      names(municipality_list) <- df$name
    }
    
    updateSelectInput(session, "municipality", choices = municipality_list)
    updateSelectInput(session, "parish", choices = c(NULL), selected = "")
  })
  
  
  # updates the parish list, after selecting a new municipality
  observeEvent(input$municipality, {
    parish_list <- c(NULL)
    df <- parish_sh %>% filter(CCA_2 == input$municipality)
    
    if (nrow(df) > 0) {
      parish_list <- df$CCA_3
      names(parish_list) <- df$name
    }

    updateSelectInput(session, "parish", choices = parish_list)
  })
  
  
  # map of the parish to select the coordinates
  output$coordinates_map <- renderLeaflet({
    
    code <- input$parish
    shiny::validate(need(!is.empty(code), label = "Parish"))
    parish_sh %>%
      filter(CCA_3 == code) %>%
      leaflet(options = leafletOptions(
        zoomControl = TRUE,
        attributionControl = FALSE
        # dragging = FALSE
      )) %>%
      addTiles() %>%
      addPolygons(
        color = "#444444", weight = 1, smoothFactor = 0.5,
        opacity = 1.0,
        fillOpacity = 0.15,
        fillColor = "cornflowerblue"
      )
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
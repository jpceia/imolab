
ui_valuation <- function(id)
{
  ns <- NS(id)
  tabItem(
    tabName = ns("valuationTab"),
    tags$h2(tags$strong('Valuation')),
    fluidRow(
      box(
        title = tags$strong("Inputs"),
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = FALSE,
        fluidRow(
          column(4,
                 selectizeInput(ns("prop_type"), NULL,
                                prop_types, selected = "Apartment"),
                 radioButtons(ns("deal_type"), NULL,
                              choices = c("Sale", "Rent"),
                              inline = TRUE),
                 selectInput(ns("energy_certificate"),
                             "Energy Certificate", NULL,
                             choices = energy_certificate_levels),
                 selectInput(ns("condition"),
                             "Condition",
                             selected = "Usado",
                             choices = condition_levels),
                 numericInput(ns("construction_year"),
                              "Construction Year", NULL, min=0, max=2020)
          ),
          column(4,
                 numericInput(ns("net_area"),     "Net Area",      100, min=0, step=1),
                 numericInput(ns("gross_area"),   "Gross Area",   NULL, min=0, step=1),
                 numericInput(ns("terrain_area"), "Terrain Area", NULL, min=0, step=1),
                 numericInput(ns("rooms"),        "#Rooms",       NULL, min=0, max=10, step=1),
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
                 selectizeInput(ns("city"), NULL, c(" "),
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
    fluidRow(htmlOutput(ns("valuationOutput")))
  )
}

server_valuation <- function(input, output, session) {
  
  # updates the city list, after selecting a new district
  observe({
    city_list <- c(NULL)
    df <- municipality_sh %>% filter(CCA_1 == input$district)
    
    if (nrow(df) > 0) {
      city_list <- df$id
      names(city_list) <- df$name
    }
    
    updateSelectInput(session, "city", choices = city_list)
    updateSelectInput(session, "parish", choices = c(" "))
  })
  
  
  # updates the parish list, after selecting a new city
  observe({
    parish_list <- c(NULL)
    df <- parish_sh %>% filter(CCA_2 == input$city)
    
    if (nrow(df) > 0) {
      parish_list <- df$id
      names(parish_list) <- df$name
    }
    
    updateSelectInput(session, "parish", choices = parish_list)
  })
  
  
  # map of the parish to select the coordinates
  output$coordinates_map <- renderLeaflet({
    
    parish_code <- input$parish
    validate(need(!is.empty(parish_code), label = "Parish"))
    parish_sh %>%
      filter(CCA_3 == parish_code) %>%
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
  
  
  # performs the actual valuation
  valuationResult <- eventReactive(input$calculate, {
    validate(
      need(!is.empty(input$district), label = "District"),
      need(!is.empty(input$city), label = "Municipality"),
      need(!is.empty(input$net_area), label = "Area")
    )
    
    row <- list(
      DealType = input$deal_type,
      PropType = input$prop_type,
      
      district_code = input$district,
      city_code = input$city,
      parish_code = input$parish,
      
      construction_year = input$construction_year,
      energy_certificate = input$energy_certificate,
      condition = input$condition,
      rooms = input$rooms,
      bathrooms = input$bathrooms,
      
      area = input$net_area,
      gross_area = input$gross_area,
      terrain_area = input$terrain_area,
      
      latitude = NA,
      longitude = NA
    )
    
    for(c in unlist(other_attrs, use.names = FALSE))
    {
      row[[c]] <- 1.0 * (c %in% input$attrs)
    }
    
    row <- data.frame(
      lapply(row, function(x) ifelse(is.null(x), NA, x)),
      check.names = FALSE) %>%
      tbl_df() %>%
      mutate(
        area = as.double(area),
        gross_area = as.double(gross_area),
        terrain_area = as.double(terrain_area)
      )
    
    ### DROPDOWN
    
    drop_cols <- c(
      input$attrs,
      "bathrooms",
      "rooms",
      "construction_year",
      "energy_certificate",
      "condition",
      "terrain_area",
      "gross_area",
      "area",
      "parish_code",
      "city_code",
      "district_code"
    )
    
    df <- row
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
    
    pred_price <- exp(predict(reg$price, xgb.DMatrix(data = as.matrix(X))))
    
    res <- list()
    
    res$breakdown <- data.frame(
      Characteristic = drop_cols_final,
      #price_m2 = currency(pred_price_m2, "", 0),
      price = currency(pred_price, "", 0),
      impact = accounting(pred_price - lag(pred_price)),
      row.names = NULL
    )
    
    res$price <- res$breakdown[nrow(res$breakdown), "price"]
    res$price_m2 <- res$price / input$net_area
    
    return(res)
  }, ignoreInit = TRUE)
  
  
  # formats the impact table
  output$valuationResultTable <- renderFormattable({
    valuationResult()$breakdown %>%
      formattable(
        align = c("l", "r", "r", "r"),
        list(
          area(col = "Characteristic") ~ formatter(
            "span", style = ~formattable::style(color = "grey", font.weight = "bold")),
          price = normalize_bar("pink", 0.2),
          impact = formatter(
            "span", style = x ~ formattable::style(color = ifelse(x < 0, "red", "green")))
        )
      )
  })
  
  # renderUI
  output$valuationOutput <- renderUI({
    res <- valuationResult()
    
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
          tags$h3("Valuation breakdown"),
          formattableOutput(session$ns("valuationResultTable")),
          solidHeader = FALSE,
          width = 12
        )
      ),
      width = 12
    )
  })
}
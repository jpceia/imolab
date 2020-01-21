
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
                 selectizeInput(ns("prop_type"), "Property Type",
                                prop_types, selected = "Apartment"),
                 radioButtons(ns("deal"), NULL,
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
    fluidRow(htmlOutput(ns("valuationOutput")))
  )
}

server_valuation <- function(input, output, session) {
  
  # updates the municipality list, after selecting a new district
  observeEvent(input$district, {
    municipality_list <- c(NULL)
    
    df <- municipality_sh %>% filter(CCA_1 == input$district)
    municipality_list <- df$id
    names(municipality_list) <- df$name
    
    updateSelectInput(session, "municipality", choices = municipality_list)
    updateSelectInput(session, "parish",       choices = c(NULL))
  })
  
  
  # updates the parish list, after selecting a new municipality
  observeEvent(input$municipality, {
    parish_list <- c(NULL)
    
    df <- parish_sh %>% filter(CCA_2 == input$municipality)
    parish_list <- df$id
    names(parish_list) <- df$name
    updateSelectInput(session, "parish", choices = parish_list)
  })
  
  
  # map of the parish to select the coordinates
  output$coordinates_map <- renderLeaflet({
    
    code <- input$parish
    validate(need(!is.empty(code), label = "Parish"))
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
  
  
  # performs the actual valuation
  valuationResult <- eventReactive(input$calculate, {
    validate(
      need(!is.empty(input$district),     label = "District"),
      need(!is.empty(input$municipality), label = "Municipality"),
      need(!is.empty(input$net_area),     label = "Area")
    )
    
    row <- list(
      Deal = input$deal,
      `Property Type` = input$prop_type,
      
      DistrictID = input$district,
      MunicipalityID = input$municipality,
      ParishID = input$parish,
      
      `Construction Year` = input$construction_year,
      `Energy Certificate` = input$energy_certificate,
      Condition = input$condition,
      Bedrooms = input$bedrooms,
      Bathrooms = input$bathrooms,
      
      Area = input$net_area,
      `Gross Area` = input$gross_area,
      `Terrain Area` = input$terrain_area,
      
      Latitude = NA,
      Longitude = NA
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
        Area = as.double(Area),
        `Gross Area` = as.double(`Gross Area`),
        `Terrain Area` = as.double(`Terrain Area`)
      )
    
    ### DROPDOWN
    
    drop_cols <- c(
      input$attrs,
      "Bathrooms",
      "Bedrooms",
      "Construction Year",
      "Energy Certificate",
      "Condition",
      "Terrain Area",
      "Gross Area",
      "Area",
      "ParishID",
      "MunicipalityID",
      "DistrictID"
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
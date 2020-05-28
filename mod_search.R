
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
                              100000, min=0),
                 selectizeInput(ns("objective"), "Objective",
                                c("Buy", "Rent", "Buy-to-Let")),
                 selectizeInput(ns("prop_types"), "Property Types",
                                choices = setNames(prop_types_ids, prop_types),
                                multiple = TRUE,
                                selected = c(1, 2)),
                 offset = 1
          ),
          column(4,
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
                 offset = 1
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
  
  rv <- reactiveValues(
    queryOutput = data.frame(
      'Url' = NULL,
      'Agency' = NULL,
      'Title' = NULL,
      'Area' = NULL,
      'Price' = NULL,
      'Predicted Price' = NULL
    )
  )
  
  searchResult <- eventReactive(input$search, {
    
    rank_query <- ''
    filter_query <- ''
    if(input$objective == "Buy")
    {
      rank_query <- '(LOG10(Price / Area) - LOG10(`pred-price_m2`)) / `pred-std_price_m2`'
      pred_query <- '`pred-price_m2` * Area'
      filt_query <- 'Deal = 1'
    }
    else if(input$objective == "Rent")
    {
      rank_query <- '(LOG10(Price / Area) - LOG10(`pred-price_m2`)) / `pred-std_price_m2`'
      pred_query <- '`pred-price_m2` * Area'
      filt_query <- 'Deal = 0'
    }
    else if(input$objective == "Buy-to-Let")
    {
      rank_query <- '(LOG10(Price / Area) - LOG10(`pred-rent_m2`)) / `pred-std_rent_m2`'
      pred_query <- '`pred-rent_m2` * Area'
      filt_query <- 'Deal = 1'
    }
    else
    {
      stop("Invalid Field")
    }
    
    filt_query <- c(
      filt_query,
      paste('Price <', as.character(input$max_investment))
    )
    
    if(length(input$prop_types) > 0)
    {
      filt_query <- c(
        filt_query,
        sprintf("`Property Type` IN (%s)", paste(input$prop_types, collapse=", "))
      )
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
          Url AS Link,
          Agency,
          Title,
          Area,
          Price,
          ROUND(%s, 0) AS `Predicted Price`
       FROM VIEW_daily
       WHERE %s
       ORDER BY %s
       LIMIT 100", pred_query, filt_query, rank_query)
    
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
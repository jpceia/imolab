
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
                                prop_types, multiple = TRUE,
                                selected = c("Apartment", "House")),
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
                   "search",
                   "Search",
                   icon = icon("search"),
                   style = "color: #ffffff; background-color: #474949"
                 ),
                 offset = 1
          )
        ),
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
      municipality_list <- df$id
      names(municipality_list) <- df$name
    }
    
    updateSelectInput(session, "municipality", choices = municipality_list)
    updateSelectInput(session, "parish",       choices = c(NULL))
  })
  
  
  # updates the parish list, after selecting a new municipality
  observeEvent(input$municipality, {
    parish_list <- c(NULL)
    df <- parish_sh %>% filter(CCA_2 == input$municipality)
    
    if (nrow(df) > 0) {
      parish_list <- df$id
      names(parish_list) <- df$name
    }
    
    updateSelectInput(session, "parish", choices = parish_list)
  })
}
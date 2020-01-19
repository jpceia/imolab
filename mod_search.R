
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
        fluidRow(
          column(4,
                 selectizeInput(ns("prop_types"), "Types of property",
                                prop_types, multiple = TRUE),
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
                                ))
          ),
          column(4,
                 selectizeInput(ns("objective"), "Objective",
                                c("Buy-to-Let", "Resell")),
                 numericInput(ns("max_investment"), "Maximum investment value",
                              100000, min=0),
                 sliderInput(ns("yield_target"), "Target Return",
                             min = 0, max = 10, value = 5, step = 0.01, post = " %")
          ),
          column(4,
                 numericInput(ns("fixed_cost"), "Fixed costs per transaction",
                              500, min = 0),
                 numericInput(ns("var_cost"), "Variable costs per transaction",
                              0.02, min = 0, max = 1)
          )
        ),
        fluidRow(
          column(12,
                 actionButton(
                   "search",
                   "Search",
                   icon = icon("search"),
                   style = "color: #ffffff; background-color: #474949"
                 )
          )
        ),
        width = 12
      )
    )
  )
}
  
  # photo
  # title
  # location
  # price
  # area
  # xPrice +- error
  # xRent +- error
  # explainability
  #   price distribution in sale parish / city
  #   map
  #   points of interest
  #   parish / city statistics
  # results

server_search  <- function(input, output, session) {
  
  # updates the city list, after selecting a new district
  observe({
    city_list <- c(NULL)
    df <- city_meta %>% filter(code1 == input$district)
    
    if (nrow(df) > 0) {
      city_list <- df$Dicofre
      names(city_list) <- df$Designacao
    }
    
    updateSelectInput(session, "city", choices = city_list)
    updateSelectInput(session, "parish", choices = c(" "))
  })
  
  
  # updates the parish list, after selecting a new city
  observe({
    parish_list <- c(NULL)
    df <- parish_meta %>% filter(code1 == input$city)
    
    if (nrow(df) > 0) {
      parish_list <- df$Dicofre
      names(parish_list) <- df$Designacao
    }
    
    updateSelectInput(session, "parish", choices = parish_list)
  })
}

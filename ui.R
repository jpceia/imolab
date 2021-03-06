

# ----------------------------------------------------------------------------------------
#                                        SIDE BAR
# ----------------------------------------------------------------------------------------


sideBar <- dashboardSidebar(
  tags$head(
    includeHTML("google-analytics.js"),
    tags$style(type = "text/css",
    "
      #rleaflet_control {
        padding: 0px;
        border-radius: 0px;
      }
      
      .modal-lg {
        width: 75%;
      }
    ")
  ),
  sidebarMenu(
    id = "sidebarmenu",
    # menuItem("Home", tabName = "homeTab"),
    menuItem("Search", icon = icon("search-dollar"),
             tabName = NS("mod_search")("investmentSearchTab")),

    menuItem("Appraisal", icon = icon("brain"),
             tabName = NS("mod_appraisal")("appraisalTab")),
             #badgeLabel = "Beta",
             #badgeColor = "blue"),
    menuItem("Exploration", icon = icon("poll"),
             menuSubItem("Territory",
                         tabName = "territoryTab"),
             menuSubItem("Categories",
                         tabName = "categoriesTab"),
             menuSubItem("Correlations",
                         tabName = "correlationTab"),
             menuSubItem("Histograms",
                         tabName = "histogramsTab")
    ),
    hr(),
    conditionalPanel(
      condition = "['histogramsTab', 'categoriesTab', 'correlationTab', 'territoryTab'].indexOf(input.sidebarmenu) >= 0",
      radioButtons("deal", NULL, c("Sale", "Rent"), inline = TRUE),
      selectizeInput("prop_type", NULL, prop_types, selected = c("Apartment", "House"), multiple = TRUE),
      selectizeInput("condition", "Condition", condition_codes, selected = NULL, multiple = TRUE),
      htmlOutput("geomenu")
    ),
    conditionalPanel(
      condition = "'histogramsTab' == input.sidebarmenu",
      radioButtons(
        "truncation",
        "Truncation",
        choiceNames = c("2.5%", "1%", "0.5%"),
        choiceValues = c(2.5, 1.0, 0.5),
        selected = 1.0,
        inline = TRUE
      )
    ),
    conditionalPanel(
      condition = "['categoriesTab', 'territoryTab'].indexOf(input.sidebarmenu) >= 0",
      selectizeInput("target_col", "Target",
                     target_list, selected = "Price/m2")
    )
  )
)


# ----------------------------------------------------------------------------------------
#                                          BODY
# ----------------------------------------------------------------------------------------

categoryTab <- function(id, title = NULL) {
  if(is.null(title))
  {
    title = id
  }
  
  tabPanel(
    title = tags$strong(title),
    fluidRow(
      box(
        fluidRow(
          column(6, highchartOutput(paste(id, "BoxPlot", sep = "")) %>% withSpinner(type=SPINNER_TYPE)),
          column(6, highchartOutput(paste(id, "Count", sep = "")) %>% withSpinner(type=SPINNER_TYPE))
        ), width = 12
      )
    ),
    fluidRow(
      box(
        column(12, formattableOutput(paste(id, "Table", sep = ""))),
        width = 12
      )
    )
  )
}


body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "histogramsTab",
      tags$h2(tags$strong('Price and Area distributions')),
      
      navbarPage("",
        tabPanel(
          title = tags$strong("Price / m2"),
          fluidRow(
            box(
              highchartOutput("HistogramPrice_m2") %>% withSpinner(type=SPINNER_TYPE),
              width = 12
            )
          )
        ),
        tabPanel(
          title = tags$strong("Price"),
          fluidRow(
            box(
              highchartOutput("HistogramPrice") %>% withSpinner(type=SPINNER_TYPE),
              width = 12
            )
          )
        ),
        tabPanel(
          title = tags$strong("Net Area"),
          fluidRow(
            box(
              highchartOutput("HistogramArea") %>% withSpinner(type=SPINNER_TYPE),
              width = 12
            )
          )
        )
      ),
      fluidRow(
        box(
          title = tags$strong("Quantiles"),
          status = "primary",
          collapsible = TRUE,
          collapsed = TRUE,
          formattableOutput("tableQuantiles"),
          width = 12
        )
      )
    ),
    tabItem(
      tabName = "categoriesTab",
      tags$h2(tags$strong("Categories")),
      tags$h5(tags$strong(textOutput("CategoryTextTargetName")), style='color:grey'),
      navbarPage("",
         categoryTab("EnergyCertificate",  "Energy Certificate"),
         categoryTab("Condition"),
         categoryTab("Tipology",           "Tipology"),
         categoryTab("Bathrooms",          "#Bathrooms"),
         categoryTab("ConstructionDecade", "Construction Decade")
      )
    ),
    tabItem(
      tabName = "territoryTab",
      tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      leafletOutput("map")
    ),
    tabItem(
      tabName = "correlationTab",
      tags$h2(tags$strong("Correlation Explorer")),
      tags$h5(tags$strong(textOutput("CorrelationTextTargetName")), style='color:grey'),
      fluidRow(
        box(
          fluidRow(
            column(4,
                   selectizeInput("target1", "Target1",
                                  target_list, selected = "price_m2"),
                   selectizeInput("target2", "Target2",
                                  target_list, selected = "Net.Area"),
                   htmlOutput("corr_level_input"),
                   htmlOutput("corr_agg_prop_input")
            ),
            column(8, highchartOutput("CorrelationPlot") %>% withSpinner(type=SPINNER_TYPE))
          ),
          width = 12
        )
      )
    ),
    ui_appraisal("mod_appraisal"),
    ui_search("mod_search")
  )
)


# ----------------------------------------------------------------------------------------
#                                           UI
# ----------------------------------------------------------------------------------------
  

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    #skin = "blue",
    dashboardHeader(title = "Imobom"),
    sideBar,
    body
  )
)

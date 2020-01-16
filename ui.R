

# ----------------------------------------------------------------------------------------
#                                        SIDE BAR
# ----------------------------------------------------------------------------------------

sideBar <- dashboardSidebar(
  tags$head(includeHTML("google-analytics.js")),
  sidebarMenu(
    id = "sidebarmenu",
    # menuItem("Home", tabName = "homeTab"),
    menuItem("Exploration", icon = icon("poll"),
             menuSubItem("Territory",
                         tabName = "territoryTab"),
             menuSubItem("Property Type",
                         tabName = "propertyTypeTab"),
             menuSubItem("Categories",
                         tabName = "categoriesTab"),
             menuSubItem("Correlations",
                         tabName = "correlationTab"),
             menuSubItem("Histograms",
                         tabName = "histogramsTab")
    ),
    menuItem("Valuation", icon = icon("brain"),
             tabName = NS("mod_valuation")("valuationTab"),
             badgeLabel = "Beta",
             badgeColor = "blue"),
    menuItem("Data Sources", icon = icon("table"),
             menuSubItem("Raw Data", tabName = "rawdataTab"),
             menuSubItem("Pivots", tabName = "pivotTableTab")),
    hr(),
    conditionalPanel(
      condition = "['histogramsTab', 'propertyTypeTab', 'categoriesTab', 'correlationTab', 'territoryTab'].indexOf(input.sidebarmenu) >= 0",
      radioButtons("deal", NULL, c("Sale", "Rent"), inline = TRUE)
    ),
    conditionalPanel(
      condition = "['histogramsTab', 'categoriesTab', 'correlationTab', 'territoryTab'].indexOf(input.sidebarmenu) >= 0",
      selectizeInput("prop_type", NULL, prop_types, selected = "Apartment", multiple = TRUE),
    ),
    conditionalPanel(
      condition = "['histogramsTab', 'propertyTypeTab', 'categoriesTab', 'correlationTab', 'territoryTab'].indexOf(input.sidebarmenu) >= 0",
      selectizeInput("district", "Location", district_list,
                     size = 3,
                     options = list(
                       placeholder = 'District',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      conditionalPanel(
        condition = "input.district != ''",
        selectizeInput("municipality", NULL, c(" "),
                       options = list(
                         placeholder = 'Municipality',
                         onInitialize = I('function() { this.setValue(""); }')
                       )),
        conditionalPanel(
          condition = "input.municipality != ''",
          selectizeInput("parish", NULL, c(" "),
                         multiple = FALSE,
                         options = list(
                           placeholder = 'Parish',
                           onInitialize = I('function() { this.setValue(""); }')
                         ))
        )
      )
    ),
    conditionalPanel(
      condition = "['histogramsTab'].indexOf(input.sidebarmenu) >= 0",
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
      condition = "['categoriesTab', 'propertyTypeTab', 'territoryTab'].indexOf(input.sidebarmenu) >= 0",
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
          column(6, plotOutput(paste(id, "BoxPlot", sep = "")) %>% withSpinner(type=SPINNER_TYPE)),
          column(6, plotOutput(paste(id, "Count", sep = "")) %>% withSpinner(type=SPINNER_TYPE))
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
          title = tags$strong("Area"),
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
      tabName = "propertyTypeTab",
      tags$h2(tags$strong("Property Type")),
      tags$h5(tags$strong(textOutput("PropertyTypeTextTargetName")), style='color:grey'),
      categoryTab("PropertyType", "Property Type"),
    ),
    tabItem(
      tabName = "categoriesTab",
      tags$h2(tags$strong("Categories")),
      tags$h5(tags$strong(textOutput("CategoryTextTargetName")), style='color:grey'),
      navbarPage("",
         categoryTab("EnergyCertificate", "Energy Certificate"),
         categoryTab("Condition"),
         categoryTab("Rooms",             "#Rooms"),
         categoryTab("Bathrooms",         "#Bathrooms"),
         categoryTab("ConstructionYear",  "Construction decade")
      )
    ),
    tabItem(
      tabName = "territoryTab",
      tags$h2(tags$strong("Territory")),
      tags$h5(tags$strong(textOutput("TerritoryTextTargetName")), style='color:grey'),
      uiOutput("territory_tab")
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
                                  target_list, selected = "area"),
                   selectizeInput("target2", "Target2",
                                  target_list, selected = "price_m2"),
                   selectizeInput("agg_level", "Aggregation Level",
                                  c("District", "Municipality", "Parish"),
                                  selected = "District"),
                   checkboxInput("agg_prop_type", "Aggregate different property types")
            ),
            column(8, highchartOutput("CorrelationPlot") %>% withSpinner(type=SPINNER_TYPE))
          ),
          width = 12
        )
      )
    ),
    tabItem(
      tabName = "rawdataTab",
      tags$h2(tags$strong('Raw Data')),
      box(
        title = tags$strong("Imovirtual database"),
        solidHeader = TRUE,
        status = "primary",
        collapsible = TRUE,
        collapsed = FALSE,
        DT::dataTableOutput("rawDataTable") %>% withSpinner(type=SPINNER_TYPE),
        width = 12
      )
    ),
    tabItem(
      tabName = "pivotTableTab",
      #tags$h2(tags$strong("Pivot Table")),
      #tags$h3("Imovirtual database"),
      rpivotTableOutput("pivotTable") %>% withSpinner(type=SPINNER_TYPE)
    ),
    ui_valuation("mod_valuation")
  )
)


# ----------------------------------------------------------------------------------------
#                                           UI
# ----------------------------------------------------------------------------------------
  

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    #skin = "blue",
    dashboardHeader(title = "IMO Lab"),
    sideBar,
    body
  )
)

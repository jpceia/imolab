

# ----------------------------------------------------------------------------------------
#                                        SIDE BAR
# ----------------------------------------------------------------------------------------

sideBar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebarmenu",
    # menuItem("Home", tabName = "homeTab"),
    menuItem("Exploration", icon = icon("poll"),
             menuSubItem("Histograms",
                         tabName = "histogramsTab"),
             menuSubItem("Categories",
                         tabName = "categoriesTab")),
    menuItem("Territory", tabName = "territoryTab", icon = icon("globe-africa")
    ),
    menuItem("Data Sources", icon = icon("table"),
             menuSubItem("Raw Data", tabName= "rawdataTab"),
             menuSubItem("Pivots", tabName = "pivotTableTab")),
    menuItem("Valuation", icon = icon("brain"),
             tabName = "valuationTab",
             badgeLabel = "Beta",
             badgeColor = "blue"),
    
    hr(),
    conditionalPanel(
      condition = "['histogramsTab', 'categoriesTab', 'territoryTab'].indexOf(input.sidebarmenu) >= 0",
      selectizeInput("prop_type", NULL, prop_types,
                     multiple = TRUE,
                     options = list(
                       placeholder = "Property Type",
                       onInitialize = I('function() { this.setValue("Apartment"); }')
                     )
      ),
      radioButtons("is_sale", NULL,
                   choiceNames = c("Sale", "Rent"),
                   choiceValues = c(TRUE, FALSE),
                   inline = TRUE)
    ),
    conditionalPanel(
      condition = "['histogramsTab', 'categoriesTab'].indexOf(input.sidebarmenu) >= 0",
      selectizeInput("district", "Location", district_list,
                     size = 3,
                     options = list(
                       placeholder = 'District',
                       onInitialize = I('function() { this.setValue(""); }')
                     )),
      conditionalPanel(
        condition = "input.district != ''",
        selectizeInput("city", NULL, c(" "),
                       options = list(
                         placeholder = 'Municipality',
                         onInitialize = I('function() { this.setValue(""); }')
                       )),
        conditionalPanel(
          condition = "input.city != ''",
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
      condition = "input.sidebarmenu == 'categoriesTab'",
      selectInput("category", "Category", c("energy_certificate", "condition",	"rooms", "bathrooms"))
    ),
    conditionalPanel(
      condition = "['histogramsTab', 'categoriesTab'].indexOf(input.sidebarmenu) >= 0",
      radioButtons("truncation", "Truncation",
                   choiceNames = c("2.5%", "1%", "0.5%"),
                   choiceValues = c(2.5, 1.0, 0.5),
                   selected = 1.0,
                   inline = TRUE)
    ),
    conditionalPanel(
      condition = "input.sidebarmenu == 'valuationTab'",
      fluidRow(actionButton("calculate_val", tags$strong("Calculate")), align="center")
    )
  )
)



# ----------------------------------------------------------------------------------------
#                                          BODY
# ----------------------------------------------------------------------------------------


body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "histogramsTab",
      h2('Price and Area distributions'),
      box(
        tabsetPanel(
          type="tabs",
          tabPanel(
            "Price / m2",
            fluidRow(
              column(12, highchartOutput("HistogramPrice_m2") %>% withSpinner(type=SPINNER_TYPE))
            ),
            fluidRow(
              column(12, DT::dataTableOutput("tablePrice_m2"))
            )
          ),
          tabPanel(
            "Prices",
            fluidRow(
              highchartOutput("HistogramPrice") %>% withSpinner(type=SPINNER_TYPE)
            ),
            fluidRow(
              column(6, verbatimTextOutput("textReport2")),
              column(6, formattableOutput("tablePrice"))
            )
          ),
          tabPanel(
            "Areas",
            fluidRow(
              highchartOutput("HistogramArea") %>% withSpinner(type=SPINNER_TYPE)
            ),
            fluidRow(
              column(6, verbatimTextOutput("textReport3")),
              column(6, formattableOutput("tableArea"))
            )
          ),
          tabPanel(
            "Price vs Area",
            fluidRow(
              column(7, plotOutput("ScatterPriceArea") %>% withSpinner(type=SPINNER_TYPE))
            )
          )
        ),
        width=12
      )
    ),
    tabItem(
      tabName = "categoriesTab",
      h2(textOutput("categoryText")),
      fluidRow(
        box(
          fluidRow(
            column(12, plotOutput("CategoriesBoxPlot") %>% withSpinner(type=SPINNER_TYPE))
          ), width = 6
        ),
        box(
          fluidRow(
            column(12, plotOutput("CategoriesCount") %>% withSpinner(type=SPINNER_TYPE))
          ), width = 6
        )
      ),
      fluidRow(
        box(
          column(12, tableOutput("tableCategories") %>% withSpinner(type=SPINNER_TYPE)),
          width = 12
        )
      )
    ),
    tabItem(
      tabName = "territoryTab"
    ),
    tabItem(
      tabName = "rawdataTab",
      h2('Imovirtual database'),
      box(
        DT::dataTableOutput("rawDataTable") %>% withSpinner(type=SPINNER_TYPE),
        width = 12
      )
    ),
    tabItem(
      tabName = "pivotTableTab",
      h2("Pivot Table"),
      rpivotTableOutput("pivotTable") %>% withSpinner(type=SPINNER_TYPE)
    ),
    tabItem(
      tabName = "valuationTab",
      h2('Valuation'),
      box(
        column(3,
               box(
                 selectizeInput("prop_type_val", "Property Type", prop_types, selected = "Apartment"),
                 radioButtons("is_sale_val", NULL,
                              choiceNames = c("Sale", "Rent"),
                              choiceValues = c(TRUE, FALSE),
                              inline = TRUE),
                 numericInput("net_area_val", "Net Area", NULL, min=0, step=1),
                 numericInput("gross_area_val", "Gross Area", NULL, min=0, step=1),
                 numericInput("terrain_area_val", "Terrain Area", NULL, min=0, step=1),
                 numericInput("rooms_val", "#Rooms", NULL, min=0, max=10, step=1),
                 numericInput("bathrooms_val", "#Bathrooms", NULL, min=0, max=4, step=1),
                 selectInput("condition_val", "Condition", choices=condition_levels),
                 width = 12,
                 status = "warning"
               )
        ),
        column(3,
               box(
                 
                 selectizeInput("district_val", "Location", district_list,#,  district
                                size = 3,
                                options = list(
                                  placeholder = 'District',
                                  onInitialize = I('function() { this.setValue(""); }')
                                )),
                 selectizeInput("city_val", NULL, c(" "),
                                options = list(
                                  placeholder = 'Municipality',
                                  onInitialize = I('function() { this.setValue(""); }')
                                )),
                 selectizeInput("parish_val", NULL, c(" "),
                                multiple = FALSE,
                                options = list(
                                  placeholder = 'Parish',
                                  onInitialize = I('function() { this.setValue(""); }')
                                )),
                 width = 12,
                 status = "warning"
               )
        ),
        column(6,
               box(
                 highchartOutput("valuationOutput"),
                 width = 12,
                 status = "primary"
               )
        ),
        width = 12
      )
    )
  )
)


# ----------------------------------------------------------------------------------------
#                                           UI
# ----------------------------------------------------------------------------------------
  

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "IMO Lab"),
    sideBar,
    body
  )
)

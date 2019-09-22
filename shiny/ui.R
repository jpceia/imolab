

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
    menuItem("Territory", tabName = "territoryTab", icon = icon("globe-africa")),
    menuItem("Data Sources", icon = icon("table"),
             menuSubItem("Raw Data", tabName = "rawdataTab"),
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
      fluidRow(
        box(
          title = tags$strong("Price / m2"),
          status = "primary",
          #solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          highchartOutput("HistogramPrice_m2") %>% withSpinner(type=SPINNER_TYPE),
          width = 12
        )
      ),
      fluidRow(
        box(
          title = tags$strong("Price"),
          status = "primary",
          #solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          highchartOutput("HistogramPrice") %>% withSpinner(type=SPINNER_TYPE),
          width = 12
        )  
      ),
      fluidRow(
        box(
          title = tags$strong("Area"),
          status = "primary",
          #solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          highchartOutput("HistogramArea") %>% withSpinner(type=SPINNER_TYPE),
          width = 12
        )
      ),
      fluidRow(
        box(
          title = tags$strong("Quantiles"),
          collapsible = TRUE,
          collapsed = TRUE,
          formattableOutput("tableQuantiles"),
          width = 12
        )
      )
      #,
      #fluidRow(
      #  box(
      #    title = h3("Price vs Area"),
      #    status = "primary",
      #    column(7, plotOutput("ScatterPriceArea") %>% withSpinner(type=SPINNER_TYPE)),
      #    width = 12
      #  )  
      #)
      # idea: Add Histograms and Quantiles tab
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
          column(12, formattableOutput("tableCategories")),
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
      # h2('Valuation'),
      navbarPage("", id="valuation_tabs",
        tabPanel(
          tabName = "valuationInputs",
          title = "Inputs",
          status = "primary",
          box(
            fluidRow(
              column(4,
                     selectizeInput("prop_type_val", "Property Type", prop_types, selected = "Apartment"),
                     radioButtons("is_sale_val", NULL,
                                  choiceNames = c("Sale", "Rent"),
                                  choiceValues = c(TRUE, FALSE),
                                  inline = TRUE),
                     selectInput("condition_val",
                                 "Condition",
                                 selected = "usado",
                                 choices = condition_levels),
                     selectInput("energy_certificate_val",
                                 "Energy Certificate", "D",
                                 choices = energy_certificate_levels)
              ),
              column(4,
                     numericInput("net_area_val",           "Net Area",     100, min=0, step=1),
                     numericInput("gross_area_val",         "Gross Area",   115, min=0, step=1),
                     numericInput("terrain_area_val",       "Terrain Area", NULL, min=0, step=1),
                     numericInput("rooms_val",              "#Rooms",       2, min=0, max=10, step=1),
                     numericInput("bathrooms_val",          "#Bathrooms",   1, min=0, max=4, step=1)
              ),
              column(4,
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
                                    ))
              )
            ),
            width = 12
          )
        ),
        tabPanel(
          tabName = "valuationResults",
          title = "Simulation Results",
          icon = icon("calculator"),
          status = "primary",
          box(
            verbatimTextOutput("valuationResult"),
            width = 12
          )
        )
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

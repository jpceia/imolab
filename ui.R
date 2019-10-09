

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
                         tabName = "categoriesTab"),
             menuSubItem("Territory",
                         tabName = "territoryTab")
    ),
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
      radioButtons("deal_type", NULL,
                   choiceNames = c("Sale", "Rent"),
                   choiceValues = c("Sale", "Rent"),
                   inline = TRUE),
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
      ),
      radioButtons(
        "truncation",
        "Truncation",
        choiceNames = c("2.5%", "1%", "0.5%"),
        choiceValues = c(2.5, 1.0, 0.5),
        selected = 1.0,
        inline = TRUE
      )
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
      tabName = "categoriesTab",
      tags$h2(tags$strong("Categories")),
      navbarPage("",
         categoryTab("EnergyCertificate", "Energy Certificate"),
         categoryTab("Condition"),
         categoryTab("Rooms", "#Rooms"),
         categoryTab("Bathrooms", "#Bathrooms")
      )
    ),
    tabItem(
      tabName = "territoryTab",
      tags$h2(tags$strong("Territory")),
      tags$h5(tags$strong("Price/m2"), style='color:grey'),
      uiOutput("territory_tab")
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
    tabItem(
      tabName = "valuationTab",
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
                   selectizeInput("prop_type_val", "Property Type", prop_types, selected = "Apartment"),
                   radioButtons("deal_type_val", NULL,
                                choiceNames = c("Sale", "Rent"),
                                choiceValues = c("Sale", "Rent"),
                                inline = TRUE),
                   selectInput("energy_certificate_val",
                               "Energy Certificate", "d",
                               choices = energy_certificate_levels),
                   selectInput("condition_val",
                               "Condition",
                               selected = "usado",
                               choices = condition_levels),
                   numericInput("construction_year_val",
                                "Construction Year", 1990, min=0, max=2020)
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
      fluidRow(
        box(
          title = tags$strong("Simulation Results"),
          #status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          formattableOutput("valuationResult"),
          width = 12
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
    #skin = "blue",
    dashboardHeader(title = "IMO Lab"),
    sideBar,
    body
  )
)
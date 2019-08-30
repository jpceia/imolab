#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)

# TODO's
#  * adicionar city / parish
#  * adicionar estatisticas ao lado do grafico, c possibilidade de fazer download
#      - datapoints
#      - median
#      - quantiles

# What make us different

# Interactive Pareto Curve

# Categories
#  * Energy Certificates


districts <- c(
  "ALL",
  'Aveiro',
  'Beja',
  'Braga',
  'Braganca',
  'Castelo Branco',
  'Coimbra',
  'Evora',
  'Faro',
  'Guarda',
  'Leiria',
  'Lisboa',
  'Portalegre',
  'Porto',
  'Santarem',
  'Setubal',
  'Viana do Castelo',
  'Vila Real',
  'Viseu'
)


prop_types <- c(
  'Apartment',
  'House',
  'Terrain',
  'Store',
  'Warehouse',
  'Garage',
  'Office',
  'Building',
  'Farm'
)


sideBar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Exploration",
             tabName = "explorationTab", #icon=icon("wpexplorer"),
             menuSubItem("Histograms",
                         tabName = "histogramsTab"),
             menuSubItem("Pareto",
                         tabName = "paretoTab"),
             menuSubItem("Categories",
                         tabName = "categoriesTab")),
    menuItem("Territory", tabName = "territoryTab"#, icon = icon("globe-africa")
    ),
    menuItem("Simulator",
             # tabName = "simulatorTab",
             icon = icon("dashboard"),
             badgeLabel = "Beta",
             badgeColor = "blue"),
    
    hr(),
    radioButtons("is_sale", NULL,
                 choiceNames = c("Sale", "Rent"),
                 choiceValues = c(TRUE, FALSE),
                 inline = TRUE),
    selectInput("prop_type", "Property Type", prop_types,
                selected = "Apartment", multiple = FALSE),
    selectInput("district", "District", districts,
                selected = "Lisboa", multiple = FALSE),
    radioButtons("truncation", "Truncation",
                 choiceNames = c("1%", "0.1%", "0.01%"),
                 choiceValues = c(1.0, 0.1, 0.01),
                 inline = TRUE),
    radioButtons("granularity", "Granularity",
                 choiceNames = c("High", "Medium", "Low"),
                 choiceValues = c(100, 30, 10),
                 selected=30,
                 inline = TRUE)
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "histogramsTab",
      box(
      tabsetPanel(
        type="tabs",
        tabPanel(
          "Price / m2",
          fluidRow(
            column(8, plotOutput("HistogramPrice_m2"))
          )
        ),
        tabPanel(
          "Prices",
          fluidRow(
            column(8, plotOutput("HistogramPrice"))
          )
        ),
        tabPanel(
          "Areas",
          fluidRow(
            column(8, plotOutput("HistogramArea"))
          ))
      ),
      width=12
    )
    )
  )
)


  

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = "IMO Lab"),
    sideBar,
    body
  )
)

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
#library(leaflet)
#library(ggiraph)

# TODO's
#  * adicionar city / parish
#  * adicionar estatisticas ao lado do grafico, c possibilidade de fazer download
#      - datapoints
#      - median
#      - quantiles
# Waiting time
# Warning quando há poucos datapoints (<25)

# Interactive Pareto Curve

# Categories
#  * Energy Certificates
#  * n_rooms


# shinythemes
# shinyjs


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
                selected = " ", multiple = FALSE),
    selectInput("city", "City", c(" ")),
    selectInput("parish", "Parish", c(" ")),
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
              column(7, plotOutput("HistogramPrice_m2"))
            )
          ),
          tabPanel(
            "Prices",
            fluidRow(
              column(7, plotOutput("HistogramPrice"))
            )
          ),
          tabPanel(
            "Areas",
            fluidRow(
              column(7, plotOutput("HistogramArea"))
            )
          )
        ),
        width=12
      )
    ),
    tabItem(
      tabName = "categoriesTab",
      box(
        tabsetPanel(
          type="tabs",
          tabPanel(
            "Energy Certificate",
            fluidRow(
              column(7, plotOutput("CategoriesEnergyCertificate"))
            )
          ),
          tabPanel(
            "#Rooms",
            fluidRow(
              column(7, plotOutput("CategoriesRooms"))
            ),
            fluidRow(),
            fluidRow(
              column(7, plotOutput("CategoriesRoomsArea"))
            )
          )
        ),
        width=12
      )
    ),
    tabItem(
      tabName = "territoryTab"#, leafletOutput("mymap")
    )
    
  )
)


  

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "IMO Lab"),
    sideBar,
    body
  )
)

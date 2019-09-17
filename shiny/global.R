
# ----------------------------------------------------------------------------------------
#                                         IMPORTS
# ----------------------------------------------------------------------------------------

library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(highcharter)
library(leaflet)
library(rpivotTable)


# ----------------------------------------------------------------------------------------
#                                       STATIC DATA
# ----------------------------------------------------------------------------------------

SPINNER_TYPE <- 8

dataFolder <- "C:/Users/joaop/OneDrive - insidemedia.net/Data/1 - Houses"
district_meta <- read_csv(
  file.path(dataFolder, "geodata", "district_meta.csv"),
  col_types = c(Designacao="f"),
  locale = readr::locale(encoding = "latin1")
  ) %>%
  filter(Continente) %>%
  select(Dicofre, Designacao)

district_list <- district_meta$Dicofre
names(district_list) <- district_meta$Designacao

energy_certificate_levels = c("Isento", "G", "F", "E", "D", "C", "B-", "B", "A", "A+")
bathrooms_levels = c("1", "2", "3", "4")
rooms_levels = 0:10
condition_levels = c("ruina", "para_recuperar", "usado", "em_construcao", "renovado", "novo")


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

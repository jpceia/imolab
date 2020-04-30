
# ----------------------------------------------------------------------------------------
#                                         IMPORTS
# ----------------------------------------------------------------------------------------

library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyBS)
library(tidyverse)
library(dplyr)
library(data.table)
library(highcharter)
library(leaflet.extras)
library(formattable)
library(openssl)
library(RMySQL)

require(jsonlite)
require(scales)
require(httr)
require(DT)
require(sf)
require(ids)

source("utils.R")
source("mod_valuation.R")
source("mod_search.R")


# ----------------------------------------------------------------------------------------
#                                       STATIC DATA
# ----------------------------------------------------------------------------------------


SEARCH_TAB <- FALSE
SPINNER_TYPE <- 8
MIN_DATAPOINTS <- 5
SAMPLING_THRESHOLD <- 2500
MIN_DATAPOINTS_MSG <- "Filter too narrow: not enough datapoints"

DB_HOST <- "#{DB_HOST}"
DB_PORT <- 3306
DB_NAME <- "rentalgo"
DB_USER <- "admin"
DB_PWD <- "***REMOVED***"
DB_TABLE <- "imovirtual_prod"

for(conn in dbListConnections(dbDriver(drv = "MySQL")))
{
  print(conn)
  dbDisconnect(conn)
}

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

prop_types_ids <- c(1, 2, 4, 5, 6, 7, 8, 9, 11)


condition_levels <- c(
  "Ruina",
  "Para Recuperar",
  "Usado",
  "Em Construcao",
  "Renovado",
  "Novo"
)

condition_ids <- c(
  0, 1, 2, 5, 3, 4
)

condition_codes <- c(
  "ruina",
  "para_recuperar",
  "usado",
  "em_construcao",
  "renovado",
  "novo"
)


energy_certificate_levels <- c(
  "Isento",
  "G", "F", "E", "D", "C", "B-", "B", "A", "A+"
)

energy_certificate_ids <- c(
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9
)

other_attrs <- list(
  Elevator = 'Elevator',
  Balcony = 'Balcony',
  View = 'View',
  Garden = 'Garden',
  Swimming.Pool = 'Swimming Pool',
  Garage = 'Garage',
  Parking = 'Parking'
)

bathrooms_levels <- 1:4

bedrooms_levels <- 0:10

target_list <- list(
  'Price/m2' = 'price_m2',
  'Area' = 'Area',
  'Construction Year' = 'Construction.Year'
)


decades <- c(-Inf, as.integer(seq(1900, 2020, 10)))
decades_labels <- as.character(decades)
decades_labels[1] <- "< 1900"
decades_labels <- decades_labels[1: length(decades_labels) - 1]


# ------------------------------------ TERRITORY MAPS ------------------------------------

start_time <- Sys.time()

district_sh <- sf::read_sf(file.path("data", "geo", "distritos-shapefile", "distritos.shp"))
district_sh  <- district_sh[district_sh$TYPE_1 == "Distrito", ]
names <- district_sh$NAME_1
# polygon simplification to speedup rendering
district_sh <- rmapshaper::ms_simplify(district_sh, keep_shapes = TRUE)
district_sh$id <- district_sh$CCA_1
district_sh$name <- names


municipality_sh <- sf::read_sf(file.path("data", "geo", "concelhos-shapefile", "concelhos.shp"))
names <- municipality_sh$NAME_2
municipality_sh <- rmapshaper::ms_simplify(municipality_sh, keep_shapes = TRUE)
#municipality_sh <- municipality_sh[municipality_sh$ID_1 %in% district_sh$ID_1, ]
municipality_sh$CCA_1 <- stringr::str_sub(municipality_sh$CCA_2, 1, -3)
municipality_sh$id <- municipality_sh$CCA_2
municipality_sh$name <- names


## LOADING PARISH DATA
# https://dados.gov.pt/s/resources/freguesias-de-portugal/20181112-195834/cont-aad-caop2017.zip
parish_sh <- sf::read_sf(file.path("data", "geo", "cont-aad-caop2017", "Cont_AAD_CAOP2017.shp"))
print(object.size(parish_sh) / 1024 / 1024)
names <- as.data.frame(parish_sh)[, c("Dicofre", "Freguesia", "Des_Simpli")]
parish_sh <- rmapshaper::ms_simplify(parish_sh)
print(object.size(parish_sh) / 1024 / 1024)
parish_sh$CCA_1 <- stringr::str_sub(parish_sh$Dicofre, 1, -5)
parish_sh$CCA_2 <- stringr::str_sub(parish_sh$Dicofre, 1, -3)
parish_sh$CCA_3 <- parish_sh$Dicofre
parish_sh$id <- parish_sh$CCA_3
parish_sh$name <- names$Des_Simpli[match(parish_sh$Dicofre, names$Dicofre)]
parish_sh <- sf::st_transform(parish_sh, "+init=epsg:4326")

end_time <- Sys.time()


district_list <- setNames(district_sh$CCA_1, district_sh$NAME_1)

# ------------------------------- Loading the main Dataset -------------------------------

dataset <- data.table(load_dataset())

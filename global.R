
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
library(rapportools) # is.empty
library(formattable)
library(xgboost)

require(sf)

source("utils.R")


# ----------------------------------------------------------------------------------------
#                                       STATIC DATA
# ----------------------------------------------------------------------------------------

SPINNER_TYPE <- 8
MIN_DATAPOINTS <- 5


energy_certificate_levels <- list(
  Isento = "isento",
  G = "g",
  `F` = "f",
  E = "e",
  D = "d",
  C = "c",
  `B-` = "bminus",
  B = "b",
  A = "a",
  `A+` = "aplus"
)

bathrooms_levels <- 1:4

rooms_levels <- 0:10

condition_levels <- list(
  Ruina = "ruina",
  `Para recuperar` = "para_recuperar",
  Usado = "usado",
  `Em construcao` = "em_construcao",
  Renovado = "renovado", 
  Novo = "novo"
)

decades <- c(-Inf, as.integer(seq(1900, 2020, 10)))
decades_labels <- as.character(decades)
decades_labels[1] <- "< 1900"
decades_labels <- decades_labels[1: length(decades_labels) - 1]


# ------------------------------------- LOCATION DATA ------------------------------------

district_meta <- read_csv(
    file.path("data", "geo", "district_meta.csv"),
    col_types = c(Designacao="f"),
    locale = readr::locale(encoding = "latin1")
  ) %>%
  filter(Continente) %>%
  select(Dicofre, Designacao)


city_meta <- read_csv(
    file.path("data", "geo", "concelho_meta.csv"),
    col_types = c(Designacao="f"),
    locale = readr::locale(encoding = "latin1")
  ) %>%
  mutate(code1 = stringr::str_sub(as.character(Dicofre), 0, -3)) %>%
  filter(code1 %in% district_meta$Dicofre) %>%
  select(code1, Dicofre, Designacao)


parish_meta <- read_csv(
    file.path("data", "geo", "freguesias_meta.csv"),
    col_types = c(Designacao="f"),
    locale = readr::locale(encoding = "latin1")
  ) %>%
  mutate(code1 = stringr::str_sub(as.character(Dicofre), 0, -3)) %>%
  filter(code1 %in% city_meta$Dicofre) %>%
  select(code1, Dicofre, Designacao)


district_list <- district_meta$Dicofre
names(district_list) <- district_meta$Designacao


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



dataset <- load_dataset()


# ------------------------------------ TERRITORY MAPS ------------------------------------


country_map_sh <- sf::read_sf(file.path("data", "geo", "distritos-shapefile", "distritos.shp"))
country_map_sh <- rmapshaper::ms_simplify(country_map_sh ) # polygon simplification to speedup rendering
country_map_sh  <- country_map_sh[country_map_sh$TYPE_1 == "Distrito", ]
country_map_sh$CCA_1 <- as.factor(as.integer(country_map_sh$CCA_1))
country_map_sh$id <- country_map_sh$CCA_1
country_map_sh$name <- country_map_sh$NAME_1


district_map_sh <- sf::read_sf(file.path("data", "geo", "concelhos-shapefile", "concelhos.shp"))
district_map_sh <- rmapshaper::ms_simplify(district_map_sh)
#district_map_sh <- district_map_sh[district_map_sh$ID_1 %in% country_map_sh$ID_1, ]
district_map_sh$CCA_1 <- as.factor(as.integer(stringr::str_sub(district_map_sh$CCA_2, 1, -3)))
district_map_sh$CCA_2 <- as.factor(as.integer(district_map_sh$CCA_2))
district_map_sh$id <- district_map_sh$CCA_2
district_map_sh$name <- district_map_sh$NAME_2

## LOADING PARISH DATA
# https://dados.gov.pt/s/resources/freguesias-de-portugal/20181112-195834/cont-aad-caop2017.zip
city_map_sh <- sf::read_sf(file.path("data", "geo", "cont-aad-caop2017", "Cont_AAD_CAOP2017.shp"))
city_map_sh <- rmapshaper::ms_simplify(city_map_sh)
city_map_sh$CCA_1 <- as.factor(as.integer(stringr::str_sub(city_map_sh $Dicofre, 1, -5)))
city_map_sh$CCA_2 <- as.factor(as.integer(stringr::str_sub(city_map_sh $Dicofre, 1, -3)))
city_map_sh$CCA_3 <- as.factor(as.integer(city_map_sh $Dicofre, 1, -3))
city_map_sh$id <- city_map_sh$CCA_3
city_map_sh$name <- city_map_sh$Freguesia
city_map_sh <- sf::st_transform(city_map_sh, "+init=epsg:4326")


# ------------------------------------- XGBOOST MODEL ------------------------------------


match_tables <- list()

# cat
# cat - district
# cat - district - city
# cat - district - city - parish
# cat - energy_certificate
# cat - condition

match_tables$mean.enc.cat <- dataset %>%
  group_by(DealType, PropType) %>%
  summarize(mean.enc.cat = median(price_m2)) %>%
  na.omit()

match_tables$mean.area.enc.cat <- dataset %>%
  group_by(PropType) %>%
  summarize(mean.area.enc.cat = median(area)) %>%
  na.omit()

match_tables$count.enc.cat <- dataset %>%
  group_by(PropType) %>%
  summarize(count.enc.cat = n(price_m2)) %>%
  na.omit()


match_tables$mean.enc.cat.district <- dataset %>%
  group_by(DealType, PropType, district_code) %>%
  summarize(mean.enc.cat.district = median(price_m2)) %>%
  na.omit()

match_tables$count.enc.district <- dataset %>%
  group_by(district_code) %>%
  summarize(count.enc.district = n(price_m2)) %>%
  na.omit()


match_tables$mean.enc.cat.city <- dataset %>%
  group_by(DealType, PropType, district_code, city_code) %>%
  summarize(mean.enc.cat.city = median(price_m2)) %>%
  na.omit()

match_tables$count.enc.city <- dataset %>%
  group_by(city_code) %>%
  summarize(count.enc.city = n(price_m2)) %>%
  na.omit()


match_tables$mean.enc.cat.parish <- dataset %>%
  group_by(DealType, PropType, district_code, city_code, parish_code) %>%
  summarize(mean.enc.cat.parish = median(price_m2)) %>%
  na.omit()

match_tables$count.enc.parish <- dataset %>%
  group_by(parish_code) %>%
  summarize(count.enc.parish = n(price_m2)) %>%
  na.omit()


match_tables$mean.enc.cat.energy_certificate <- dataset %>%
  group_by(DealType, PropType, energy_certificate) %>%
  summarize(mean.enc.cat.energy_certificate = median(price_m2)) %>%
  na.omit()


match_tables$mean.enc.cat.condition <- dataset %>%
  group_by(DealType, PropType, condition) %>%
  summarize(mean.enc.cat.condition = median(price_m2)) %>%
  na.omit()

match_tables$count.enc.condition <- dataset %>%
  group_by(condition) %>%
  summarize(count.enc.condition = n(price_m2)) %>%
  na.omit()

X <- get_features(dataset, match_tables)

cat("Started XGBoost training\n")

xgb <- list()

fairobj <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  c <- 2 # the lower the "slower/smoother" the loss is. Cross-Validate.
  x <-  preds-labels
  grad <- c * x / (abs(x) + c)
  hess <- c ^ 2 / (abs(x) + c) ^ 2
  return(list(grad = grad, hess = hess))
}

y <- log10(dataset$price_m2)
xgb$price_m2 <- xgb.train(
  data = xgb.DMatrix(data = as.matrix(X), label = y),
  eta = 0.30,
  max.depth = 6,
  nround = 100,
  seed = 0,
  nthread = 4,
  objective = fairobj , #"reg:squarederror",
  eval.metric = "mae",
  verbose = 2
)


rm(X)
rm(y)

cat("Ended XGBoost training\n")


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
library(xgboost)


source("utils.R")


# ----------------------------------------------------------------------------------------
#                                       STATIC DATA
# ----------------------------------------------------------------------------------------

SPINNER_TYPE <- 8
MIN_DATAPOINTS <- 5


dataFolder <- "C:/Users/joaop/OneDrive - insidemedia.net/Data/1 - Houses"

energy_certificate_levels <- c("Isento", "G", "F", "E", "D", "C", "B-", "B", "A", "A+")
bathrooms_levels <- c("1", "2", "3", "4")
rooms_levels <- 0:10
condition_levels <- c("ruina", "para_recuperar", "usado", "em_construcao", "renovado", "novo")


# ------------------------------------- LOCATION DATA ------------------------------------

district_meta <- read_csv(
    file.path(dataFolder, "geodata", "district_meta.csv"),
    col_types = c(Designacao="f"),
    locale = readr::locale(encoding = "latin1")
  ) %>%
  filter(Continente) %>%
  select(Dicofre, Designacao)


city_meta <- read_csv(
    file.path(dataFolder, "geodata", "concelho_meta.csv"),
    col_types = c(Designacao="f"),
    locale = readr::locale(encoding = "latin1")
  ) %>%
  mutate(code1 = stringr::str_sub(as.character(Dicofre), 0, -3)) %>%
  filter(code1 %in% district_meta$Dicofre) %>%
  select(code1, Dicofre, Designacao)


parish_meta <- read_csv(
    file.path(dataFolder, "geodata", "freguesias_meta.csv"),
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



# ------------------------------------- XGBOOST MODEL ------------------------------------


match_tables <- list()

# cat
# cat - district
# cat - district - city
# cat - district - city - freg
# cat - energy_certificate
# cat - condition

match_tables$mean.enc.cat <- dataset %>%
  group_by(Sale, PropType) %>% summarize(median.enc.cat = median(price_m2)) %>% na.omit()
match_tables$mean.enc.cat.district <- dataset %>%
  group_by(Sale, PropType, district) %>% summarize(mean.enc.cat.district = median(price_m2)) %>% na.omit()
match_tables$mean.enc.cat.city <- dataset %>%
  group_by(Sale, PropType, district, city) %>% summarize(mean.enc.cat.city = median(price_m2)) %>% na.omit()
match_tables$mean.enc.cat.parish <- dataset %>%
  group_by(Sale, PropType, district, city, freg) %>% summarize(mean.enc.cat.parish = median(price_m2)) %>% na.omit()
match_tables$mean.enc.cat.energy_certificate <- dataset %>%
  group_by(Sale, PropType, energy_certificate) %>% summarize(mean.enc.cat.energy_certificate = median(price_m2)) %>% na.omit()
match_tables$mean.enc.cat.condition <- dataset %>%
  group_by(Sale, PropType, condition) %>% summarize(mean.enc.cat.condition = median(price_m2)) %>% na.omit()

X <- get_features(dataset, match_tables)

cat("Started XGBoost training\n")

xgb <- list()

fairobj <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  c <- 2 #the lower the "slower/smoother" the loss is. Cross-Validate.
  x <-  preds-labels
  grad <- c*x / (abs(x)+c)
  hess <- c^2 / (abs(x)+c)^2
  return(list(grad = grad, hess = hess))
}

y <- log10(dataset$price_m2)
xgb$price_m2 <- xgb.train(
  data = xgb.DMatrix(data = as.matrix(X), label = y),
  #eta = 0.30,
  #max.depth = 6,
  nround = 50,
  seed = 0,
  nthread = 4,
  objective = fairobj , #"reg:squarederror",
  #eval.metric = "rmse",
  eval.metric = "mae",
  verbose = 2
)

y <- log10(dataset$price)
xgb$price <- xgb.train(
  data = xgb.DMatrix(data = as.matrix(X), label = y),
  #eta = 0.30,
  #max.depth = 6,
  nround = 50,
  seed = 0,
  nthread = 4,
  objective = fairobj , #"reg:squarederror",
  #eval.metric = "rmse",
  eval.metric = "mae",
  verbose = 2
)


# removing variables to clean memory
rm(X)
rm(y)

cat("Ended XGBoost training\n")

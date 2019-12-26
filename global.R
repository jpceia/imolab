
# ----------------------------------------------------------------------------------------
#                                         IMPORTS
# ----------------------------------------------------------------------------------------

library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyBS)
library(tidyverse)
library(dplyr)
library(highcharter)
library(leaflet)
library(rpivotTable)
library(rapportools) # is.empty
library(formattable)
library(xgboost)
library(openssl)

require(DT)
require(sf)
require(ids)

source("utils.R")
source("mod_valuation.R")
source("mod_search.R")


# ----------------------------------------------------------------------------------------
#                                       STATIC DATA
# ----------------------------------------------------------------------------------------

SPINNER_TYPE <- 8
MIN_DATAPOINTS <- 5
MIN_DATAPOINTS_MSG <- "Filter too narrow: not enough datapoints"
NFOLDS <- 5



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

other_attrs <- list(
  Elevator = 'elevator',
  Balcony = 'balcony',
  View = 'view',
  Garden = 'garden',
  'Swimming Pool' = 'pool',
  Garage = 'garage (box)',
  Parking = 'parking'
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

target_list <- list(
  'Price/m2' = 'price_m2',
  'Area' = 'area',
  'xYield' = 'xYield',
  'Construction Year' = 'construction_year'
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


# ------------------------------- Loading the main Dataset -------------------------------


df <- load_dataset()

# -------------------------------------- GroupKFold --------------------------------------

group_cols <- c("latitude", "longitude", "area", "city_code", "parish_code")
df$group <- as.factor(md5(apply(df[, group_cols], 1, paste, collapse = "/")))
unique_groups <- unique(df$group)
df_groups <- data.frame(
  group=unique_groups,
  Fold=sample(1:NFOLDS, length(unique_groups), replace = TRUE)
) %>% tbl_df()

df <- left_join(df, df_groups, by="group")


dataset <- df


# ------------------------------ MATCH TABLES (ENCODINGS) --------------------------------


match_tables <- list()


target_enc_cols <- list(
  deal.prop_type=c('DealType', 'PropType'),
  deal.district=c('DealType', 'district_code'),
  deal.city=c('DealType', 'city_code'),
  deal.parish=c('DealType', 'parish_code'),
  deal.condition=c('DealType', 'condition')
)

area_enc_cols <- list(
  prop_type='PropType',
  condition='condition'
)

count_enc_cols <- list(
  prop_type='PropType',
  district='district_code',
  city='city_code',
  parish='parish_code',
  condition='condition',
  geo=c('latitude', 'longitude')
)



# ------------------ ENCODINGS W/ FOR EACH FOLD -------------------
for(k in 1:NFOLDS)
{
  k_name <- paste('F', k, sep='')
  match_tables[[k_name]] <- list()
  
  # encodings w/ targets
  for(key in names(target_enc_cols))
  {
    cols <- target_enc_cols[[key]]
    col_name <- paste('target.enc', key, sep='.')
    match_tables[[k_name]][[col_name]] <- df %>%
      filter(Fold != k) %>%
      group_by_at(cols) %>%
      summarize(!!col_name := median(price_m2)) %>%
      na.omit()
  }
  
  # encodings w/ median areas
  for(key in names(area_enc_cols))
  {
    cols <- area_enc_cols[[key]]
    col_name <- paste('area.enc', key, sep='.')
    match_tables[[k_name]][[col_name]] <- df %>%
      filter(Fold != k) %>%
      group_by_at(cols) %>%
      summarize(!!col_name := median(area)) %>%
      na.omit()
  }
  
  # encodings w/ counts
  for(key in names(count_enc_cols))
  {
    cols <- count_enc_cols[[key]]
    col_name <- paste('count.enc', key, sep='.')
    match_tables[[k_name]][[col_name]] <- df %>%
      filter(Fold != k) %>%
      group_by_at(cols) %>%
      summarise(!!col_name := n(price_m2)) %>%
      na.omit()
  }
}

# ------------------ ENCODINGS W/ THE WHOLE DATASET -------------------
match_tables$ALL <- list()

# encodings w/ targets
for(key in names(target_enc_cols))
{
  cols <- target_enc_cols[[key]]
  col_name <- paste('target.enc', key, sep='.')
  match_tables$ALL[[col_name]] <- df %>%
    group_by_at(cols) %>%
    summarize(!!col_name := median(price_m2)) %>%
    na.omit()
}

# encodings w/ areas
for(key in names(area_enc_cols))
{
  cols <- area_enc_cols[[key]]
  col_name <- paste('area.enc', key, sep='.')
  match_tables$ALL[[col_name]] <- df %>%
    group_by_at(cols) %>%
    summarize(!!col_name := median(area)) %>%
    na.omit()
}

# encodings w/ counts
for(key in names(count_enc_cols))
{
  cols <- count_enc_cols[[key]]
  col_name <- paste('count.enc', key, sep='.')
  match_tables$ALL[[col_name]] <- df %>%
    group_by_at(cols) %>%
    summarise(!!col_name := n(price_m2)) %>%
    na.omit()
}


# ------------------------------------- XGBOOST MODEL ------------------------------------

X <- get_features(dataset, match_tables)


cat("Started XGBoost training\n")

reg <- list()

fairobj <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  c <- 2 # the higher the "slower/smoother" the loss is. Cross-Validate.
  x <-  preds-labels
  grad <- c * x / (abs(x) + c)
  hess <- c ^ 2 / (abs(x) + c) ^ 2
  return(list(grad = grad, hess = hess))
}



y <- log(dataset$price)

reg$price <- xgb.train(
  data = xgb.DMatrix(data = as.matrix(X), label = y),
  objective = fairobj,
  eta = 0.08,
  max.depth = 6,
  nround = 100,
  seed = 0,
  nthread = 4,
  verbose = 2
)


filt <- dataset$DealType == "Sale"
filt <- filt & !(dataset$PropType %in% c("Farm", "Terrain"))
X <- dataset[filt, ] %>% mutate(DealType = "Rent") %>% get_features(match_tables)
rent_pred <- exp(predict(reg$price, xgb.DMatrix(data = as.matrix(X))))
dataset[filt, "xYield"] <- 12 * rent_pred / dataset[filt, "price"]

#filt <- dataset$DealType == "Rent"
#X <- dataset[filt, ] %>% mutate(DealType = "Sale") %>% get_features(match_tables)
#sell_pred <- 10^(predict(xgb$price, xgb.DMatrix(data = as.matrix(X))))
#dataset[filt, "xYield"] <- dataset[filt, "price"] / sell_pred

                                                            
rm(X)
rm(y)

cat("Ended XGBoost training\n")


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
library(rapportools) # is.empty
library(formattable)
library(xgboost)
library(openssl)

require(DT)
require(sf)

source("utils.R")
source("mod_valuation.R")


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

prop_types_ids <- c(1, 2, 4, 5, 6, 7, 8, 9, 11)


condition_levels <- c(
  "Ruina",
  "Para Recuperar",
  "Usado",
  "Em Construcao",
  "Renovado",
  "Novo",
  "Projecto"
)

condition_ids <- c(
  0, 1, 2, 5, 3, 4, 6
)


energy_certificate_levels <- c(
  "Isento",
  "G", "F", "E", "D", "C", "B-", "B", "A", "A+"
)

energy_certificate_ids <- c(
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9
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

bedrooms_levels <- 0:10

target_list <- list(
  'Price/m2' = 'price_m2',
  'Area' = 'area',
  'xYield' = 'xYield',
  'Construction Year' = 'Construction Year'
)


decades <- c(-Inf, as.integer(seq(1900, 2020, 10)))
decades_labels <- as.character(decades)
decades_labels[1] <- "< 1900"
decades_labels <- decades_labels[1: length(decades_labels) - 1]


# ------------------------------------ TERRITORY MAPS ------------------------------------


district_sh <- sf::read_sf(file.path("data", "geo", "distritos-shapefile", "distritos.shp"))
district_sh <- rmapshaper::ms_simplify(district_sh ) # polygon simplification to speedup rendering
district_sh  <- district_sh[district_sh$TYPE_1 == "Distrito", ]
district_sh$CCA_1 <- as.factor(district_sh$CCA_1)
district_sh$id <- district_sh$CCA_1
district_sh$name <- district_sh$NAME_1


municipality_sh <- sf::read_sf(file.path("data", "geo", "concelhos-shapefile", "concelhos.shp"))
municipality_sh <- rmapshaper::ms_simplify(municipality_sh)
#municipality_sh <- municipality_sh[municipality_sh$ID_1 %in% district_sh$ID_1, ]
municipality_sh$CCA_1 <- as.factor(stringr::str_sub(municipality_sh$CCA_2, 1, -3))
municipality_sh$CCA_2 <- as.factor(municipality_sh$CCA_2)
municipality_sh$id <- municipality_sh$CCA_2
municipality_sh$name <- municipality_sh$NAME_2

## LOADING PARISH DATA
# https://dados.gov.pt/s/resources/freguesias-de-portugal/20181112-195834/cont-aad-caop2017.zip
parish_sh <- sf::read_sf(file.path("data", "geo", "cont-aad-caop2017", "Cont_AAD_CAOP2017.shp"))
parish_sh <- rmapshaper::ms_simplify(parish_sh)
parish_sh$CCA_1 <- as.factor(stringr::str_sub(parish_sh$Dicofre, 1, -5))
parish_sh$CCA_2 <- as.factor(stringr::str_sub(parish_sh$Dicofre, 1, -3))
parish_sh$CCA_3 <- as.factor(sprintf("%06d", as.integer(parish_sh$Dicofre)))
parish_sh$id <- parish_sh$CCA_3
parish_sh$name <- parish_sh$Freguesia
parish_sh <- sf::st_transform(parish_sh, "+init=epsg:4326")


district_list <- district_sh$CCA_1
names(district_list) <- district_sh$NAME_1


# ------------------------------- Loading the main Dataset -------------------------------

df <- load_dataset()

# -------------------------------------- GroupKFold --------------------------------------

group_cols <- c("latitude", "longitude", "MunicipalityID", "Deal", "Property Type")
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
  deal.prop_type=c('Deal', 'Property Type'),
  deal.district=c('Deal', 'DistrictID'),
  deal.municipality=c('Deal', 'MunicipalityID'),
  deal.parish=c('Deal', 'ParishID'),
  deal.condition=c('Deal', 'Condition')
)

area_enc_cols <- list(
  prop_type='Property Type',
  condition='Condition'
)

count_enc_cols <- list(
  prop_type='Property Type',
  district='DistrictID',
  municipality='MunicipalityID',
  parish='ParishID',
  condition='Condition',
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


filt <- dataset$Deal == "Sale"
filt <- filt & !(dataset$`Property Type` %in% c("Farm", "Terrain"))
X <- dataset[filt, ] %>% mutate(Deal = "Rent") %>% get_features(match_tables)
rent_pred <- exp(predict(reg$price, xgb.DMatrix(data = as.matrix(X))))
dataset[filt, "xYield"] <- 12 * rent_pred / dataset[filt, "price"]

# filt <- dataset$Deal == "Rent"
# X <- dataset[filt, ] %>% mutate(Deal = "Sale") %>% get_features(match_tables)
# sell_pred <- 10^(predict(xgb$price, xgb.DMatrix(data = as.matrix(X))))
# dataset[filt, "xYield"] <- dataset[filt, "price"] / sell_pred

                                                            
rm(X)
rm(y)

cat("Ended XGBoost training\n")



# ----------------------------------------------------------------------------------------
#                                     UTIL FUNCTIONS
# ----------------------------------------------------------------------------------------


qpal <- function(palette, x) {
  if (max(x) == min(x))
  {
    color <- colorQuantile(palette, c(0, 1))(0.5)
    colors <- x
    colors[!is.na(colors)] <- color
  }
  else
  {
    colors <- colorQuantile(palette, unique(x))(x)
  }
  return(colors)
}


target_name <- function(target_col) {
  switch(
    target_col,
    price_m2 = "Price / m2",
    area = "Area",
    price = "Price",
    xYield = "Expected Yield",
    construction_year = "Construction Year"
  )
}


remove_outliers <- function(df, col_name, trunc)
{
  q <- as.numeric(trunc) / 100.0
  quantiles <- quantile(pull(df, col_name), probs=c(q, 1 - q))
  
  df %>% filter(!!between(as.name(col_name), quantiles[1], quantiles[2]))
}


hc_hist <- function(df, col_name, xunits, xlabel = "", truncation = 1)
{
  x <- df[[col_name]]
  x <- x[!is.na(x)]
  
  q <- as.numeric(truncation) / 100.0
  quantiles <- as.numeric(quantile(x, probs=c(q, 1 - q), na.rm = TRUE))
  
  x <- x[x < quantiles[2] * 1.2]
  
  formatter <- sprintf("function() {
                       if (this.series.name == 'ecdf'){
                       return this.y + '%%<br/>' + this.x + ' %s';
                       }
                       else return 'Count: ' + this.y + '<br/>' + this.key;
}", xunits, xunits)
  
  
  g <- hchart(x) %>%
    hc_xAxis(min = quantiles[1], max=quantiles[2]) %>%
    hc_xAxis(title=list(text=xunits)) %>%
    hc_yAxis_multiples(
      list(
        title=list(text="Count")
      ),
      list(
        min=0, max=100,
        tickInterval=25,
        title=list(text="Cumulative percentage"),
        labels = list(format = "{value}%"),
        opposite=TRUE)) %>%
    hc_legend(enabled=FALSE)
  
  if(!is.empty(xlabel))
    g <- g %>% hc_title(text = xlabel)
  
  step <- g$x$hc_opts$series[[1]]$pointRange
  start <- round(quantiles[1] / step) * step
  end <- round(quantiles[2] / step) * step
  nsteps <- as.integer((end - start) / step)
  
  if (TRUE) #((nsteps < 50) & (step > 1))
  {
    step <- step / 2
    nsteps <- 2 * nsteps
  }
  
  u <- start + step * (1 : (nsteps + 1) - 1)
  v <- round(100 * ecdf(x)(u), 2)
  
  g <- g %>%
    hc_add_series(
      data.frame(x = u, y = v),
      #hcaes(name = x),
      type = "line",
      color = "red",
      name = "ecdf",
      yAxis = 1) %>%
    hc_tooltip(useHTML = TRUE, formatter = JS(formatter)) %>%
    hc_plotOptions(
      line = list(marker = list(enabled = FALSE))
    )
  
  return(g)
  }


load_dataset <- function()
{
  fname <- "imovirtual_20191230.csv"
  col_types <- c(
    DealType = "f",
    PropType = "f",
    area = "d",
    gross_area = "d",
    terrain_area = "d",
    district_code = "f",
    city_code = "f",
    parish_code = "f",
    energy_certificate = "f",
    construction_year = "d",
    rooms = "d",
    bathrooms = "d",
    condition = "f",
    elevator = "d",
    balcony = "d",
    view = "d",
    garden = "d",
    pool = "d",
    'garage (box)' = "d",
    parking = "d",
    latitude = "d",
    longitude = "d",
    price = "d"
  )
  
  df <- readr::read_csv(
    file.path("data", "sm", fname),
    col_types = col_types
  ) %>% select(names(col_types))
  
  df$DealType <-  plyr::mapvalues(df$DealType, 0:1, c("Rent", "Sale"))
  df$PropType <-  plyr::mapvalues(df$PropType, prop_types_ids, prop_types)
  
  df$condition <- plyr::mapvalues(
    as.integer(df$condition),
    condition_ids,
    condition_levels
  )
  
  df$construction_year[df$construction_year < 1800] <- NA
  
  # df$energy_certificate <- factor(df$energy_certificate)
  levels(df$energy_certificate) <- energy_certificate_levels

  df <- add_column(df, price_m2 = round(df$price / df$area, 2),                                  .after="price")
  df <- add_column(df, construction_decade = cut(df$construction_year, decades, decades_labels), .after="construction_year")
  df <- add_column(df, district = district_sh$name[match(df$district_code, district_sh$id)],     .after="district_code")
  df <- add_column(df, city     = municipality_sh$name[match(df$city_code, municipality_sh$id)], .after="city_code")
  df <- add_column(df, parish   = parish_sh$name[match(df$parish_code, parish_sh$id)],           .after="parish_code")
  
  df <- df %>% filter(!is.na(price_m2) & (price_m2 > 0))
  
  return(df)
}


get_features <- function(df, match_tables)
{
  df <- df %>%
    mutate(
      DealType = as.factor(DealType),
      PropType = as.factor(PropType),
      district_code = as.factor(district_code),
      city_code = as.factor(city_code),
      parish_code = as.factor(parish_code),
      condition = as.factor(condition)
    )
  
  energy_certificate_ord <- c("G", "F", "E", "D", "C", "B-", "B", "A", "A+")
  df$energy_certificate_ord <- match(df$energy_certificate, energy_certificate_ord)
  
  if(!("Fold" %in% names(df)))
  {
    df$Fold <- NA
  }
  
  # ------------------ APPLYING ENCODINGS TO THE TRAINING SET -------------------
  enc_cols <- names(match_tables$ALL)
  df$oof <- TRUE
  
  for(k in 1:NFOLDS)
  {
    k_name <- paste('F', k, sep='')
    mapping <- match_tables[[k_name]]
    filt <- df$Fold == k
    filt[is.na(filt)] <- FALSE
    df$oof <- df$oof & !filt
    
    for(c in enc_cols)
    {
      curr_map <- mapping[[c]]
      cols <- names(curr_map[, 1:(length(curr_map) - 1)])
      df[filt, c] <- left_join(df[filt, cols], curr_map, by=cols)[[c]]
    }
  }
  
  # ------------------ APPLYING ENCODINGS TO NEW DATA -------------------
  
  for(c in enc_cols)
  {
    curr_map <- match_tables$ALL[[c]]
    cols <- names(curr_map[, 1:(length(curr_map) - 1)])
    df[df$oof, c] <- left_join(df[df$oof, cols], curr_map, by=cols)[[c]]
  }
  
  # ------------- Replace NA's by zeros for Count Encodings -------------

  for(c in c("count.enc.city", "count.enc.parish", "count.enc.geo"))
  {
    df[is.na(df[[c]]), c] <- 0
  }
  
  # Replace NA's by zeros for hierarchical target Encoding
  
  filt <- is.na(df$target.enc.deal.parish)
  df[filt, "target.enc.deal.parish"] <- df[filt, "target.enc.deal.city"]
  
  filt <- is.na(df$target.enc.deal.city)
  df[filt, "target.enc.deal.city"] <- df[filt, "target.enc.deal.district"]
  
  feat_cols <- c(
    "area", "gross_area", "terrain_area",
    "rooms", "bathrooms",
    "construction_year",
    "energy_certificate_ord",
    unlist(other_attrs, use.names=FALSE),
    enc_cols
  )
  
  df <- df[, feat_cols]
  
  return(df)
}

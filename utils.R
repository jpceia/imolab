

# ----------------------------------------------------------------------------------------
#                                     UTIL FUNCTIONS
# ----------------------------------------------------------------------------------------


is.empty <- function (x) 
{
  if (length(x) <= 1) {
    if (is.null(x)) 
      return(TRUE)
    if (length(x) == 0) 
      return(TRUE)
    if (is.na(x) || is.nan(x)) 
      return(TRUE)
    if (is.character(x) && nchar(x) == 0) 
      return(TRUE)
    if (is.logical(x) && !isTRUE(x)) 
      return(TRUE)
    if (is.numeric(x) && x == 0) 
      return(TRUE)
    return(FALSE)
  }
  else sapply(x, is.empty)
}


qpal <- function(palette, x) {
  na_mask <- is.na(x)
  if (all(na_mask))
  {
    color <- colorQuantile(palette, c(0, 1))(0.5)
    colors <- x
    colors[] <- color
  }
  else if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE))
  {
    color <- colorQuantile(palette, c(0, 1))(0.5)
    colors <- x
    colors[!na_mask] <- color
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
    Area = "Area",
    Price = "Price",
    xYield = "Expected Yield",
    `Construction Year` = "Construction Year"
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
    Deal = "f",
    `Property Type` = "f",
    Area = "d",
    `Gross Area` = "d",
    `Terrain Area` = "d",
    DistrictID = "f",
    MunicipalityID = "f",
    ParishID = "f",
    `Energy Certificate` = "f",
    `Construction Year` = "d",
    Bedrooms = "d",
    Bathrooms = "d",
    Condition = "f",
    Elevator = "d",
    Balcony = "d",
    View = "d",
    Garden = "d",
    `Swimming Pool` = "d",
    Garage = "d",
    Parking = "d",
    Latitude = "d",
    Longitude = "d",
    Price = "d"
  )
  
  df <- readr::read_csv(
    file.path("data", "sm", fname),
    col_types = col_types
  ) %>% select(names(col_types))
  
  df$Deal <-  plyr::mapvalues(df$Deal, 0:1, c("Rent", "Sale"))
  df$`Property Type` <-  plyr::mapvalues(
    as.integer(as.character(df$`Property Type`)),
    prop_types_ids,
    prop_types)
  
  df$Condition <- plyr::mapvalues(
    as.integer(as.character(df$Condition)),
    condition_ids,
    condition_levels
  )
  
  year <- df[["Construction Year"]]
  df[!is.na(year) & !between(year, 1800, 2025), "Construction Year"] <- NA
  
  # df$`Energy Certificate` <- factor(df[["Energy Certificate"]])
  df$`Energy Certificate` <- plyr::mapvalues(
    as.integer(as.character(df$`Energy Certificate`)),
    energy_certificate_ids,
    energy_certificate_levels
  )

  df <- add_column(df, price_m2     = round(df$Price / df$Area, 2),                                       .after = "Price")
  df <- add_column(df, `Construction Decade` = cut(df[["Construction Year"]], decades, decades_labels),   .after = "Construction Year")
  df <- add_column(df, District     = district_sh$name[match(df$DistrictID, district_sh$id)],             .after = "DistrictID")
  df <- add_column(df, Municipality = municipality_sh$name[match(df$MunicipalityID, municipality_sh$id)], .after = "MunicipalityID")
  df <- add_column(df, Parish       = parish_sh$name[match(df$ParishID, parish_sh$id)],                   .after = "ParishID")
  
  df <- df %>% filter(!is.na(price_m2) & (price_m2 > 0))
  
  return(df)
}


get_features <- function(df, match_tables)
{
  df <- df %>%
    mutate(
      Deal = as.factor(Deal),
      `Property Type` = as.factor(`Property Type`),
      DistrictID = as.factor(DistrictID),
      MunicipalityID = as.factor(MunicipalityID),
      ParishID = as.factor(ParishID),
      Condition = as.factor(Condition)
    )
  
  energy_certificate_ord <- c("G", "F", "E", "D", "C", "B-", "B", "A", "A+")
  df$energy_certificate_ord <- match(df[["Energy Certificate"]], energy_certificate_ord)
  
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

  for(c in c("count.enc.municipality", "count.enc.parish", "count.enc.geo"))
  {
    df[is.na(df[[c]]), c] <- 0
  }
  
  # Replace NA's by zeros for hierarchical target Encoding
  
  filt <- is.na(df$target.enc.deal.parish)
  df[filt, "target.enc.deal.parish"] <- df[filt, "target.enc.deal.municipality"]
  
  filt <- is.na(df$target.enc.deal.municipality)
  df[filt, "target.enc.deal.municipality"] <- df[filt, "target.enc.deal.district"]
  
  feat_cols <- c(
    "Area", "Gross Area", "Terrain Area",
    "Bedrooms", "Bathrooms",
    "Construction Year",
    "energy_certificate_ord",
    unlist(other_attrs, use.names=FALSE),
    enc_cols
  )
  
  df <- df[, feat_cols]
  
  return(df)
}



# ----------------------------------------------------------------------------------------
#                                     UTIL FUNCTIONS
# ----------------------------------------------------------------------------------------


euro <- scales::dollar_format(prefix = "", suffix = " \u20ac", big.mark = ",", largest_with_cents = 100)
euro_m2 <- scales::dollar_format(prefix = "", suffix = " \u20ac/m\u00b2", big.mark = " ", largest_with_cents = 100)
area_format <- scales::unit_format(unit = "m\u00b2", big.mark = ",")
year_format <- scales::comma_format(big.mark = "")


format_value <- function(type, value) {
  switch(
    tolower(type),
    price = euro,
    price_m2 = euro_m2,
    area = area_format,
    construction.year = year_format
  )(value)
}


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


getCurrentFileLocation <-  function() {
  this_file <- commandArgs() %>%
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  
  if (length(this_file) == 0) {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}



location_type <- function(s) {
  if(is.empty(s))
    return("country")
  
  switch(
    as.character(stringr::str_length(s)),
    '1' = 'district',
    '2' = 'district',
    '3' = 'municipality',
    '4' = 'municipality',
    '5' = 'parish',
    '6' = 'parish',
    'section'
  )
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
    Net.Area = "Net Area",
    Price = "Price",
    Construction.Year = "Construction Year"
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
  conn <- dbConnect(MySQL(), user=DB_USER, password=DB_PWD, host=DB_HOST, dbname=DB_NAME, port=DB_PORT)
  df <- dbReadTable(conn, DB_TABLE)
  dbDisconnect(conn)
  df <- tbl_df(df)
  
  df$DistrictID <- as.factor(sprintf("%02d", as.integer(df$DistrictID)))
  df$MunicipalityID <- as.factor(sprintf("%04d", as.integer(df$MunicipalityID)))
  df$ParishID <- as.factor(sprintf("%06d", as.integer(df$ParishID)))
  
  df$Deal <-  plyr::mapvalues(df$Deal, 0:1, c("Rent", "Sale"))
  df$Property.Type <-  plyr::mapvalues(df$Apartment, 0:1, c("House", "Apartment"))
  df$Condition <- factor(df$Condition, condition_levels)
  df$Energy.Certificate <- factor(df$Energy.Certificate, energy_certificate_levels)

  df <- add_column(df, price_m2     = round(df$Price / df$Net.Area, 2),                                      .after = "Price")
  df <- add_column(df, Construction.Decade = cut(df$Construction.Year, decades, decades_labels),             .after = "Construction.Year")
  df <- add_column(df, District     = district_sh$name[match(df$DistrictID, district_sh$CCA_1)],             .after = "DistrictID")
  df <- add_column(df, Municipality = municipality_sh$name[match(df$MunicipalityID, municipality_sh$CCA_2)], .after = "MunicipalityID")
  df <- add_column(df, Parish       = parish_sh$name[match(df$ParishID, parish_sh$CCA_3)],                   .after = "ParishID")
  
  df <- df %>% filter(!is.na(price_m2) & (price_m2 > 0))
  
  return(df)
}

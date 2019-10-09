

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
  
  #max_row <- 100000
  #if(length(x) > max_row)
  #{
  #  set.seed(0)
  #  x <- sample(x, max_row)
  #}
  
  formatter <- sprintf("function() {
                       if (this.series.name == 'ecdf'){
                       return this.y + '%%<br/>' + this.x + ' %s';
                       }
                       else return 'Count: ' + this.y + '<br/>' + this.key;
}", xunits, xunits)
  
  
  g <- hchart(x) %>%
    #hc_plotOptions(histogram = list(turboThreshold=1000)) %>%
    #hc_boost(boost = TRUE) %>%
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
    hc_tooltip(useHTML = TRUE, formatter = JS(formatter)) %>% #formatter = JS(formatter)
    hc_plotOptions(
      #series = list(turboThreshold = 5000),
      line = list(marker = list(enabled = FALSE))
    )
  
  return(g)
  }


load_dataset <- function() {
  
  fname <- "short_summary_20190921.csv"
  df <- readr::read_csv(file.path("data", fname), col_types=c(
    DealType = "f",
    PropType = "f",
    terrain_area = "d",
    district_code = "f",
    city_code = "f",
    parish_code = "f",
    energy_certificate = "f",
    construction_year = "d",
    rooms = "f",
    bathrooms = "f",
    condition = "f",
    latitude = "d",
    longitude = "d"
  ))
  df <- df %>% select(-createdDays, -modifiedDays)

  
  df$energy_certificate <- factor(
    df$energy_certificate,
    levels=energy_certificate_levels)
  
  df$rooms <- factor(
    df$rooms,
    levels=rooms_levels)
  #levels(df$rooms)[11] <- "10+"
  
  df$bathrooms <- factor(df$bathrooms, levels=bathrooms_levels)
  #levels(df$bathrooms)[4] <- "4+"
  
  df$condition <- factor(df$condition, levels=condition_levels)
  
  df <- add_column(df, price_m2 = round(df$price / df$area, 2), .after="price")
  df <- add_column(df, district = district_meta$Designacao[match(df$district_code, district_meta$Dicofre)], .after="district_code")
  df <- add_column(df, city = city_meta$Designacao[match(df$city_code, city_meta$Dicofre)], .after="city_code")
  df <- add_column(df, parish = parish_meta$Designacao[match(df$parish_code, parish_meta$Dicofre)], .after="parish_code")
  
  df <- df %>% filter(!is.na(price_m2) & (price_m2 > 0))
  
  return(df)
}


get_features <- function(df, match_tables) {
  df <- df %>%
    select(
      DealType, PropType,
      district_code, city_code, parish_code,
      area, gross_area, terrain_area,
      rooms, bathrooms,
      condition, energy_certificate,
      construction_year
    ) %>%
    mutate(
      DealType = as.factor(DealType),
      PropType = as.factor(PropType),
      district_code = as.factor(district_code),
      city_code = as.factor(city_code),
      parish_code = as.factor(parish_code),
      condition = as.factor(condition),
      energy_certificate = as.factor(energy_certificate)
    ) %>%
    left_join(match_tables$mean.enc.cat) %>%
    left_join(match_tables$mean.area.enc.cat) %>%
    left_join(match_tables$mean.enc.cat.district) %>%
    left_join(match_tables$mean.enc.cat.city) %>%
    left_join(match_tables$mean.enc.cat.parish) %>%
    left_join(match_tables$mean.enc.cat.energy_certificate) %>%
    left_join(match_tables$mean.enc.cat.condition) %>%
    mutate(
      rooms = as.integer(rooms),
      bathrooms = as.integer(bathrooms)) %>%
    select(
      area, gross_area, terrain_area,
      rooms, bathrooms,
      construction_year,
      mean.enc.cat,
      mean.area.enc.cat,
      mean.enc.cat.district,
      mean.enc.cat.city,
      mean.enc.cat.parish,
      mean.enc.cat.energy_certificate,
      mean.enc.cat.condition,
    )
  
  filt <- is.na(df$mean.enc.cat.district)
  df$mean.enc.cat.district[filt] <- df$mean.enc.cat[filt]
  
  filt <- is.na(df$mean.enc.cat.city)
  df$mean.enc.cat.city[filt] <- df$mean.enc.cat.district[filt]
  
  filt <- is.na(df$mean.enc.cat.parish)
  df$mean.enc.cat.parish[filt] <- df$mean.enc.cat.city[filt]
  
  filt <- is.na(df$area)
  df$area[filt] <- df$mean.area.enc.cat[filt]
  
  return(df)
}

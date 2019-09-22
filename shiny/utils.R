

# ----------------------------------------------------------------------------------------
#                                     UTIL FUNCTIONS
# ----------------------------------------------------------------------------------------



remove_outliers <- function(df, col_name, trunc)
{
  q <- as.numeric(trunc) / 100.0
  quantiles <- quantile(pull(df, col_name), probs=c(q, 1 - q))
  
  df %>% filter(!!between(as.name(col_name), quantiles[1], quantiles[2]))
}


hc_hist <- function(df, col_name, xunits, xlabel, truncation = 1)
{
  max_row <- 5000
  if(nrow(df) > max_row)
  {
    set.seed(0)
    df <- df[sample(nrow(df), max_row), ]
  }
  q <- as.numeric(truncation) / 100.0
  quantiles <- as.numeric(quantile(df[[col_name]], probs=c(q, 1 - q)))
  formatter <- sprintf("function() {
                       if (this.series.name == 'ecdf'){
                       return this.y + '%%<br/>' + this.x + ' %s';
                       }
                       else return 'Count: ' + this.y + '<br/>' + this.key;
}", xunits, xunits)
  
  g <- hchart(df[[col_name]]) %>%
    hc_plotOptions(histogram = list(turboThreshold=1000)) %>%
    hc_boost(boost = TRUE) %>%
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
    hc_legend(enabled=FALSE) %>%
    hc_title(text = xlabel)
  
  step <- g$x$hc_opts$series[[1]]$pointRange
  start <- round(quantiles[1] / step) * step
  end <- round(quantiles[2] / step) * step
  nsteps <- 2 * as.integer((end - start) / step)
  
  x <- start + (step / 2) * (1 : nsteps - 1)
  y <- round(100 * ecdf(df[[col_name]])(x), 2)
  
  g <- g %>%
    hc_add_series(
      data.frame(x = x, y = y),
      hcaes(name=x),
      type = "line",
      color = "red",
      name = "ecdf",
      yAxis = 1) %>%
    hc_tooltip(useHTML = TRUE, formatter = JS(formatter)) %>% #formatter = JS(formatter)
    hc_plotOptions(
      series = list(turboThreshold = 5000),
      line = list(marker = list(enabled = FALSE))
  
  return(g)
  }


load_dataset <- function() {
  
  fname <- "short_summary_20190826.csv"
  df <- readr::read_csv(file.path("data", fname), col_types=c(
    Sale = "f",
    PropType = "f",
    terrain_area = "d",
    district = "f",
    city = "f",
    freg = "f",
    energy_certificate = "f",
    rooms = "f",
    bathrooms = "f",
    condition = "f"
  ))
  df <- df %>% select(-createdDays, -modifiedDays)

  
  df$energy_certificate <- factor(
    df$energy_certificate,
    levels=energy_certificate_levels)
  
  df$rooms <- factor(
    df$rooms,
    levels=rooms_levels)
  levels(df$rooms)[11] <- "10+"
  
  df$bathrooms <- factor(df$bathrooms, levels=bathrooms_levels)
  levels(df$bathrooms)[4] <- "4+"
  
  df$condition <- factor(df$condition, levels=condition_levels)
  
  df <- add_column(df, price_m2 = round(df$price / df$area, 2), .after="price")
  df <- add_column(df, district_name = district_meta$Designacao[match(df$district, district_meta$Dicofre)], .after="district")
  df <- add_column(df, city_name = city_meta$Designacao[match(df$city, city_meta$Dicofre)], .after="city")
  df <- add_column(df, parish_name = parish_meta$Designacao[match(df$freg, parish_meta$Dicofre)], .after="freg")
  
  df <- df %>% filter(!is.na(price_m2) & (price_m2 > 0))
  
  return(df)
}


get_features <- function(df, match_tables) {
  df <- df %>%
    select(
      Sale, PropType,
      district, city, freg,
      area, gross_area, terrain_area,
      rooms, bathrooms,
      condition, energy_certificate
    ) %>%
    mutate(
      Sale = as.factor(Sale),
      PropType = as.factor(PropType),
      district = as.factor(district),
      city = as.factor(city),
      freg = as.factor(freg),
      condition = as.factor(condition),
      energy_certificate = as.factor(energy_certificate)
    ) %>%
    left_join(match_tables$mean.enc.cat) %>%
    left_join(match_tables$mean.enc.cat.district) %>%
    left_join(match_tables$mean.enc.cat.city) %>%
    left_join(match_tables$mean.enc.cat.parish) %>%
    left_join(match_tables$mean.enc.cat.energy_certificate) %>%
    left_join(match_tables$mean.enc.cat.condition) %>%
    mutate(
      rooms = as.integer(rooms),
      bathrooms = as.integer(bathrooms)) %>%
    select(
      -district, -city, -freg,
      -Sale, -PropType,
      -condition, -energy_certificate)
  return(df)
}

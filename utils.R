

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


load_dataset <- function() {
  
  fname <- "short_summary_20190921.csv"
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
    rooms = "i",
    bathrooms = "i",
    condition = "f",
    latitude = "d",
    longitude = "d",
    price = "d"
  )
  
  df <- readr::read_csv(
    file.path("data", "sm", fname),
    col_types = col_types
  ) %>% select(names(col_types))
  
  
  # df$energy_certificate <- factor(df$energy_certificate)
  levels(df$energy_certificate) <- energy_certificate_levels

  
  # df$condition <- factor(df$condition)
  levels(df$condition) <- condition_levels

  df <- add_column(df, price_m2 = round(df$price / df$area, 2), .after="price")
  df <- add_column(df, construction_decade = cut(df$construction_year, decades, decades_labels), .after="construction_year")
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
    left_join(match_tables$count.enc.cat) %>%
    left_join(match_tables$count.enc.district) %>%
    left_join(match_tables$count.enc.city) %>%
    left_join(match_tables$count.enc.parish) %>%
    left_join(match_tables$count.enc.condition) %>%
    mutate(
      count.enc.cat = replace_na(count.enc.cat, 0),
      count.enc.district = replace_na(count.enc.district, 0),
      count.enc.city = replace_na(count.enc.city, 0),
      count.enc.parish = replace_na(count.enc.parish, 0),
      count.enc.condition = replace_na(count.enc.condition, 0)
      ) %>%
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
      count.enc.cat,
      count.enc.district,
      count.enc.city,
      count.enc.parish,
      count.enc.condition
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

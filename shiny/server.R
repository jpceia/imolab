library(shiny)
library(grid)
library(highcharter)
library(leaflet)
library(ggthemes)
library(Cairo)
options(shiny.usecairo=TRUE)


remove_outliers <- function(df, col_name, trunc)
{
  q <- as.numeric(trunc) / 100.0
  quantiles <- quantile(pull(df, col_name), probs=c(q, 1 - q))

  df %>% filter(!!between(as.name(col_name), quantiles[1], quantiles[2]))
}


hc_hist <- function(df, col_name, xunits, xlabel, truncation = 1)
{
  max_row <- 25000
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
    hc_plotOptions(line=list(marker=list(enabled=FALSE))) %>%
    hc_boost(boost = FALSE)
  
  return(g)
}


# ----------------------------------------------------------------------------------------
#                                         SERVER
# ----------------------------------------------------------------------------------------


shinyServer(function(input, output, session) {
  
  session$onSessionEnded(stopApp)
  
  observe({
    city_list <- c("")
    df <- city_meta() %>% filter(code1 == input$district)
    
    if(nrow(df) > 0) {
      city_list <- df$Dicofre
      names(city_list) <- df$Designacao
    }

    updateSelectInput(session, "city", choices=city_list)
    updateSelectInput(session, "parish", choices=c(""))
  })
  
  
  observe({
    parish_list <- c("")
    df <- parish_meta() %>% filter(code1 == input$city)
    
    if(nrow(df) > 0) {
      parish_list <- df$Dicofre
      names(parish_list) <- df$Designacao
    }
    
    updateSelectInput(session, "parish", choices=parish_list)
  })

  
  city_meta <- reactive({
    read_csv(
      file.path(dataFolder, "geodata", "concelho_meta.csv"),
      col_types = c(Designacao="f"),
      locale = readr::locale(encoding = "latin1")
    ) %>%
      mutate(code1 = stringr::str_sub(as.character(Dicofre), 0, -3)) %>%
      filter(code1 %in% district_meta$Dicofre) %>%
      select(code1, Dicofre, Designacao)
  })

  
  parish_meta <- reactive({
    read_csv(
      file.path(dataFolder, "geodata", "freguesias_meta.csv"),
      col_types = c(Designacao="f"),
      locale = readr::locale(encoding = "latin1")
    ) %>%
      mutate(
        code1 = stringr::str_sub(as.character(Dicofre), 0, -3)) %>%
      filter(code1 %in% city_meta()$Dicofre) %>%
      select(code1, Dicofre, Designacao)
  })
  
    
  dataset <- reactive({
    
    fname <- "short_summary_20190826.csv"
    df <- readr::read_csv(fname, col_types=c(
      terrain_area="d",
      district="f",
      city="f",
      freg="f",
      energy_certificate="f",
      rooms="f",
      bathrooms="f",
      condition="f"
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
    df <- add_column(df, city_name = city_meta()$Designacao[match(df$city, city_meta()$Dicofre)], .after="city")
    df <- add_column(df, parish_name = parish_meta()$Designacao[match(df$freg, parish_meta()$Dicofre)], .after="freg")
    
    return(df)
  })
  
  filtered_dataset_cat <- reactive({
    df <- dataset()
    
    df <- df %>%
      dplyr::filter(PropType %in% input$prop_type) %>%
      dplyr::filter(Sale == input$is_sale) %>%
      dplyr::filter(district %in% district_meta$Dicofre)
    
    # if less then 10 datapoints, do not display data
    
    validate(
      need(nrow(df) > 10, "Not enough datapoints")
    )
    
    return(df)
  })
  
  filtered_dataset <- reactive({
    
    df <- filtered_dataset_cat()
    
    if(input$district != "")
    {
      if(input$city != "")
      {
        if(input$parish != "")
        {
          df <- df %>% dplyr::filter(freg == input$parish)
        }
        else
        {
          df <- df %>% dplyr::filter(city == input$city)
        }
      }
      else
      {
        df <- df %>% dplyr::filter(district == input$district)
      }
    }
    
    # if less then 10 datapoints, do not display data
    
    validate(
      need(nrow(df) > 10, "Not enough datapoints")
    )
    
    return(df)
  })
  
  # ----------------------------------------------------------------------------------------
  #                                     NUMERICAL SECTION
  # ----------------------------------------------------------------------------------------
  
  output$HistogramPrice_m2 <- renderHighchart({
    df <- filtered_dataset()
    hc_hist(df, "price_m2", "EUR/m2", "Price/m2 distribution", input$truncation)
  })
  
  output$tablePrice_m2 <- renderTable({
    df <- filtered_dataset()
    quantiles <- c(.01, .05, .1, .25, .4, .5, .75, .9, .95, .99)
    values <- quantile(pull(df, "price_m2"), probs=quantiles, na.rm=TRUE)
    table <- data.frame("price/m2"=values)
    row.names(table) <- paste(quantiles * 100, "%", sep="")
    return (t(table))
  }, align="c", digits=0)
  
  output$HistogramPrice <- renderHighchart({
    df <- filtered_dataset()
    hc_hist(df, "price", "EUR", "Price distribution", input$truncation)
  })
  
  output$tablePrice <- renderTable({
    df <- filtered_dataset()
    quantiles <- c(.01, .05, .1, .25, .4, .5, .75, .9, .95, .99)
    values <- quantile(pull(df, "price"), probs=quantiles, na.rm=TRUE)
    data.frame(quantile=paste(quantiles * 100, "%", sep=""), "price"=values)
  }, align="c", digits=0)
  
  
  output$HistogramArea <- renderHighchart({
    df <- filtered_dataset()
    hc_hist(df, "area", "m2", "Area distribution", input$truncation)
  })
  
  output$tableArea <- renderTable({
    df <- filtered_dataset()
    quantiles <- c(.01, .05, .1, .25, .4, .5, .75, .9, .95, .99)
    values <- quantile(pull(df, "area"), probs=quantiles, na.rm=TRUE)
    data.frame(quantile=paste(quantiles * 100, "%", sep=""), area=values)
  }, align="c", digits=0)
  
  
  output$ScatterPriceArea <- renderPlot({
    df <- filtered_dataset()
    max_row <- 1000
    if(nrow(df) > max_row)
    {
      set.seed(0)
      df <- df[sample(nrow(df), max_row), ]
    }
    
    q <- as.numeric(input$truncation) / 100.0
    quantiles_area <- quantile(df$area, probs = c(q, 1 - q))
    quantiles_price <- quantile(df$price, probs = c(q, 1 - q))
    
    ggplot(df, aes(x=area, y=price, color=PropType)) +
      geom_jitter(shape=21) + 
      geom_smooth(method=lm, se=TRUE, fullrange=TRUE) +
      scale_x_continuous(limits=quantiles_area) + # trans='log10', 
      scale_y_continuous(limits=quantiles_price) +
      ggtitle("Area distribution") +
      xlab("Area (m2)") +
      ylab("Price (Eur)")
  })


  # ----------------------------------------------------------------------------------------
  #                                    CATEGORIES SECTION
  # ----------------------------------------------------------------------------------------
  
  output$CategoriesBoxPlot <- renderPlot({
    
    # https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
    
    cat_col <- input$category
    
    df <- filtered_dataset()
    df <- df[!is.na(df[[cat_col]]), ]
    
    validate(
      need(nrow(df) > 10, "Filtering too narrow: not enough datapoints")
    )
    
    q <- as.numeric(input$truncation) / 100.0
    quantiles <- quantile(df$price_m2, probs = c(q, 1 - q))
    
    ggplot(df) +
      geom_boxplot(aes_string(x=cat_col, y="price_m2", fill=cat_col), size=.5) +
      scale_x_discrete(drop=FALSE) +
      scale_y_continuous(trans='log10', limits=quantiles) +
      theme(legend.position="none") +
      coord_flip()
  })
  
  output$CategoriesCount <- renderPlot({

    cat_col <- input$category
    
    df <- filtered_dataset()
    df <- df[!is.na(df[[cat_col]]), ]
    
    validate(
      need(nrow(df) > 10, "Filtering too narrow: not enough datapoints")
    )
    
    ggplot(df) +
      geom_bar(aes_string(x=cat_col, fill=cat_col), color="black") +
      scale_x_discrete(drop=FALSE) +
      theme(legend.position="none") +
      coord_flip()
  })
  
  
  output$tableCategories <- renderTable({
    
  })
  
  # ----------------------------------------------------------------------------------------
  #                                   DATA SOURCES SECTION
  # ----------------------------------------------------------------------------------------
  
  output$imovirtualDataTable <- DT::renderDataTable(dataset(), filter = 'top', options = list(scrollX = TRUE))
  
  # ----------------------------------------------------------------------------------------
  #                                     VALUATION SECTION
  # ----------------------------------------------------------------------------------------
  
  output$valuationOutput <- renderHighchart({
    
    highchart() %>%
         hc_chart(type = "waterfall") %>% 
         hc_xAxis(categories = c("area", "location", "condition", "rooms", "energy_certificate")) %>% 
         hc_add_series(c(10,19.4,21.1, 14.4, 6.5), showInLegend = FALSE) 
    
  })
})

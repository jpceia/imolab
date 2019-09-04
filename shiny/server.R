library(shiny)
library(grid)
library(highcharter)
#library(leaflet)


remove_outliers <- function(df, col_name, trunc)
{
  q <- as.numeric(trunc) / 100.0
  quantiles <- quantile(pull(df, col_name), probs=c(q, 1 - q))
  df %>%
    dplyr::filter(!!as.name(col_name) > quantiles[1]) %>%
    dplyr::filter(!!as.name(col_name) < quantiles[2])
}


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
  observe({
    city_list <- c(" ")
    df <- city_meta() %>% filter(code1 == input$district)
    
    if(nrow(df) > 0) {
      city_list <- df$Dicofre
      names(city_list) <- df$Designacao
    }

    updateSelectInput(session, "city", choices=city_list)
    updateSelectInput(session, "parish", choices=c(" "))
  })
  
  
  observe({
    parish_list <- c(" ")
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
      locale = readr::locale(encoding = "latin1")
    ) %>%
      mutate(code1 = stringr::str_sub(as.character(Dicofre), 0, -3)) %>%
      filter(code1 %in% district_meta$Dicofre) %>%
      select(code1, Dicofre, Designacao)
  })

  
  parish_meta <- reactive({
    read_csv(
      file.path(dataFolder, "geodata", "freguesias_meta.csv"),
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
      district="f",
      city="f",
      freg="f",
      terrain_area="d",
      energy_certificate="f",
      rooms="f",
      bathrooms="f",
      condition="f"
      ))
    df$energy_certificate <- factor(df$energy_certificate, levels=energy_certificate_levels)
    df$rooms <- factor(df$rooms, levels=rooms_levels)
    levels(df$rooms)[11] <- "10+"
    df$bathrooms <- factor(df$bathrooms, levels=bathrooms_levels)
    levels(df$bathrooms)[4] <- "4+"
    df$condition <- factor(df$condition, levels=condition_levels)
    df$price_m2 <- df$price / df$area
    return(df)
  })
  
  filtered_dataset <- reactive({
    df <- dataset()
    
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
    
    df <- df %>%
      dplyr::filter(PropType %in% input$prop_type) %>%
      dplyr::filter(Sale == input$is_sale)
    
    # if less then 10 datapoints, do not display data
    
    validate(
      need(nrow(df) > 10, "Not enough datapoints")
    )
    
    return(df)
  })
  
  
  output$HistogramPrice_m2 <- renderPlot({
    df <- filtered_dataset()
    
    q <- as.numeric(input$truncation) / 100.0
    quantiles <- quantile(df$price_m2, probs = c(q, 1 - q))
    
    g1 <- ggplot(df, aes(x=price_m2)) +
      geom_histogram(bins=input$granularity) +
      scale_x_continuous(trans='log10', limits=quantiles) +
      ggtitle('Price/m2 distribution') +
      xlab('Price/Area (€/m2)')
    
    g2 <- ggplot(df, aes(x=price_m2)) +
      stat_ecdf(color="red", size=1.5) + 
      scale_x_continuous(trans='log10', limits=quantiles) +
      ggtitle('Price/m2 distribution') +
      xlab('Price/Area (€/m2)')
    
    grid.newpage()
    grid.draw(rbind(ggplotGrob(g1), ggplotGrob(g2), size="last"))
  })
  
  output$tablePrice_m2 <- renderTable({
    df <- filtered_dataset()
    quantiles <- c(.01, .05, .1, .25, .4, .5, .75, .9, .95, .99)
    values <- quantile(pull(df, "price_m2"), probs=quantiles, na.rm=TRUE)
    data.frame(quantile=paste(quantiles * 100, "%", sep=""), "price/m2"=values)
  }, align="c", digits=0)
  
  output$HistogramPrice <- renderPlot({
    df <- filtered_dataset()
    q <- as.numeric(input$truncation) / 100.0
    quantiles <- quantile(df$price, probs = c(q, 1 - q))
    ggplot(df, aes(x=price)) +
      geom_histogram(aes(y=..density..), bins=input$granularity) +
      scale_x_continuous(trans='log10', limits=quantiles) +
      ggtitle("Price distribution") +
      xlab("Price (€)")
  })
  
  output$tablePrice <- renderTable({
    df <- filtered_dataset()
    quantiles <- c(.01, .05, .1, .25, .4, .5, .75, .9, .95, .99)
    values <- quantile(pull(df, "price"), probs=quantiles, na.rm=TRUE)
    data.frame(quantile=paste(quantiles * 100, "%", sep=""), "price"=values)
  }, align="c", digits=0)
  
  output$HistogramArea <- renderPlot({
    df <- filtered_dataset()
    q <- as.numeric(input$truncation) / 100.0
    quantiles <- quantile(df$area, probs = c(q, 1 - q))
    ggplot(df, aes(x=area)) +
      geom_histogram(bins=input$granularity) +
      scale_x_continuous(trans='log10', limits=quantiles) +
      ggtitle("Area distribution") +
      xlab("Area (m2)")
  })
  
  
  output$tableArea <- renderTable({
    df <- filtered_dataset()
    quantiles <- c(.01, .05, .1, .25, .4, .5, .75, .9, .95, .99)
    values <- quantile(pull(df, "area"), probs=quantiles, na.rm=TRUE)
    data.frame(quantile=paste(quantiles * 100, "%", sep=""), area=values)
  }, align="c", digits=0)
  
  output$ScatterPriceArea <- renderPlot({
    df <- filtered_dataset()
    q <- as.numeric(input$truncation) / 100.0
    quantiles_area <- quantile(df$area, probs = c(q, 1 - q))
    quantiles_price <- quantile(df$price, probs = c(q, 1 - q))
    ggplot(df, aes(x=area, y=price)) +
      geom_bin2d(bins=20) + 
      geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color="red") +
      scale_x_continuous(limits=quantiles_area) + # trans='log10', 
      scale_y_continuous(limits=quantiles_price) +
      ggtitle("Area distribution") +
      xlab("Area (m2)") +
      ylab("Price (Eur)")
  })
  
  output$ParetoPrice <- renderPlot({
  })
  
  output$ParetoArea <- renderPlot({
  })
  
  output$CategoriesBoxPlot <- renderPlot({
    
    # https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
    
    cat_col <- input$category
    #target_col <- input$target
    
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
      #geom_bar(aes(x=energy_certificate), stat="count", width=0.7, size=1) + 
      theme(legend.position="bottom") +
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
      geom_bar(aes_string(x=cat_col, fill=cat_col)) +
      #geom_bar(aes(x=energy_certificate), , width=0.7, size=1) + 
      scale_x_discrete(drop=FALSE) +
      theme(legend.position="bottom") +
      coord_flip()
    
  })
  
  
  output$tableCategoriesEnergyCertificate <- renderTable({
    
  })
  
  output$imovirtualDataTable<- DT::renderDataTable(dataset(), filter = 'top', options = list(scrollX = TRUE))
})

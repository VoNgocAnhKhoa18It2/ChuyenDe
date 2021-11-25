library(shinydashboard)
library(shiny)

server <- function(input, output) {
  output$region <- renderPlotly({
    plot_ly(
      filter12,
      labels = ~ region,
      values = ~ Number,
      type = 'pie'
    )
  })
  
  output$worker <- renderPlot({
    corrplot(
      data %>% select(-region,-X,-id,-status,-varieties,-bimas) %>% cor(method = "pearson"),
      order = 'AOE'
    )
  })
  
  # Table
  output$Rtable <- renderDataTable({
    region <- data
    
    if (input$sBimas != "--- All ---") {
      region <- region %>% filter(bimas == print(input$sBimas))
    }
    
    if (input$sVarieties != "--- All ---") {
      region <- region %>% filter(varieties == print(input$sVarieties))
    }
    
    if (input$sStatus != "--- All ---") {
      region <- region %>% filter(status == print(input$sStatus))
    }
    
    if (input$sRegion != "--- All ---") {
      region <- region %>% filter(region == print(input$sRegion))
    }
    
    if (input$sSort != "--- All ---") {
      region <- region %>% arrange(desc(.data[[input$sSort]]))
    }
    
    if (input$sTop != 0) {
      region <- region %>% head(input$sTop)
    }
    region
  }, options = list(scrollX = TRUE))
  
  #Graphic
  output$gVarieties <- renderPlotly(if (input$gRegion == "--- All ---") {
    if (input$sgVarieties == "--- All ---") {
      plot_ly(
        filter17,
        labels = ~ region,
        values = ~ varieties_sum,
        type = 'pie'
      )
    } else {
      a <- filter16 %>% filter(varieties == input$sgVarieties)
      plot_ly(
        a,
        labels = ~ region,
        values = ~ Number,
        type = 'pie'
      )
    }
  } else {
    a <- filter16 %>% filter(region == input$gRegion)
    plot_ly(
      a,
      labels = ~ varieties,
      values = ~ Number,
      type = 'pie'
    )
  })
  
  output$ghVarieties <- renderPlotly(if (input$gRegion == "--- All ---") {
    if (input$sgVarieties == "--- All ---") {
      plot_ly(
        filter17,
        x = ~ region,
        y = ~ trad,
        name = 'Trad',
        marker =  list(color = 'rgb(255,215,0)'),
        type = 'bar'
      ) %>%
        add_trace(
          y = ~ high,
          name = 'High',
          marker =  list(color = 'rgb(192,192,192)')
        ) %>%
        add_trace(
          y = ~ mixed,
          name = 'Mixed',
          marker =  list(color = 'rgb(205,127,50)')
        ) %>%
        layout(
          yaxis = list(title = 'Varieties'),
          xaxis = list(title = 'Region'),
          barmode = 'stack'
        )
    } else {
      a <- filter16 %>% filter(varieties == input$sgVarieties)
      plot_ly(
        a,
        x = ~ region,
        y = ~ Number,
        name = input$sgVarieties,
        marker =  list(color = 'rgb(255,215,0)'),
        type = 'bar'
      ) %>%
        layout(
          yaxis = list(title = input$sgVarieties),
          xaxis = list(title = 'Region'),
          barmode = 'stack'
        )
    }
  } else {
    a <- filter16 %>% filter(region == input$gRegion)
    plot_ly(
      a,
      x = ~ varieties,
      y = ~ Number,
      name = input$gRegion,
      marker =  list(color = 'rgb(255,215,0)'),
      type = 'bar'
    ) %>%
      layout(
        yaxis = list(title = "Varieties"),
        xaxis = list(title = input$gRegion),
        barmode = 'stack'
      )
  })
  
  output$dDetail <- renderPlot({
    ggplot(filter17, aes(x = region, y = filter17[,input$dSDetail])) + 
      geom_col(aes(fill = region), color = "black") +
      theme(axis.text.x = element_text(angle = 0)) +
      xlab("Region") + ylab(input$dSDetail)
  })
  
  output$edDetail <- renderPlot({
    
    ggplot(data,aes(x = data[,input$eSDetail],y = goutput, 
                    color = region)) + 
      facet_grid(~region)+geom_point() + 
      geom_smooth(method = "lm") + 
      labs(x = input$eSDetail,y = "goutput")
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}
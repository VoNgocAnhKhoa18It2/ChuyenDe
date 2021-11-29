library(shinydashboard)
library(shiny)
library(plotly)
library(dplyr)
library(sqldf)
library(corrplot)
library(BMA)
library(tidyverse)
library(knitr)
library(forecast)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("tachometer-alt")
      ),
      menuItem("Region", tabName = "Region", icon = icon("globe-asia"))
    )
  ),
  dashboardBody(style = "background-color: white;",
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBox(length(unique(data$id)), "Nông trại", icon = icon("tractor")),
          valueBox(
            length(filter17$region),
            "Vùng",
            icon = icon("globe-asia"),
            color = "purple"
          ),
          valueBox(
            sum(filter17$size_sum),
            "Diện Tích",
            icon = icon("chart-area"),
            color = "yellow"
          ),
          valueBox(
            sum(data$seed),
            "Giống",
            icon = icon("seedling"),
            color = "orange"
          ),
          valueBox(
            sum(filter17$totlabor_sum),
            "Công Nhân",
            icon = icon("users"),
            color = "navy"
          ),
          valueBox(
            sum(filter17$goutput_sum),
            "Gạo",
            icon = icon("product-hunt"),
            color = "green"
          ),
          column(6,style = "padding-right: 0px;",
            plotOutput("worker", height = 500)
          ),
          column(6,style = "padding-left: 0px;",
            plotlyOutput("region", height = 500)
          ),
        ),
      ),
      
      # Second tab content
      tabItem(
        tabName = "Region",
        fluidRow(
          tabBox(
            width = 12,
            id = "tabset1",
            tabPanel(
              "Table",
              fluidRow(
                box(
                  title = "Inputs",
                  solidHeader = TRUE,
                  status = "primary",
                  width = 3,
                  selectInput(
                    "sRegion",
                    "Region",
                    c("--- All ---", filter17$region),
                    selected = "--- All ---"
                  ),
                  selectInput(
                    "sStatus", 
                    "Status", 
                    c("--- All ---", levels(factor(data$status))), 
                    selected = "--- All ---"
                  ),
                  selectInput(
                    "sVarieties",
                    "Varieties",
                    c("--- All ---", levels(factor(data$varieties))),
                    selected = "--- All ---"
                  ),
                  selectInput(
                    "sBimas", 
                    "Bimas", 
                    c("--- All ---", levels(factor(data$bimas))), 
                    selected = "--- All ---"),
                  sliderInput(
                    "sPrice", 
                    "Price",
                    as.numeric(levels(factor(data$price))[1]), 
                    as.numeric(tail(levels(factor(data$price)), 1)),
                    c(
                      as.numeric(levels(factor(data$price))[1]),
                      as.numeric(tail(levels(factor(data$price)), 1))
                    )
                  ),
                  selectInput(
                    "sSort", 
                    "Sort", 
                    c("--- All ---", unique(names(data))),
                    selected = "--- All ---"
                  ),
                  sliderInput("sTop", "Top", 0, 25, 0)
                ),
                box(width = 9,
                    dataTableOutput("Rtable"))
              ),
            ),
            tabPanel(
              "Graphic",
              fluidRow(
                box(
                  width = 12,
                  title = "Varieties",
                  solidHeader = TRUE,
                  status = "primary",
                  fluidRow(
                    column(
                      width = 2,
                      selectInput(
                        "gRegion",
                        "Region",
                        c("--- All ---", filter17$region),
                        selected = "--- All ---"
                      ),
                      selectInput(
                        "sgVarieties",
                        "Varieties",
                        c("--- All ---", levels(factor(data$varieties))),
                        selected = "--- All ---"
                      ),
                    ),
                    column(
                      width = 10,
                      fluidRow(
                        box(title = "Biểu đồ tỷ lệ",
                            solidHeader = TRUE,
                            status = "info",
                            plotlyOutput("gVarieties")),
                        box(
                          title = "Biểu đồ Histogram",
                          solidHeader = TRUE,
                          status = "warning",
                          plotlyOutput("ghVarieties")),
                        
                      )
                    )   
                  )
                ),
                box(
                  width = 12,
                  title = "Detail",
                  solidHeader = TRUE,
                  status = "warning",
                  fluidRow(
                    column(
                      width = 2,
                      selectInput(
                        "dSDetail",
                        "Detail",
                        choices = names(filter18)[-1],
                      ),
                    ),
                    column(
                      width = 10,
                      plotOutput("dDetail")
                    )   
                  )
                ),
                box(
                  width = 12,
                  title = "Expected",
                  solidHeader = TRUE,
                  status = "success",
                  fluidRow(
                    column(
                      width = 2,
                      selectInput(
                        "eSDetail",
                        "Expected",
                        choices = names(data)[3:9],
                      ),
                    ),
                    column(
                      width = 10,
                      fluidRow(
                        plotOutput("edDetail")
                      )
                    )   
                  )
                ),
              ),
            )
          ),
        ),
      )
    )
  )
)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  PHA MAX BI Tool
# Purpose:      Shiny server
# programmer:   Zhe Liu
# Date:         28-06-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##---- load packages ----
require(DT)
require(reshape2)
require(plyr)
require(data.table)
library(shiny)
library(stringi)
library(dplyr)
library(plotly)
library(tidyr)
library(lubridate)
library(openxlsx)
library(shinydashboard)
library(rlang)
library(shinyjs)
library(webshot)
library(leaflet)
library(leafletCN)
library(shinyWidgets)

##---- load functions ----
source("./functions/functions.R", encoding = "UTF-8")

##---- options ----
options(shiny.maxRequestSize = 1000 * 1024 ^ 2,
        stringsAsFactors = FALSE)

##---- server ----
server <- function(input, output, session) {
  
  raw <- reactive({
    if (is.null(input$raw))
      return(NULL)
    
    inFile.raw <- input$raw
    raw <- fread(
      inFile.raw$datapath,
      na.strings = "NA",
      stringsAsFactors = FALSE
    ) %>% 
      setDF() %>% 
      mutate(Period_Code = gsub("M", "/", Period_Code))
    
    raw
  })
  
  mapping <- reactive({
    if (is.null(input$mapping))
      return(NULL)
    
    inFile.raw <- input$mapping
    mapping <- fread(
      inFile.raw$datapath,
      na.strings = "NA",
      stringsAsFactors = FALSE
    ) %>% 
      setDF()
    
    mapping
  })
  
  ## year-month
  ym.list <- reactive({
    data <- raw()
    ym <- data$Period_Code[!duplicated(data$Period_Code)]
    ym <- sort(ym)
    
    ym
  })
  
  observeEvent(input$raw, {
    updateSelectInput(session,
                      inputId = "ym",
                      choices = ym.list(),
                      selected = ym.list()[1])
  })
  
  ## market
  market.list <- reactive({
    data <- raw()
    data <- data[data$Period_Code %in% input$ym, ]
    market <- data$Market[!duplicated(data$Market)]
    market <- sort(market)
    
    market
  })
  
  observeEvent(c(input$ym), {
    updateSelectInput(session,
                      inputId = "market",
                      choices = market.list(),
                      selected = market.list()[1])
  })
  
  ## atc2
  atc2.list <- reactive({
    data <- raw()
    data <- data[data$Period_Code %in% input$ym, ]
    data <- data[data$Market %in% input$market, ]
    
    atc2 <- data$ATC2[!duplicated(data$ATC2)]
    atc2 <- sort(atc2)
    atc2 <- c("ALL", atc2)
    
    atc2
  })
  
  observeEvent(c(input$ym, input$market), {
    updateSelectInput(session,
                      inputId = "atc2",
                      choices = atc2.list(),
                      selected = atc2.list()[1])
  })
  
  ## atc3
  atc3.list <- reactive({
    data <- raw()
    data <- data[data$Period_Code %in% input$ym, ]
    data <- data[data$Market %in% input$market, ]
    if ("ALL" %in% input$atc2) {
      data <- data
    } else {
      data <- data[data$ATC2 %in% input$atc2, ]
    }
    
    atc3 <- data$ATC3[!duplicated(data$ATC2)]
    atc3 <- sort(atc3)
    atc3 <- c("ALL", atc3)
    
    atc3
  })
  
  observeEvent(c(input$ym, input$market, input$atc2), {
    updateSelectInput(session,
                      inputId = "atc3",
                      choices = atc3.list(),
                      selected = atc3.list()[1])
  })
  
  ## region
  region.list <- reactive({
    data <- raw()
    data <- data[data$Period_Code %in% input$ym, ]
    data <- data[data$Market %in% input$market, ]
    if ("ALL" %in% input$atc2) {
      data <- data
    } else {
      data <- data[data$ATC2 %in% input$atc2, ]
    }
    if ("ALL" %in% input$atc3) {
      data <- data
    } else {
      data <- data[data$ATC3 %in% input$atc3, ]
    }
    
    region <- data$Region[!duplicated(data$Region)]
    region <- sort(region)
    region <- c("National", region)
    
    region
  })
  
  observeEvent(c(input$ym, input$market, input$atc2, input$atc3), {
    updateSelectInput(session,
                      inputId = "region",
                      choices = region.list(),
                      selected = region.list()[1])
  })
  
  ## province
  province.list <- reactive({
    data <- raw()
    data <- data[data$Period_Code %in% input$ym, ]
    data <- data[data$Market %in% input$market, ]
    if ("ALL" %in% input$atc2) {
      data <- data
    } else {
      data <- data[data$ATC2 %in% input$atc2, ]
    }
    if ("ALL" %in% input$atc3) {
      data <- data
    } else {
      data <- data[data$ATC3 %in% input$atc3, ]
    }
    if ("National" %in% input$region) {
      data <- data
    } else {
      data <- data[data$Region %in% input$region, ]
    }
    
    province <- data$Province[!duplicated(data$Province)]
    province <- sort(province)
    province <- c("National", province)
    
    province
  })
  
  observeEvent(c(input$ym, input$market, input$atc2, input$atc3, input$region), {
    updateSelectInput(session,
                      inputId = "province",
                      choices = province.list(),
                      selected = province.list()[1])
  })
  
  ## city
  city.list <- reactive({
    data <- raw()
    data <- data[data$Period_Code %in% input$ym, ]
    data <- data[data$Market %in% input$market, ]
    if ("ALL" %in% input$atc2) {
      data <- data
    } else {
      data <- data[data$ATC2 %in% input$atc2, ]
    }
    if ("ALL" %in% input$atc3) {
      data <- data
    } else {
      data <- data[data$ATC3 %in% input$atc3, ]
    }
    if ("National" %in% input$region) {
      data <- data
    } else {
      data <- data[data$Region %in% input$region, ]
    }
    if ("National" %in% input$province) {
      data <- data
    } else {
      data <- data[data$Province %in% input$province, ]
    }
    
    city <- data$City[!duplicated(data$City)]
    city <- sort(city)
    city <- c("National", city)
    
    city
  })
  
  observeEvent(c(input$ym, input$market, input$atc2, input$atc3, input$region, input$province),{
    updateSelectInput(session,
                      inputId = "city",
                      choices = city.list(),
                      selected = city.list()[1])
  })
  
  ## channel
  channel.list <- reactive({
    data <- raw()
    data <- data[data$Period_Code %in% input$ym, ]
    data <- data[data$Market %in% input$market, ]
    if ("ALL" %in% input$atc2) {
      data <- data
    } else {
      data <- data[data$ATC2 %in% input$atc2, ]
    }
    if ("ALL" %in% input$atc3) {
      data <- data
    } else {
      data <- data[data$ATC3 %in% input$atc3, ]
    }
    if ("National" %in% input$region) {
      data <- data
    } else {
      data <- data[data$Region %in% input$region, ]
    }
    if ("National" %in% input$province) {
      data <- data
    } else {
      data <- data[data$Province %in% input$province, ]
    }
    if ("National" %in% input$city) {
      data <- data
    } else {
      data <- data[data$City %in% input$city, ]
    }
    
    channel <- data$Channel[!duplicated(data$Channel)]
    channel <- sort(channel)
    channel <- c("National", channel)
    
    channel
  })
  
  observeEvent(c(input$ym, input$market, input$atc2, input$atc3, input$region, input$province, input$city), {
    updateSelectInput(session,
                      inputId = "channel",
                      choices = channel.list(),
                      selected = channel.list()[1])
  })
  
  ## molecule
  molecule.list <- reactive({
    data <- raw()
    data <- data[data$Period_Code %in% input$ym, ]
    data <- data[data$Market %in% input$market, ]
    if ("ALL" %in% input$atc2) {
      data <- data
    } else {
      data <- data[data$ATC2 %in% input$atc2, ]
    }
    if ("ALL" %in% input$atc3) {
      data <- data
    } else {
      data <- data[data$ATC3 %in% input$atc3, ]
    }
    if ("National" %in% input$region) {
      data <- data
    } else {
      data <- data[data$Region %in% input$region, ]
    }
    if ("National" %in% input$province) {
      data <- data
    } else {
      data <- data[data$Province %in% input$province, ]
    }
    if ("National" %in% input$city) {
      data <- data
    } else {
      data <- data[data$City %in% input$city, ]
    }
    if ("National" %in% input$channel) {
      data <- data
    } else {
      data <- data[data$Channel %in% input$channel, ]
    }
    
    molecule <- data$`Molecule_CN`[!duplicated(data$`Molecule_CN`)]
    molecule <- sort(molecule)
    molecule <- c("National", molecule)
    
    molecule
  })
  
  observeEvent(c(input$ym, input$market, input$atc2, input$atc3, input$region, input$province, input$city, input$channel), {
    updateSelectInput(session,
                      inputId = "molecule",
                      choices = molecule.list(),
                      selected = molecule.list()[1])
  })
  
  ##---- by brand ----
  brandData <- reactive({
    data <- work_out(
      data = raw(),
      input_metric = input$period,
      current_time_point = input$ym,
      input_period = 12,
      input_market = input$market,
      input_tc_group = input$tc,
      input_measure = input$measure,
      input_index = input$index,
      input_region = input$region,
      input_province = input$province,
      input_city = input$city,
      input_channel = input$channel,
      measure_list = input$measure,
      output_type = "brand group"
    )
    
  })
  
  ##---- table1 ----
  
  
  
  
  
  
  output$table1 <- renderTable({
    
  })
  
  
  ##---- plot1 ----
  plot1 <- reactive({
    
  })
  
  
  
}








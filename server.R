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
library(stringr)
library(dplyr)
library(plotly)
library(tidyr)
library(lubridate)
library(purrr)
library(readxl)
library(RcppRoll)
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
load("./data/mappingData.RData")

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
      mutate(Period_Code = gsub("M", "", Period_Code))
    colnames(raw) <- tolower(colnames(raw))
    
    raw
  })
  
  mapping <- reactive({
    if (is.null(input$mapping))
      return(NULL)
    
    inFile.raw <- input$mapping
    mapping <- read_excel(
      inFile.raw$datapath,
      sheet = 1,
      na = "NA"
    ) %>% 
      setDF()
    colnames(mapping) <- tolower(colnames(mapping))
    
    mapping
  })
  
  
  ##---- input ----
  ## year-month
  ym.list <- reactive({
    data <- raw()
    ym <- data$period_code[!duplicated(data$period_code)]
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
    data <- data[data$period_code %in% input$ym, ]
    market <- data$market[!duplicated(data$market)]
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
    data <- data[data$period_code %in% input$ym, ]
    data <- data[data$market %in% input$market, ]
    
    atc2 <- data$atc2[!duplicated(data$atc2)]
    atc2 <- sort(atc2)
    atc2 <- c("All", atc2)
    
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
    data <- data[data$period_code %in% input$ym, ]
    data <- data[data$market %in% input$market, ]
    if ("All" %in% input$atc2) {
      data <- data
    } else {
      data <- data[data$atc2 %in% input$atc2, ]
    }
    
    atc3 <- data$atc3[!duplicated(data$atc2)]
    atc3 <- sort(atc3)
    atc3 <- c("All", atc3)
    
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
    data <- data[data$period_code %in% input$ym, ]
    data <- data[data$market %in% input$market, ]
    if ("All" %in% input$atc2) {
      data <- data
    } else {
      data <- data[data$atc2 %in% input$atc2, ]
    }
    if ("All" %in% input$atc3) {
      data <- data
    } else {
      data <- data[data$atc3 %in% input$atc3, ]
    }
    
    region <- data$region[!duplicated(data$region)]
    region <- sort(region)
    region <- c("All", region)
    
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
    data <- data[data$period_code %in% input$ym, ]
    data <- data[data$market %in% input$market, ]
    if ("All" %in% input$atc2) {
      data <- data
    } else {
      data <- data[data$atc2 %in% input$atc2, ]
    }
    if ("All" %in% input$atc3) {
      data <- data
    } else {
      data <- data[data$atc3 %in% input$atc3, ]
    }
    if ("All" %in% input$region) {
      data <- data
    } else {
      data <- data[data$region %in% input$region, ]
    }
    
    province <- data$province[!duplicated(data$province)]
    province <- sort(province)
    province <- c("All", province)
    
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
    data <- data[data$period_code %in% input$ym, ]
    data <- data[data$market %in% input$market, ]
    if ("All" %in% input$atc2) {
      data <- data
    } else {
      data <- data[data$atc2 %in% input$atc2, ]
    }
    if ("All" %in% input$atc3) {
      data <- data
    } else {
      data <- data[data$atc3 %in% input$atc3, ]
    }
    if ("All" %in% input$region) {
      data <- data
    } else {
      data <- data[data$region %in% input$region, ]
    }
    if ("All" %in% input$province) {
      data <- data
    } else {
      data <- data[data$province %in% input$province, ]
    }
    
    city <- data$city[!duplicated(data$city)]
    city <- sort(city)
    city <- c("All", city)
    
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
    data <- data[data$period_code %in% input$ym, ]
    data <- data[data$market %in% input$market, ]
    if ("All" %in% input$atc2) {
      data <- data
    } else {
      data <- data[data$atc2 %in% input$atc2, ]
    }
    if ("All" %in% input$atc3) {
      data <- data
    } else {
      data <- data[data$atc3 %in% input$atc3, ]
    }
    if ("All" %in% input$region) {
      data <- data
    } else {
      data <- data[data$region %in% input$region, ]
    }
    if ("All" %in% input$province) {
      data <- data
    } else {
      data <- data[data$province %in% input$province, ]
    }
    if ("All" %in% input$city) {
      data <- data
    } else {
      data <- data[data$city %in% input$city, ]
    }
    
    channel <- data$channel[!duplicated(data$channel)]
    channel <- sort(channel)
    channel <- c("All", channel)
    
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
    data <- data[data$period_code %in% input$ym, ]
    data <- data[data$market %in% input$market, ]
    if ("All" %in% input$atc2) {
      data <- data
    } else {
      data <- data[data$atc2 %in% input$atc2, ]
    }
    if ("All" %in% input$atc3) {
      data <- data
    } else {
      data <- data[data$atc3 %in% input$atc3, ]
    }
    if ("All" %in% input$region) {
      data <- data
    } else {
      data <- data[data$region %in% input$region, ]
    }
    if ("All" %in% input$province) {
      data <- data
    } else {
      data <- data[data$province %in% input$province, ]
    }
    if ("All" %in% input$city) {
      data <- data
    } else {
      data <- data[data$city %in% input$city, ]
    }
    if ("All" %in% input$channel) {
      data <- data
    } else {
      data <- data[data$channel %in% input$channel, ]
    }
    
    molecule <- data$molecule_cn[!duplicated(data$molecule_cn)]
    molecule <- sort(molecule)
    molecule <- c("All", molecule)
    
    molecule
  })
  
  observeEvent(c(input$ym, input$market, input$atc2, input$atc3, input$region, input$province, input$city, input$channel), {
    updateSelectInput(session,
                      inputId = "molecule",
                      choices = molecule.list(),
                      selected = molecule.list()[1])
  })
  
  ##---- by group ----
  groupData <- reactive({
    if (is.null(raw()) | is.null(mapping()))
      return(NULL)
    
    data <- work_out(
      data = raw(),
      mapping_data = mapping(),
      input_metric = input$metric,
      current_time_point = input$ym,
      input_period = as.numeric(input$period),
      input_market = input$market,
      input_atc2 = input$atc2,
      input_atc3 = input$atc3,
      input_measure = input$measure,
      input_region = input$region,
      input_province = input$province,
      input_city = input$city,
      input_channel = input$channel,
      input_molecule = input$molecule,
      output_type = "by group",
      mapping_table_list = en_cn
    )
    
    data
  })
  
  ## table1
  table1 <- reactive({
    if (is.null(groupData()))
      return(NULL)
    
    data <- groupData() %>% 
      filter(date == input$ym) %>% 
      select(`Display Name En`, `Display Name Cn`, `Sales(Mn)`, `Growth%`, `Share%`, `share_delta%`, `EI`)
    
    table1 <- datatable(
      data
    )
    
    table1
  })
  
  output$table1 <- renderDataTable({
    table1()
  })
  
  
  ## plot1
  plot1 <- reactive({
    if (is.null(groupData()))
      return(NULL)
    
    period_date <- sort(unique(substr(gsub("-", "", ymd(paste0(input$ym, "01")) - months(0:(as.numeric(input$period) - 1))), 1, 6)))
    
    data <- groupData() %>% 
      filter(date == period_date, `Display Name En` != "Market") %>% 
      select(date, `Display Name En`, input$index) %>% 
      arrange(`Display Name En`, date)
    colnames(data) <- c("date", "key", "value")
    
    key <- unique(data$key)

    plot1 <- plot_ly(hoverinfo = "name + x + text")

    for (i in key) {
      plot1 <- plot1 %>%
        add_trace(x = data$date[data$key == i],
                  y = data$value[data$key == i],
                  type = "scatter",
                  mode = "lines",
                  name = i)
    }
    
    plot1
  })
  
  output$plot1 <- renderPlotly({
    plot1()
  })
  
  ## plot2
  plot2 <- reactive({
    if (is.null(groupData()))
      return(NULL)
    
    period_date <- sort(unique(substr(gsub("-", "", ymd(paste0(input$ym, "01")) - months(0:(as.numeric(input$period) - 1))), 1, 6)))
    
    data <- groupData() %>% 
      filter(date == period_date, `Display Name En` != "Market") %>% 
      select(date, `Display Name En`, input$index) %>% 
      arrange(`Display Name En`, date)
    colnames(data) <- c("date", "key", "value")
    
    key <- unique(data$key)
    
    plot2 <- plot_ly(hoverinfo = "name + x + text")
    
    for (i in key) {
      plot2 <- plot2 %>% 
        add_bars(x = data$date[data$key == i],
                 y = data$value[data$key == i],
                 type = "bar",
                 text = data$value[data$key == i],
                 textposition = "inside",
                 name = i)
    }
    
    plot2 <- plot2 %>%
      layout(barmode = "stack")
    
    plot2
  })
  
  output$plot2 <- renderPlotly({
    plot2()
  })
  
  ## table2
  table2 <- reactive({
    if (is.null(groupData()))
      return(NULL)
    
    data <- groupData() %>% 
      melt(id.vars = c("date", "Display Name En", "Display Name Cn"),
           measure.vars = c("Sales(Mn)", "Growth%", "Share%", "share_delta%", "EI"),
           variable.name = "Index",
           value.name = "value") %>% 
      dcast(`Display Name En` + `Display Name Cn` + `Index` ~ `date`) %>% 
      arrange(Index, `Display Name Cn`)
    
    table2 <- datatable(
      data
    )
    
    table2
  })
  
  output$table2 <- renderDataTable({
    table2()
  })
  
  ##---- by molecule ----
  moleculeData <- reactive({
    if (is.null(raw()) | is.null(mapping()))
      return(NULL)
    
    data <- work_out(
      data = raw(),
      mapping_data = mapping(),
      input_metric = input$metric,
      current_time_point = input$ym,
      input_period = as.numeric(input$period),
      input_market = input$market,
      input_atc2 = input$atc2,
      input_atc3 = input$atc3,
      input_measure = input$measure,
      input_region = input$region,
      input_province = input$province,
      input_city = input$city,
      input_channel = input$channel,
      input_molecule = input$molecule,
      output_type = "by molecule",
      mapping_table_list = en_cn
    )
    
    data
  })
  
  ## table3
  table3 <- reactive({
    if (is.null(moleculeData()))
      return(NULL)
    
    data <- moleculeData() %>% 
      filter(date == input$ym) %>% 
      select(`Molecule En`, `Molecule Cn`, `Display Name1 En`, `Display Name1 Cn`, 
             `Sales(Mn)`, `Growth%`, `Share%`, `share_delta%`, `EI`)
    
    table3 <- datatable(
      data
    )
    
    table3
  })
  
  output$table3 <- renderDataTable({
    table3()
  })
  
  
  ## plot3
  plot3 <- reactive({
    if (is.null(moleculeData()))
      return(NULL)
    
    period_date <- sort(unique(substr(gsub("-", "", ymd(paste0(input$ym, "01")) - months(0:(as.numeric(input$period) - 1))), 1, 6)))
    
    data <- moleculeData() %>% 
      filter(date == period_date, `Molecule En` != "Total") %>% 
      select(date, `Molecule En`, input$index) %>% 
      arrange(`Molecule En`, date)
    colnames(data) <- c("date", "key", "value")
    
    key <- unique(data$key)
    
    plot3 <- plot_ly(hoverinfo = "name + x + text")
    
    for (i in key) {
      plot3 <- plot3 %>%
        add_trace(x = data$date[data$key == i],
                  y = data$value[data$key == i],
                  type = "scatter",
                  mode = "lines",
                  name = i)
    }
    
    plot3
  })
  
  output$plot3 <- renderPlotly({
    plot3()
  })
  
  ## plot4
  plot4 <- reactive({
    if (is.null(moleculeData()))
      return(NULL)
    
    period_date <- sort(unique(substr(gsub("-", "", ymd(paste0(input$ym, "01")) - months(0:(as.numeric(input$period) - 1))), 1, 6)))
    
    data <- moleculeData() %>% 
      filter(date == period_date, `Molecule En` != "Market") %>% 
      select(date, `Molecule En`, input$index) %>% 
      arrange(`Molecule En`, date)
    colnames(data) <- c("date", "key", "value")
    
    key <- unique(data$key)
    
    plot4 <- plot_ly(hoverinfo = "name + x + text")
    
    for (i in key) {
      plot4 <- plot4 %>% 
        add_bars(x = data$date[data$key == i],
                 y = data$value[data$key == i],
                 type = "bar",
                 text = data$value[data$key == i],
                 textposition = "inside",
                 name = i)
    }
    
    plot4 <- plot4 %>%
      layout(barmode = "stack")
    
    plot4
  })
  
  output$plot4 <- renderPlotly({
    plot4()
  })
  
  ## table4
  table4 <- reactive({
    if (is.null(moleculeData()))
      return(NULL)
    
    data <- moleculeData() %>% 
      melt(id.vars = c("date", "Molecule En", "Molecule Cn"),
           measure.vars = c("Sales(Mn)", "Growth%", "Share%", "share_delta%", "EI"),
           variable.name = "Index",
           value.name = "value") %>% 
      dcast(`Molecule En` + `Molecule Cn` + `Index` ~ `date`) %>% 
      arrange(Index, `Molecule Cn`)
    
    table4 <- datatable(
      data
    )
    
    table4
  })
  
  output$table4 <- renderDataTable({
    table4()
  })
  
  ##---- by brand ----
  brandData <- reactive({
    if (is.null(raw()) | is.null(mapping()))
      return(NULL)
    
    data <- work_out(
      data = raw(),
      mapping_data = mapping(),
      input_metric = input$metric,
      current_time_point = input$ym,
      input_period = as.numeric(input$period),
      input_market = input$market,
      input_atc2 = input$atc2,
      input_atc3 = input$atc3,
      input_measure = input$measure,
      input_region = input$region,
      input_province = input$province,
      input_city = input$city,
      input_channel = input$channel,
      input_molecule = input$molecule,
      output_type = "by brand",
      mapping_table_list = en_cn
    )
    
    data
  })
  
  ## table5
  table5 <- reactive({
    if (is.null(brandData()))
      return(NULL)
    
    data <- brandData() %>% 
      filter(date == input$ym) %>% 
      select(`Brand En`, `Brand Cn`, `Manufacture En`, `Manufacture Cn`, `Display Name1 En`, `Display Name1 Cn`, 
             `Display Name2 En`, `Display Name2 Cn`, `Display Name3 En`, `Display Name3 Cn`, 
             `Sales(Mn)`, `Growth%`, `Share%`, `share_delta%`, `EI`)
    
    table5 <- datatable(
      data
    )
    
    table5
  })
  
  output$table5 <- renderDataTable({
    table5()
  })
  
  
  ## plot5
  plot5 <- reactive({
    if (is.null(brandData()))
      return(NULL)
    
    period_date <- sort(unique(substr(gsub("-", "", ymd(paste0(input$ym, "01")) - months(0:(as.numeric(input$period) - 1))), 1, 6)))
    
    data <- brandData() %>% 
      filter(date == period_date, `Brand En` != "Total") %>% 
      select(date, `Brand En`, input$index) %>% 
      arrange(`Brand En`, date)
    colnames(data) <- c("date", "key", "value")
    
    key <- unique(data$key)
    
    plot5 <- plot_ly(hoverinfo = "name + x + text")
    
    for (i in key) {
      plot5 <- plot5 %>%
        add_trace(x = data$date[data$key == i],
                  y = data$value[data$key == i],
                  type = "scatter",
                  mode = "lines",
                  name = i)
    }
    
    plot5
  })
  
  output$plot5 <- renderPlotly({
    plot5()
  })
  
  ## plot6
  plot6 <- reactive({
    if (is.null(brandData()))
      return(NULL)
    
    period_date <- sort(unique(substr(gsub("-", "", ymd(paste0(input$ym, "01")) - months(0:(as.numeric(input$period) - 1))), 1, 6)))
    
    data <- brandData() %>% 
      filter(date == period_date, `Brand En` != "Total") %>% 
      select(date, `Brand En`, input$index) %>% 
      arrange(`Brand En`, date)
    colnames(data) <- c("date", "key", "value")
    
    key <- unique(data$key)
    
    plot6 <- plot_ly(hoverinfo = "name + x + text")
    
    for (i in key) {
      plot6 <- plot6 %>% 
        add_bars(x = data$date[data$key == i],
                 y = data$value[data$key == i],
                 type = "bar",
                 text = data$value[data$key == i],
                 textposition = "inside",
                 name = i)
    }
    
    plot6 <- plot6 %>%
      layout(barmode = "stack")
    
    plot6
  })
  
  output$plot6 <- renderPlotly({
    plot6()
  })
  
  ## table6
  table6 <- reactive({
    if (is.null(brandData()))
      return(NULL)
    
    data <- brandData() %>% 
      melt(id.vars = c("date", "Brand En", "Brand Cn"),
           measure.vars = c("Sales(Mn)", "Growth%", "Share%", "share_delta%", "EI"),
           variable.name = "Index",
           value.name = "value") %>% 
      dcast(`Brand En` + `Brand Cn` + `Index` ~ `date`) %>% 
      arrange(Index)
    
    table6 <- datatable(
      data
    )
    
    table6
  })
  
  output$table6 <- renderDataTable({
    table6()
  })
  
}








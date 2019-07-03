# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  PHA MAX BI Tool
# Purpose:      Shiny ui
# programmer:   Zhe Liu
# Date:         28-06-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##---- load packages ----
library(shinydashboard)
library(plotly)
library(DT)
library(shinyjs)
library(leaflet)
library(leafletCN)
library(shinyWidgets)

##---- ui ----
ui <- dashboardPage(
  
  dashboardHeader(title = "PHA MAX BI TOOL"),
  
  dashboardSidebar(
    # width = "250px",
    # tags$head(includeCSS("./www/fix_siderbar.css")),
    collapsed = FALSE,
    fluidRow(
      tags$div(
        column(12, fileInput(inputId = "raw", label = "Upload Raw Data")),
        column(12, fileInput(inputId = "mapping", label = "Upload Mapping Table")),
        column(12, selectInput(inputId = "metric", label = "Metric", choices = c("MAT", "YTD", "RQ", "MTH"), 
                               selected = "MAT")),
        column(12, selectInput(inputId = "ym", label = "Year-Month", choices = "")),
        column(12, selectInput(inputId = "period", label = "Period", choices = c(12, 24, 36, 48, 60), selected = 12)),
        column(12, selectInput(inputId = "market", label = "Market", choices = "")),
        column(12, selectInput(inputId = "atc2", label = "ATC2", choices = "")),
        column(12, selectInput(inputId = "atc3", label = "ATC3", choices = "")),
        column(12, selectInput(inputId = "measure", label = "Measure", choices = c("RMB", "County Unit", "Price"))),
        conditionalPanel(
          condition = "input.measure != 'Price'",
          column(12, selectInput(inputId = "index", label = "Index", choices = c("Sales(Mn)", "Share%", "Growth%", "share_delta%", "EI"), 
                                 selected = "Sales(Mn)", multiple = TRUE))
        ),
        column(12, selectInput(inputId = "region", label = "Region", choices = "")),
        column(12, selectInput(inputId = "province", label = "Province", choices = "")),
        column(12, selectInput(inputId = "city", label = "Ciyt", choices = "")),
        column(12, selectInput(inputId = "channel", label = "Channel", choices = "")),
        column(12, selectInput(inputId = "molecule", label = "Molecule", choices = "", selected = NULL, multiple = TRUE)),
        column(12, actionButton(inputId = "go", label = "Go", width = "200px")),
        tags$div(downloadButton(outputId = "download", label = "Download", style = "width:200px; color:#000;"),
                 style = "display:inline-block; width:100%; text-align:center;")
      )
    )
  ),
  
  dashboardBody(
    
    useShinyjs(),
    
    tabsetPanel(
      tabPanel(
        strong("By Group"),
        value = "1",
        br(),
        fluidRow(
          box(
            title = "1-1 Market Summary",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            tags$div(
              dataTableOutput("table1"),
              style = "overflow-x:scroll;"
            )
          ),
          box(
            title = "1-2 Sales Trend",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            tags$div(
              plotlyOutput("plot1", height = "auto")
            )
          ),
          box(
            title = "1-3 Sales Trend",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            tags$div(
              plotlyOutput("plot2", height = "auto")
            )
          ),
          box(
            title = "1-4 Raw Data Detail",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            tags$div(
              dataTableOutput("table2"),
              style = "overflow-x:scroll;"
            )
          )
        )
      ),
      
      tabPanel(
        strong("By Molecule"),
        value = "2",
        br(),
        fluidRow(
          box(
            title = "1-1 Market Summary",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            tags$div(
              dataTableOutput("table3"),
              style = "overflow-x:scroll;"
            )
          ),
          box(
            title = "1-2 Sales Trend",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            tags$div(
              plotlyOutput("plot3", height = "auto")
            )
          ),
          box(
            title = "1-3 Sales Trend",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            tags$div(
              plotlyOutput("plot4", height = "auto")
            )
          ),
          box(
            title = "1-4 Raw Data Detail",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            tags$div(
              dataTableOutput("table4"),
              style = "overflow-x:scroll;"
            )
          )
        )
      ),
      
      tabPanel(
        strong("By Brand"),
        value = "3",
        br(),
        fluidRow(
          box(
            title = "1-1 Market Summary",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            tags$div(
              dataTableOutput("table5"),
              style = "overflow-x:scroll;"
            )
          ),
          box(
            title = "1-2 Sales Trend",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            tags$div(
              plotlyOutput("plot5", height = "auto")
            )
          ),
          box(
            title = "1-3 Sales Trend",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            tags$div(
              plotlyOutput("plot6", height = "auto")
            )
          ),
          box(
            title = "1-4 Raw Data Detail",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            tags$div(
              dataTableOutput("table6"),
              style = "overflow-x:scroll;"
            )
          )
        )
      )
    )
  )
)



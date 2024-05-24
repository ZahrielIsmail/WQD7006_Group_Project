library(shiny)
library(shinydashboard)
library(shinyjs)
library(rlang)
library(randomForest)

header <- dashboardHeader(
  title = "Transaction Risk Prediction",
  titleWidth = 900,
  tags$li(class = "dropdown", tags$a(href = "https://github.com/IzzulI/Shinyapp_WQD7001", icon("github"), "Source Code", target = "_blank"))
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebar",
    menuItem("Title", tabName = "info", icon = icon("database")),
    menuItem("Classification", tabName = "classification", icon = icon("sitemap")),
    menuItem("Regression", tabName = "regression", icon = icon("line-chart")),
    menuItem("Prediction Classification Model", tabName = "pm_class", icon = icon("computer")),
    menuItem("Prediction Regression Model", tabName = "pm_reg", icon = icon("computer")),
    menuItem("Scrapper", tabName = "scrapper", icon = icon("cloud-download-alt"))
  )
)

body <- dashboardBody(
  tags$style(HTML('.skin-blue .main-header .navbar {background-color: #0056b3}
                   .skin-blue .main-header .logo {background-color: #007bff}
                   .skin-blue .main-header .logo:hover {background-color: #0056b3}
                   .skin-blue .main-sidebar {background-color: #e9ecef;}
                   .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {background-color: #007bff}
                   .skin-blue .main-sidebar .sidebar .sidebar-menu .active a:hover {background-color: #0056b3}
                   .skin-blue .main-header .navbar .sidebar-toggle:hover {background-color: #007bff}
                   .content-wrapper, .right-side {background: linear-gradient(to bottom, #FFFFFF, #e9ecef)}')),
  tags$style(HTML('
    .scrollable-table {
      overflow-x: auto;
    }
  ')),
  tabItems(
    tabItem(
      tabName = "info",
      fluidRow(
        column(
          width = 8,
          h2("Introduction"),
          tags$img(src = "cover.png", width = 800, height = 520)
        )
      )
    ),
    tabItem(
      tabName = "classification",
      fluidRow(
        column(width = 4, tags$img(src = "c2.png", width = 300, height = 200)),
        column(width = 4, tags$img(src = "c3.png", width = 300, height = 200)),
        column(width = 4, tags$img(src = "c4.png", width = 300, height = 200)),
        column(width = 4, tags$img(src = "c5.png", width = 300, height = 200)),
        column(width = 4, tags$img(src = "c6.png", width = 300, height = 200)),
        column(width = 4, tags$img(src = "c7.png", width = 300, height = 200))
      ),
      
    ),
    tabItem(
      tabName = "regression",
      fluidRow(
        column(width = 4, tags$img(src = "r2.png", width = "300px", height = "200px")),
        column(width = 4, tags$img(src = "r3.png", width = "300px", height = "200px")),
        column(width = 4, tags$img(src = "r4.png", width = "300px", height = "200px")),
        column(width = 4, tags$img(src = "r5.png", width = "300px", height = "200px")),
        column(width = 4, tags$img(src = "r6.png", width = "300px", height = "200px")),
        column(width = 4, tags$img(src = "r7.png", width = "300px", height = "200px"))
        
      ),
      fluidRow(
        column(width = 4, tags$img(src = "r1.png", width = "900px", height = "300px")),
        #column(width = 4, tags$img(src = "img8.png", width = "400px", height = "300px")),
        #column(width = 4, tags$img(src = "img9.png", width = "400px", height = "300px"))
      )
    ),
    tabItem(
      tabName = "pm_class",
      fluidPage(
        fluidRow(hr()),
        h4("Input Attributes"),
        fluidRow(
          box(width=12,style = "color: #343a40; background: linear-gradient(to bottom, #FFFFFF, #e9ecef)",
              fluidRow(
                column(4, selectInput("location_region", "Location Region", c("Africa", "Asia", "Europe", "North America", "South America"))),
                column(4, numericInput("login_frequency", "Login Frequency",value = 0)),
                column(4, selectInput("purchase_pattern", "Purchase Pattern", c("focused", "high_value", "random"))),
                column(4, selectInput("age_group", "Age Group", c("established", "new", "veteran"))),
                column(4, selectInput("weekend_or_weekday", "Weekend or Weekday", c("Weekend", "Weekday"))),
                column(4, numericInput("session_duration_normalized", "Session Duration Normalized", 0.25, min = 0, max = 1)),
                column(4, selectInput("time_of_day","Time of Day",c("Morning","Afternoon","Evening","Late Night"))),
                # column(4, selectInput("new_transactions","New Transactions",c("purchase","sale","scam","transfer"))),
                column(4, selectInput("network_size","Network Size",c("Large","Medium","Small"))),
                column(4,numericInput("freq","Frequency",value = 0)),
                column(4,numericInput("amount","Amount",value = 0))
              )
          ),
          fluidRow(
            column(6, actionButton("predict_class", "Predict Classification", style = "vertical-align:bottom; color: #FFFFFF; background-color: #007bff", class = "btn-primary"))
          ),
          fluidRow(hr()),
          h4("Prediction Output"),
          fluidRow(
            div(
              id = "results",
              fluidRow(width=12,verbatimTextOutput("output_msg")),
              fluidRow(hr()),
              fluidRow(column(12, align = "center", uiOutput("image")))
            )
          )
        )
      )
    ),
    tabItem(
      tabName = "pm_reg",
      fluidPage(
        fluidRow(hr()),
        h4("Input Attributes"),
        fluidRow(
          box(width=12,style = "color: #343a40; background: linear-gradient(to bottom, #FFFFFF, #e9ecef)",
              fluidRow(
                column(4, selectInput("location_region_reg", "Location Region", c("Africa", "Asia", "Europe", "North America", "South America"))),
                column(4, numericInput("login_frequency_reg", "Login Frequency", value = 0)),
                column(4, selectInput("purchase_pattern_reg", "Purchase Pattern", c("focused", "high_value", "random"))),
                column(4, selectInput("age_group_reg", "Age Group", c("established", "new", "veteran"))),
                column(4, selectInput("weekend_or_weekday_reg", "Weekend or Weekday", c("Weekend", "Weekday"))),
                column(4, numericInput("session_duration_normalized_reg", "Session Duration Normalized", 0.25, min = 0, max = 1)),
                column(4, selectInput("time_of_day_reg","Time of Day",c("Morning","Afternoon","Evening","Late Night"))),
                # column(4, selectInput("new_transactions","New Transactions",c("purchase","sale","scam","transfer"))),
                column(4, selectInput("network_size_reg","Network Size",c("Large","Medium","Small"))),
                column(4,numericInput("freq_reg","Frequency", value = 0)),
                column(4,numericInput("amount_reg","Amount",value = 0))
              )
          ),
          fluidRow(
            column(6, actionButton("predict_reg", "Predict Regression", style = "vertical-align:bottom; color: #FFFFFF; background-color: #007bff", class = "btn-primary"))
          ),
          fluidRow(hr()),
          h4("Prediction Output"),
          fluidRow(
            div(
              id = "results1",
              fluidRow(width=12,verbatimTextOutput("output_reg_msg")),
              fluidRow(hr()),
              fluidRow(column(12, align = "center", uiOutput("image_reg")))
            )
          )
        )
      )
    ),
    tabItem(
      tabName = "scrapper",
      fluidPage(
        titlePanel("Ethereum Transaction Scraper"),
        fluidRow(
          column(12,
                 textInput("eth_address", "Enter Ethereum Address:", ""),
                 actionButton("scrape", "Get Transactions"),
          )
        ),
        fluidRow(
          column(12,
                 div(class = "scrollable-table", tableOutput("transactions_table")),
                 uiOutput("pagination_buttons")
          )
        )
      )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

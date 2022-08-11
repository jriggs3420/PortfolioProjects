library(shiny)
library(datarium)

ui <- fluidPage(
  titlePanel("Linear Regression Model (Marketing Dataset)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("outcome", label = h3("Dependent Variable"),
                  choices = list("Sales" = "sales",
                                 "YouTube" = "youtube",
                                 "Facebook" = "facebook",
                                 "Newspaper" = "newspaper"), selected = 1),
      
      selectInput("indepvar", label = h3("Independent Variable"),
                  choices = list("YouTube" = "youtube",
                                 "Facebook" = "facebook",
                                 "Newspaper" = "newspaper",
                                 "Sales" = "sales"), selected = 1)
      
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Scatterplot", plotOutput("scatterplot")), # Plot
                  tabPanel("Distribution", # Plots of distributions
                           fluidRow(
                             column(6, plotOutput("distribution1")),
                             column(6, plotOutput("distribution2")))
                  ),
                  tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
                  tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
                  
      )
    )
  ))

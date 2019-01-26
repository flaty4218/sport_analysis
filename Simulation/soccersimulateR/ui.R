library(shiny)
#library(plotly)

# Define UI for for soccer winning percentage simulate app
ui <- fluidPage(
  
  # App title ----
  titlePanel("soccer simulateR"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: A team's score ----
      numericInput(inputId = "score_A",
                   label = "A team's score:",
                   value = 0),
      
      # Input: B team's score ----
      numericInput(inputId = "score_B",
                   label = "B team's score:",
                   value = 0),
      
      # br() element to introduce extra vertical spacing ----
      #br(),
      
      # Input: Slider for the number of remaining time ----
      sliderInput("time",
                  "remaining time:",
                  value = 90,
                  min = 1,
                  max = 90), 
      
      # Input: Slider for the number of simulation----
      numericInput(inputId = "n",
                   label = "Number of simulation:",
                   value = 100),
      
      # Input: A team's expected score per 1 game ----
      numericInput(inputId = "expected_score_A",
                   label = "A team's expected score per 1 game:",
                   value = 1),
      
      # Input: B team's expected score per 1 game ----
      numericInput(inputId = "expected_score_B",
                   label = "B team's expected score per 1 game:",
                   value = 1)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset----
      tabsetPanel(type = "tabs",
                  tabPanel("winning percentage", plotOutput("winning_percentage")),
                  tabPanel("distribution of score", plotOutput("distribution_score")),
                  tabPanel("percentage transition", plotOutput("percentage_transition"))
      )
      
    )
  )
)
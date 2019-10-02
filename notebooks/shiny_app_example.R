# Part One
## Load The Data And The Usual Packages

cz <- read.csv("https://raw.githubusercontent.com/mjclawrence/soci385/master/data/commuting_zones.csv")
library(tidyverse)

## In this example, we only want the numeric variables; 
## they are in columns 4:34

cz_variables <- cz[, 4:34]

## Note: leave empty space before comma to show
## we want all rows



# Part Two
## Set Up The User Interface's Title, Sidebar, and Main Panel

ui <- fluidPage(

  ### Application Title
  titlePanel("Correlations From Chetty et al's Mobility Dataset"),

  ### UI For Sidebar
  sidebarPanel( # details for the panel on the left side of the screen
    selectInput("x_variable", # how r will refer to selected variable
      "X Variable", # title for drop down menu
      sort(names(cz_variables)), # possible variables to select, sorted alphabetically
      selected = FALSE, # don't pre-select a variable
      selectize = FALSE, # don't pre-select a variable
      size = 4 # drop down window should show this many options
    ), 
    selectInput("y_variable",
      "Y Variable",
      sort(names(cz_variables)),
      selected = "mobility", # pre-select the variable with this name
      selectize = FALSE,
      size = 4
    )
  ),

  ### UI For Main Panel
  mainPanel( # details for the main panel in the center of the screen
    tabsetPanel( # establishing that we will have tabbed windows in this panel
      tabPanel(
        "Scatterplot", # title for tab
        div(
          style = "font-size: 20px", # font for tab title
          plotOutput("plot") # saved object to be displayed
        )
      ), 
      tabPanel(
        "Correlation",
        div(
          style = "font-size: 20px",
          verbatimTextOutput("correlation")
        )
      )
  ) # Close the tabsetPanel open parentheses
  ), # Close the mainPanel open parentheses
  
  ### Suppress Warning Messages
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  )
)

# Part Three
## What R Does With The Variables Selected To Create Output For Tabs

server <- function(input, output, session) {

  ### Create the plot
  output$plot <- renderPlot({ # output$plot means this will show up in the plot tab
    ggplot(cz_variables, aes_string(
      x = input$x_variable, # the selected x variable
      y = input$y_variable # the selected y variable
    )) + 
      geom_point() + labs(title = "Opportunity Insights Data") +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 18)
      )
  })


  ### Generate the correlation
  output$correlation <- renderPrint({
    dataset <- cz %>%
      select(input$x_variable, input$y_variable)

    cor(dataset, use = "complete")[2, 1] # pull row 2, column 1 from correlation matrix
  })
} # Close the server function curly brackets

# Part Four: The Function That Runs Everything

shinyApp(ui = ui, server = server)
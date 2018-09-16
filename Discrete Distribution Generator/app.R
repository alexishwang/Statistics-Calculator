library(shiny)
library(plotly)

#### UI ####
ui <- fluidPage(
  titlePanel("Discrete Distribution Generator"),
  br(),
  navbarPage(title = "Distribution", 
             #### Bernoulli Distribution ####
             tabPanel("Bernoulli",   
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("bernoulliCount", "Count", 1000),
                          numericInput("bernoulliProb", "Probability", 0.8)
                        ),
                        mainPanel(
                          plotlyOutput("bernoulliPlot")
                        ))
             ), 
             #### Poisson Distribution ####
             tabPanel("Poisson", 
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("poissonCount", "Count", 1000),
                          numericInput("poissonMean", "Mean", 10)
                        ),
                        mainPanel(
                          plotlyOutput("poissonPlot")
                        ))
             ),
             #### Binomial Distribution ####
             tabPanel("Binomial",
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("binomialCount", "Count", 1000),
                          numericInput("binomialSize","Size",10),
                          numericInput("binomialProb", "Probability", 0.8)
                        ),
                        mainPanel(
                          plotlyOutput("binomialPlot")
                        ))
             ),
             #### Geometric Distribution ####
             tabPanel("Geometric",
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("geometricCount", "Count", 10),
                          numericInput("geometricProb", "Standard Deviation", 0.8)
                        ),
                        mainPanel(
                          plotlyOutput("geometricPlot")
                        ))
             ),
             
             p("Visit ",
               a("Alexis's GitHub", 
                 href = "https://github.com/alexishwang"))
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$bernoulliPlot <- renderPlotly({
    x = rbinom(n = input$bernoulliCount,1, prob = input$bernoulliProb)
    plot_ly(x = x, type = "histogram", marker = list(color = "skyblue"))
  })
  output$poissonPlot <- renderPlotly({
    x = rpois(input$poissonCount, input$poissonMean)
    plot_ly(x = x, type = "histogram", marker = list(color = "skyblue"))
  })
  output$binomialPlot <- renderPlotly({
    x = rbinom(n = input$binomialCount,size = input$binomialSize, 
               prob = input$binomialProb)
    plot_ly(x = x, type = "histogram", marker = list(color = "skyblue"))
  })
  output$geometricPlot <- renderPlotly({
    x = rgeom(n = input$geometricCount,prob = input$geometricProb)
    plot_ly(x = x, type = "histogram", marker = list(color = "skyblue"))
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
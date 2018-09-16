library(shiny)
library(plotly)

#### UI ####
ui <- fluidPage(
  titlePanel("Continous Distribution Generator"),
  br(),
  navbarPage(title = "Distribution", 
             #### Uniform Distribution ####
             tabPanel("Uniform",   
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("uniformCount", "Count", 1000),
                          numericInput("uniformMin", "Min", 0),
                          numericInput("uniformMax", "Max", 1)
                          # selectInput("dist", "Select Distribution", choice = distribution),
                          # sliderInput("bins", "Number of bins:", min = 1000, max = 10000, value = 2000)
                        ),
                        mainPanel(
                          plotlyOutput("uniformPlot")
                        ))
             ), 
             #### Normal Distribution ####
             tabPanel("Normal", 
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("normalCount", "Count", 1000),
                          numericInput("normalMean", "Mean", 0),
                          numericInput("normalSD", "Standard Deviation", 1)
                        ),
                        mainPanel(
                          plotlyOutput("normalPlot")
                        ))
             ),
             #### Exponential Distribution ####
             tabPanel("Exponential",
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("exponentialCount", "Count", 1000),
                          numericInput("exponentialMean", "Mean", 10)
                        ),
                        mainPanel(
                          plotlyOutput("exponentialPlot")
                        ))
             ),
             #### Gamma Distribution ####
             tabPanel("Gamma",
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("gammaCount", "Count", 1000),
                          numericInput("gammaShape", "Shape", 1),
                          numericInput("gammaScale", "Scale", 2)
                        ),
                        mainPanel(
                          plotlyOutput("gammaPlot")
                        ))
             ),
             #### Beta Distribution ####
             tabPanel("Beta",
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("betaCount", "Count", 1000),
                          numericInput("betaShape1", "Shape1", 1),
                          numericInput("betaShape2", "Shape2", 1)
                        ),
                        mainPanel(
                          plotlyOutput("betaPlot")
                        ))
             ),
             #### Log Normal Distribution ####
             tabPanel("Log Normal",
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("lognormalCount", "Count", 1000),
                          numericInput("lognormalMean", "Mean", 0),
                          numericInput("lognormalSD", "Standard Deviation", 1)
                        ),
                        mainPanel(
                          plotlyOutput("lognormalPlot")
                        ))
             ),
             #### chi-squared Distribution ####
             tabPanel("Chi-squared",
                      sidebarLayout(
                        sidebarPanel(
                          numericInput("chisquaredCount", "Count", 1000),
                          numericInput("chisquaredDF", "Degree of Freedom", 3)
                        ),
                        mainPanel(
                          plotlyOutput("chisquaredPlot")
                        ))
             ),
             
             p("Visit ",
               a("Alexis's GitHub", 
                 href = "https://github.com/alexishwang"))
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$uniformPlot <- renderPlotly({
    x = runif(input$uniformCount,input$uniformMin,input$uniformMax)
    plot_ly(x = x, type = "histogram", marker = list(color = "skyblue"))
  })
  output$normalPlot <- renderPlotly({
    x = rnorm(input$normalCount,input$normalMean,input$normalSD)
    plot_ly(x = x, type = "histogram", marker = list(color = "skyblue"))
  })
  output$exponentialPlot <- renderPlotly({
    x = rexp(n = input$exponentialCount,rate = 1/input$exponentialMean)
    plot_ly(x = x, type = "histogram", marker = list(color = "skyblue"))
  })
  output$gammaPlot <- renderPlotly({
    x = rgamma(n = input$gammaCount,shape = input$gammaShape,
               scale = input$gammaScale)
    plot_ly(x = x, type = "histogram", marker = list(color = "skyblue"))
  })
  output$betaPlot <- renderPlotly({
    x = rbeta(n = input$betaCount,shape1 = input$betaShape1,
              shape2 = input$betaShape2)
    plot_ly(x = x, type = "histogram", marker = list(color = "skyblue"))
  })
  output$lognormalPlot <- renderPlotly({
    x = rlnorm(input$lognormalCount,
               meanlog = input$lognormalMean,
               sdlog = input$lognormalSD)
    plot_ly(x = x, type = "histogram", marker = list(color = "skyblue"))
  })
  output$chisquaredPlot <- renderPlotly({
    x = rchisq(n = input$chisquaredCount, df = input$chisquaredDF)
    plot_ly(x = x, type = "histogram", marker = list(color = "skyblue"))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


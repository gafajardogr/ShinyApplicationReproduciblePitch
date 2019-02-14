
ui =  fluidPage(titlePanel("Exploring the mtcars dataset"),
                sidebarLayout(
                    sidebarPanel(
                        selectInput(
                            "varchoice",
                            "Choose the variable for which you want to check",
                            choices = c('Cylinders' = 'cyl',
                                        'Miles/gallon'= 'mpg', 
                                        'Displacement'= 'disp', 
                                        'Quarter miles/sec' = 'qsec', 'Gross horsepower' = 'hp', 
                                        'Weight (lb/1000)'= 'wt')
                        ),
                        radioButtons(
                            "normchoice",
                            "How do you want to check the points?",
                            choices = c("Plots", "Tests"),
                            selected = "Plots"
                        ),
                        conditionalPanel(
                            "input.normchoice == 'Plots'",
                            selectInput(
                                "plotchoice",
                                "Choose which plot you want?",
                                choices = c("Histogram", "Boxplot", "QQ-Plot")
                            )
                        )
                    ),
                    mainPanel(
                        h3("Instructions"),
                        h5("1. Choose the variable"),
                        h5("2. Select 'How do you want to check the points?"),
                        h5("3. If you select 'Plots', you can paint"),
                        h5("4 If you select Tests the program will show Shapiro-Wilk Test to check normality"),
                        conditionalPanel("input.normchoice == 'Plots'", plotOutput("myplot")),
                        conditionalPanel("input.normchoice == 'Tests'", verbatimTextOutput("mytest"))
                    )
                ))

server = function(input, output) {
    var = reactive({
        mtcars[, input$varchoice]
        
    })
    output$myplot =  renderPlot({
        if (input$plotchoice == "Histogram")
            return(hist(var(), main = "Histogram", xlab = input$varchoice))
        if (input$plotchoice == "Boxplot")
            return (boxplot(var(), main="Boxplot", xlab = input$varchoice))
        if (input$plotchoice == "QQ-Plot")
            return(qqnorm(var(), main = paste("QQ plot of", input$varchoice)))
    })
    output$mytest = renderPrint({
        shapiro.test(var())
    })
}

shinyApp(ui, server)
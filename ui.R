
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Iris Species prediction"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("test.size",
                        "Training porportion",
                        min = min(20),
                        max = max(100),
                        value = 70),
            
            sliderInput("sepal.length",
                        "Sepal Length",
                        min = min(iris$Sepal.Length),
                        max = max(iris$Sepal.Length),
                        value = mean(iris$Sepal.Length, na.rm = T)),
            
            sliderInput("petal.length",
                        'Petal Length',
                        min = min(iris$Petal.Length),
                        max = max(iris$Petal.Length),
                        value = mean(iris$Petal.Length, na.rm = T)),
            
            h5(htmlOutput('prediction')),
            
            
            br(),
            p('An intereting takeaway from this project is that not much information is needed
               to precisely classify the species of the iris flowers. In this case, just two variables
               were used.')
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h3("Decision tree - Classification of iris species on two variables"),
            plotOutput("tree"),
            h3('Position of new made-up observation'),
            plotOutput("scatterplot"),
            
        )
    )
))

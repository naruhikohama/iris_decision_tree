
library(shiny)
library(rpart)
library(rpart.plot)
suppressPackageStartupMessages(library(tidyverse))
data('iris')

# Define server logic required to draw a histogram



shinyServer(function(input, output) {
    
    plot.tree <- reactive({
        
        smp_size <- floor(input$test.size/100 * nrow(iris))
        
        ## set the seed to make your partition reproducible
        set.seed(123)
        train_ind <- sample(seq_len(nrow(iris)), size = smp_size)
        train <- iris[train_ind, ]
        
        model_tree <- rpart(Species ~ Sepal.Length + Petal.Length, data = train, method = 'class')
        
        paleta <- c("#3489eb", "#a0a1a3", "#5ec752") 
        tree <- prp(model_tree, 
                         sub = "", type = 2, extra = 'auto', fallen.leaves = TRUE, cex = 1,
                         left = FALSE, box.palette = 'auto', border.col = "#DBDBDC", varlen = 0,
                         round = 0.5, split.font = 1, split.cex = 0.9, branch.col = "grey90", 
                         branch.lty = 1, branch.type = 5, branch.tweak = 0.3, ge = " ≥ ",
                         space = 0.5, split.yshift = -1, split.space = 0, split.box.col = NULL)
        
        results <- list(tree, model_tree)
        
        return(results)
     })
     

    output$tree <- renderPlot({
        plot.tree()[[1]]
    })
    
    
    plot.scatter <- reactive({
        x.extra = input$sepal.length
        y.extra = input$petal.length
        
        paleta <- c('#f26666' , "#a0a1a3", "#5ec752") #vermelho, cinza e verde
        
        new_point_color <- ifelse(extra_point() == 'virginica', paleta[3],
                                  ifelse(extra_point() == 'versicolor', paleta[2], paleta[1]))
        
        scatter <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
                        geom_point(aes(color = Species)) +
                        scale_color_manual(values = paleta) +
                        geom_point(aes(x.extra, y.extra),
                                   color = new_point_color,
                                   size = 5,
                                   alpha = 0.6,
                                   shape = 23) +
                        theme(panel.grid = element_blank(),
                              panel.background = element_rect(fill = "white"),
                              title = element_text(family = "Lato", colour = "#4D4D4D"),
                              axis.text = element_text(family = "Lato", colour = "#4D4D4D", size = 8),
                              axis.text.x = element_text(vjust = 2),
                              legend.text = element_text(family = "Lato", colour = "#4D4D4D"),
                              axis.ticks = element_line(colour = "#8F8F8F"),
                              panel.border = element_blank(),
                              axis.line = element_line(color = "#ebebeb", size = 0.25),
                              strip.background = element_rect(fill = '#ebebeb'),
                              strip.text = element_text(family = "Lato", colour = "#474747", size = 9),
                              plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm")
                            )
        return(scatter)
    })
    
    output$scatterplot <- renderPlot({
        plot.scatter()
    })
    
    extra_point <- reactive({
        x.extra = input$sepal.length
        y.extra = input$petal.length
        return(predict(plot.tree()[[2]], newdata = data.frame(Sepal.Length = x.extra, 
                                                 Petal.Length = y.extra),
                                                 type = 'class')
        )
    })
    
    output$prediction <- renderText({
        paleta <- c('#f26666' , "#a0a1a3", "#5ec752")
        new_point_color <- ifelse(extra_point() == 'virginica', paleta[3],
                                  ifelse(extra_point() == 'versicolor', paleta[2], paleta[1]))
        
        print(paste0("The species prediction for the new observation is ",
                     paste0("<font color=", "'", new_point_color, "'", "><b>"),
                     extra_point(),
                     "</b></font>"))
    })

})

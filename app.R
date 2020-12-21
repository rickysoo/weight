library(shiny)
library(shinythemes)

min_height <- 100
max_height <- 220

min_weight <- 20
max_weight <- 120

x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

ui <- fluidPage(
    theme = shinytheme('united'),
    
    titlePanel('From Height to Weight'),
    p('Drag the slider to find your weight. Just for fun!'),
    hr(),
    
    sidebarLayout(
        sidebarPanel(
            uiOutput('height'),
            uiOutput('weight'),
            
            tags$head(
                tags$style(HTML('#lucky {background-color: #004165}'))
            ),
            actionButton(
                inputId = 'lucky',
                label = 'I\'m Feeling Lucky!'
            )
        ),
        
        mainPanel(
            h3('Linear Regression Model'),
            plotOutput('graph')
        )
    )
    
)

server <- function(input, output, session) {
    values <- reactiveValues(
        height = 150,
        weight = 0
    )
    
    getModel <- reactive({
        weight <- y
        height <- x
        relation <- lm(weight ~ height)
        return(relation)
    })    
    
    getWeight <- function(height) {
        weight <- predict(getModel(), data.frame(height = height))
        return(weight)
    }
    
    output$height <- renderUI({
        sliderInput(
            inputId = 'height',
            label = 'Your height (cm)',
            min = min_height,
            max = max_height,
            value = values$height
        )
    })
    
    output$weight <- renderUI({
        sliderInput(
            inputId = 'weight',
            label = 'Your weight (kg)',
            min = min_weight,
            max = max_weight,
            value = values$weight
        )
    })
    
    output$graph <- renderPlot({
        plot(x, y, col = "blue", main = "From Height to Weight", abline(getModel()), cex = 1.3, pch = 16, xlab = "Height in cm", ylab = "Weight in kg", xlim = c(min_height, max_height), ylim = c(min_weight, max_weight))
        abline(v = values$height, col = 'green')
        abline(h = values$weight, col = 'green')
    })
    
    observeEvent(input$height, {
        values$height <- input$height
        values$weight <- getWeight(values$height)
    })
    
    observeEvent(input$weight, {
        values$weight <- 0
        values$weight <- getWeight(values$height)
    })
    
    observeEvent(input$lucky, {
        values$height <- sample(min_height:max_height, 1)
        values$weight <- getWeight(values$height)
    })
    
}

shinyApp(ui, server)
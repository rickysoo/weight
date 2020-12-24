library(shiny)
library(shinythemes)
library(ggplot2)

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
            h3(textOutput('answer')),
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
        return(round(weight))
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
    
    output$answer <- renderText({
        sprintf('Your height is %d cm, so your weight must be %d kg!', values$height, values$weight)
    })
    
    output$graph <- renderPlot({
        intercept <- getModel()$coefficients[[1]]
        slope <- getModel()$coefficients[[2]]
        
        ggplot() +
            geom_point(mapping = aes(x = x, y = y)) +
            geom_abline(intercept = intercept, slope = slope) +
            geom_vline(xintercept = values$height, color = 'red', linetype = 'dashed') +
            geom_hline(yintercept = values$weight, color = 'red', linetype = 'dashed') +
            labs(x = 'Height (cm)', y = 'Weight (kg)') +
            theme(
                axis.title = element_text(face = 'bold', size = 14),
                plot.background = element_blank(),
                panel.background = element_blank(),
                panel.grid.major.y = element_line(color = 'grey')
            )
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
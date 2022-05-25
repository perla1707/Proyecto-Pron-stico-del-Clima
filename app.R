

library(shiny)
library(shinythemes)
library(markovchain)

# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme("spacelab"),
  
  titlePanel("Cadenas de Markov. Pronóstico del clima"),
  
 
  helpText("Matriz de transicion de un paso",
           "con la cual es posible observar la probabilidad de que",
           "el clima este nublado,lluvioso, soleado o neblinoso"),

  selectInput(inputId = "dataset",
              label = "SELECCIONE UNA CIUDAD:",
              choices = c("ÁMSTERDAM", "LONDON")),

  
  sliderInput("steps",
              "Cantidad de pasos:",
              min = 1,
              max = 10,
              value = 5),
 


mainPanel(
  h3(textOutput("caption", container = span)),
  
  
  verbatimTextOutput("summary"),
  verbatimTextOutput("st"),
  helpText("DIAGRAMA DE TRANSICION DE ESTADOS"),
  
  plotOutput(outputId = "chainplot", height = "400px"),
  
  tableOutput("view"),
  
  img ( src = "cadena.png" , alto = 140 , ancho = 140 ) ,
  
  
)

)

server <- function(input, output) {
  
  datasetInput <- reactive({
  switch(input$dataset,
         "ÁMSTERDAM" = clima_Amsterdam,
         "LONDON" = clima_london,
         )
})

output$caption <- renderText({
  input$caption
})


output$summary <- renderPrint({
  dataset <- datasetInput()
  Pe<- dataset$Clima
  fit <- markovchainFit(data=Pe,confidencelevel = 0.75)
  print(fit$estimate)
  
  
})


output$view <- renderTable({
 
  head(datasetInput(), n = 50)
})

output$chainplot <- renderPlot({
  dataset <- datasetInput()
  Pe<- dataset$Clima
  fit <- markovchainFit(data=Pe,confidencelevel = 0.75)
  plot(fit$estimate)
})

output$st <- renderPrint({
  dataset <- datasetInput()
  Pe<- dataset$Clima
  fit <- markovchainFit(data=Pe,confidencelevel = 0.75)
  a<- fit$estimate^input$steps
  print("Probabilidades por pasos: ")
  
  print(a)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

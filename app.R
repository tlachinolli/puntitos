# R Shiny App: rotation & translation of point sets
#                RTPS
#       tltlll
#   This shiny app is only for educational purposes
#   it's intended to be an instrument for simple linear tranformation understanding.
#   Whit RTPS you can draw a set of points that correspond with
#   some basic figure vertex set and by using some RShiny slider widgets
#   -using some highschool tranformations-
#   be able to rotate and/or traslate the set of points/vertex
#   
######

# Import custom functions files

library(shiny)
library(magrittr)
library(shinythemes)
library(plotly)
source("linearTransformations.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("lumen"),
    # Application title
    titlePanel("Your2tor RTPS"),

    # Sidebar with a 
    #   slider input for degrees from -360 to 360
    #   slider input for horizontal translation from -4 to 4 spaces
    #   slider input for vertical translation from -4 to 4 spaces
    ###### requirements: Add slider for scaling object, manage exeptions for border exceeding
    sidebarLayout(
        sidebarPanel(
            h4("DIY mode"),
            helpText("(See instructions below)"),
            helpText("Suscribe to get infinite mode!"),
            radioButtons("radio", h5("Choose figure"),
                         choices = list("Square" = 1, "Right Triangle" = 2, "Acute Triangle" = 3),selected = character(0)),
           
            actionButton("insertarFigura", "Insert"),
            actionButton("sliderReset", "Origin"),
            actionButton("eraseTable", "Clear"),
            sliderInput("rotateDegrees",
                        "rotation in degrees:",
                        min = -360,
                        max = 360,
                        value = 0),
            sliderInput("traslateVert",
                        "vertical traslation:",
                        min = -35,
                        max = 35,
                        value = 0),
            sliderInput("traslateHor",
                        "horizontal traslation:",
                        min = -70,
                        max = 70,
                        value = 0),
            actionButton("imprimirFigura", "Print"),
            h4("Instructions"),
            helpText("1. Select a radio button to insert the correspondant figure vertex set (The first figure is centered at the origin)"
            ),
            helpText("2. Tranform the figure vertex set by using the sliders"
            ),
            helpText("3. Use Insert button to fix the figure in place"
            ),
            helpText("4. Repeat 1-3 to insert as many figures
                     as you want (subsequent figures share the same vertex as the last figure inserted; just make a tranformation
                     after inserting and you'll see. You can center the new figure by using the Origin button)"
            ),
            helpText("5. Export your graph by using the Print button"
            ),
            helpText("* Clear button will erase every inserted figure!"
            ),
            helpText("* The solution is added at the end of exported file!"
            ),
        ),
       
        

        # Show a plot of all inserted object vertex sets
        mainPanel(
            h1(" Rotation & Traslation of Point Sets"),
            p("Hide some figures and dare your friends to find them"),
           plotOutput("pointPlot"),
           tableOutput("vertSetMatrix"),
           plotOutput("linePlot")
           
        )
    )
)

# Define server logic required to draw the tranformed vertex set
server <- function(input, output, session) {
    v <- reactiveValues(data = NULL)
    dt <- reactiveValues(data = NULL)
    # variables reactivas que reciben input de usuario a partir de sliders
    rot <- reactive(input$rotateDegrees)
    horT <- reactive(input$traslateHor / 10)
    verT <- reactive(input$traslateVert / 10)
    #creo que aqui se utiliza observe para
    # ejecutar una acción cada vez que se interactue con el boton
    # la accion a ejecutar aqui sería guardar
    #la figura rotada en la tabla y graficar la tabla y encima la figura nueva..
    observeEvent(input$insertarFigura, {
        aux <- input$radio
        aux <- strtoi(aux)
        figura <- crearFigura(aux)
        v$data <- figura %>% rotarFigura(.,rot()) %>%
            trasladarFiguraHorizontal(.,horT()) %>%
            trasladarFiguraVertical(.,verT())
        
        v$data <- cbind(v$data, type = aux)
        dt$data <- rbind(dt$data, v$data)
    })
    
    observeEvent(input$sliderReset, {
        # boton reset block code
        updateSliderInput(session,'rotateDegrees',value = 0)
        updateSliderInput(session,'traslateVert',value = 0)
        updateSliderInput(session,'traslateHor',value = 0)
    })
    observeEvent(input$eraseTable, {
        # boton erase code block
        dt$data <- NULL
    })
    
    output$pointPlot <- renderPlot({
# Este plot será elemento reactivo cada ves que se inserte figura
# la grafica base cambiará a la grafica con figura insertada
# el control cambiará a modificar nuevas figuras a insertar
            if(length(input$radio)==0) return(0)
            aux <- input$radio
            aux <- strtoi(aux)
            figura <- crearFigura(aux)
            plot(c(- 5, 5), c(- 5, 5), type = "n", asp = 1, ylab = "", yaxt = "n", xlab = "", xaxt = "n")
            if (is.null(dt$data)){
                figura %>% rotarFigura(.,rot()) %>%
                    trasladarFiguraHorizontal(.,horT()) %>%
                    trasladarFiguraVertical(.,verT()) %>%
                    points(., pch = 19)
                return()
            }
            points(dt$data, pch = 19)
            figura %>% rotarFigura(.,rot()) %>%
                trasladarFiguraHorizontal(.,horT()) %>%
                trasladarFiguraVertical(.,verT()) %>%
                points(., pch = 19)
                return()

    })
    
    output$linePlot <- renderPlot({
        if (is.null(dt$data)) return()
        df <- as.data.frame(dt$data)
        
#       df$type <- as.factor(df$type)
        
        
        fig <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length)
        
        
        fig
        # p <- ggplot(df, aes(x, y, fill = type)) + 
       #      geom_point()
       # p + geom_polygon() + coord_fixed()
    })
    
    output$vertSetMatrix <- renderTable({ # test data table to keep vertex set
        if (is.null(dt$data)) return()
        dt$data
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

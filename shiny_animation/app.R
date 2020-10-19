source("animation_helper.R")
library("shiny")

ui <- fluidPage(
    titlePanel("PD visualizer"),
    sliderInput(inputId = "num",
                label = "Choose a number between 0 and two pi",
                value = 25, min = 0, max = round(2*pi, 2), step=.1, round= -2),
    checkboxInput("diagonals", label = "Toggle Diagonals", value = FALSE),
    
    #hr(),
    #fluidRow(column(3, verbatimTextOutput("value"))),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("graphfile", "Choose CV File", accept = ".csv")
        ),
        
        mainPanel(plotOutput("PD")),
        
    )
    
    
)

server <- function(input, output){
    
    
    vert_x <- c(0, 1, 2, 3, 4)
    vert_y <- c(2, 0, 2, 0, 2)
    vert_lab <- c("v1", "v2", "v3", "v4", "v5")
    verts <- data.frame(vert_x, vert_y, vert_lab, stringsAsFactors = F)
    edges <- list(
        make_edge("v1", "v2"),
        make_edge("v2", "v3"),
        make_edge("v3", "v4"),
        make_edge("v4", "v5")
    )
    diagLim <- max(c(vert_x, vert_y)) + 1
    diagLim <- c(-diagLim, diagLim)
    
    output$PD <- renderCachedPlot({
        createPlot(input$num, verts, edges, input$diagonals, diagLim)
        
        
        
    },
    cacheKeyExpr = {input$num}
    )
}

shinyApp(ui = ui, server = server)
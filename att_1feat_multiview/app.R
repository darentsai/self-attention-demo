library(shiny)

att_3view <- function(x, wQ, bQ,
                wK1, bK1, wV1, bV1,
                wK2, bK2, wV2, bV2,
                wK3, bK3, wV3, bV3, sf) {
  Q <-  wQ  * x + bQ
  K1 <- wK1 * x + bK1
  V1 <- wV1 * x + bV1
  K2 <- wK2 * x + bK2
  V2 <- wV2 * x + bV2
  K3 <- wK3 * x + bK3
  V3 <- wV3 * x + bV3
  
  A <- cbind(Q * K1, Q * K2, Q * K3) / sf
  W <- proportions(exp(A - apply(A, 1, max)), margin = 1)
  V <- cbind(V1, V2, V3)
  out <- rowSums(V * W)
  attr(out, "value") <- V
  attr(out, "weight") <- W
  
  return(out)
}

ui <- fluidPage(
    tags$head(tags$style(HTML(".shiny-split-layout { margin-bottom: -15px !important; }"))),
    titlePanel("1-Feature Multi-View Attention"),
    withMathJax(),
    sidebarLayout(
        sidebarPanel(
          splitLayout(
            p("\\(Q\\)"),
            sliderInput("wQ", "Weight", min = -5, max = 5, value = 1, step = 0.1),
            sliderInput("bQ", "Bias", min = -5, max = 5, value = 0, step = 0.1),
            cellWidths = c("4%", "48%", "48%"), cellArgs = list(style = "padding: 2px; vertical-align: middle;")
          ),
          hr(style = "border-top: 1px solid #999999;"),
          h4(em("View 1")),
          splitLayout(
            p("\\(K\\)"),
            sliderInput("wK1", NULL, min = -5, max = 5, value = 1, step = 0.1),
            sliderInput("bK1", NULL, min = -5, max = 5, value = 0, step = 0.1),
            cellWidths = c("4%", "48%", "48%"), cellArgs = list(style = "padding: 2px; vertical-align: middle;")
          ),
          splitLayout(
            p("\\(V\\)"),
            sliderInput("wV1", NULL, min = -5, max = 5, value = 1, step = 0.1),
            sliderInput("bV1", NULL, min = -5, max = 5, value = 0, step = 0.1),
            cellWidths = c("4%", "48%", "48%"), cellArgs = list(style = "padding: 2px; vertical-align: middle;")
          ),
          hr(style = "border-top: 1px solid #999999;"),
          h4(em("View 2")),
          splitLayout(
            p("\\(K\\)"),
            sliderInput("wK2", NULL, min = -5, max = 5, value = 1, step = 0.1),
            sliderInput("bK2", NULL, min = -5, max = 5, value = 0, step = 0.1),
            cellWidths = c("4%", "48%", "48%"), cellArgs = list(style = "padding: 2px; vertical-align: middle;")
          ),
          splitLayout(
            p("\\(V\\)"),
            sliderInput("wV2", NULL, min = -5, max = 5, value = -1, step = 0.1),
            sliderInput("bV2", NULL, min = -5, max = 5, value = 0, step = 0.1),
            cellWidths = c("4%", "48%", "48%"), cellArgs = list(style = "padding: 2px; vertical-align: middle;")
          ),
          hr(style = "border-top: 1px solid #999999;"),
          h4(em("View 3")),
          splitLayout(
            p("\\(K\\)"),
            sliderInput("wK3", NULL, min = -5, max = 5, value = 1, step = 0.1),
            sliderInput("bK3", NULL, min = -5, max = 5, value = 0, step = 0.1),
            cellWidths = c("4%", "48%", "48%"), cellArgs = list(style = "padding: 2px; vertical-align: middle;")
          ),
          splitLayout(
            p("\\(V\\)"),
            sliderInput("wV3", NULL, min = -5, max = 5, value = 0.5, step = 0.1),
            sliderInput("bV3", NULL, min = -5, max = 5, value = 2, step = 0.1),
            cellWidths = c("4%", "48%", "48%"), cellArgs = list(style = "padding: 2px; vertical-align: middle;")
          ),
          hr(style = "border-top: 1px solid #999999;"),
          sliderInput("sf", "Scaling factor (divisor)", min = 0.1, max = 5, value = 1, step = 0.1)
        ),
        mainPanel(
           plotOutput("lineChart", height = 850)
        )
    )
)

# standardized scale
lim <- 3
grid_x <- seq(-lim, lim, length.out = 100)

server <- function(input, output) {

    output$lineChart <- renderPlot({
      
      res <- att_3view(grid_x, sf = input$sf, input$wQ, input$bQ,
                       input$wK1, input$bK1, input$wV1, input$bV1,
                       input$wK2, input$bK2, input$wV2, input$bV2,
                       input$wK3, input$bK3, input$wV3, input$bV3)
      
      layout(matrix(1:2, nrow = 2), heights = c(1.5, 1))
      par(mar = c(0, 4.2, 0.1, 15), cex = 1.3)
      matplot(grid_x, attr(res, "value"), type = 'l', col = 2:4, lty = 2:4, lwd = 3, ylab = "Output")
      lines(grid_x, res, lwd = 3)
      grid(); axis(1, -lim:lim)
      legend("bottomright", legend = 1:3, title = "View", col = 2:4, lty = 2:4, lwd = 3, title.font = 2,
             inset = c(-0.15, 0), xpd = TRUE)
      par(mar = c(4.2, 4.2, 3, 15))
      matplot(grid_x, attr(res, "weight"), type = 'l', col = 2:4, lty = 2:4, lwd = 3,
              xlab = "x", ylab = "Attention Score")
      grid(); axis(1, -5:5)
    })
}

# Run the application
shinyApp(ui = ui, server = server)

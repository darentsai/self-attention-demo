library(shiny)
library(ggplot2)

risk_score <- function(x1, x2, w1, w2, bias) {
  x1 * w1 + x2 * w2 + bias
}

ui <- fluidPage(
    tags$head(tags$style(HTML(".shiny-split-layout { margin-bottom: -15px !important; }"))),
    titlePanel("Individual-wise Attention"),
    withMathJax(),
    sidebarLayout(
        sidebarPanel(
          h4(strong("Risk Score－1")),
          splitLayout(p("\\(w_1\\)"), sliderInput("w1_rs1", NULL, min = -5, max = 5, value = 1, step = 0.1),
                      cellWidths = c("10%", "90%"), cellArgs = list(style = "vertical-align: middle;")),
          splitLayout(p("\\(w_2\\)"), sliderInput("w2_rs1", NULL, min = -5, max = 5, value = 0, step = 0.1),
                      cellWidths = c("10%", "90%"), cellArgs = list(style = "vertical-align: middle;")),
          splitLayout(p("(bias)"), sliderInput("w0_rs1", NULL, min = -5, max = 5, value = 0, step = 0.1),
                      cellWidths = c("10%", "90%"), cellArgs = list(style = "vertical-align: middle;")),
          h4(strong("Risk Score－2")),
          splitLayout(p("\\(w_1\\)"), sliderInput("w1_rs2", NULL, min = -5, max = 5, value = 0, step = 0.1),
                      cellWidths = c("10%", "90%"), cellArgs = list(style = "vertical-align: middle;")),
          splitLayout(p("\\(w_2\\)"), sliderInput("w2_rs2", NULL, min = -5, max = 5, value = 1, step = 0.1),
                      cellWidths = c("10%", "90%"), cellArgs = list(style = "vertical-align: middle;")),
          splitLayout(p("(bias)"), sliderInput("w0_rs2", NULL, min = -5, max = 5, value = 0, step = 0.1),
                      cellWidths = c("10%", "90%"), cellArgs = list(style = "vertical-align: middle;")),
          radioButtons("dist", "Distance/Similarity Measure",
                       c("Euclid. Dist." = "dist",
                         "Euclid. Dist. of Risk Scores: \\(-\\Vert p_1 - p_2 \\Vert_2\\)" = "dist_rs",
                         "Dot Prod. of Risk Scores: \\(p_1 \\cdot p_2\\)" = "prod_rs")),
          sliderInput("sf", "Scaling factor (divisor)", min = 0.1, max = 5, value = 1, step = 0.1),
          shinyWidgets::materialSwitch("std", "Standardization", value = FALSE, status = "info", inline = TRUE),
          shinyWidgets::materialSwitch("square", "Squared Dist.", value = FALSE, status = "info", inline = TRUE),
          radioButtons("trans", "Transform",
                       choiceNames = list("Softmax: \\(exp(x)\\)"),
                       choiceValues = list("exp")),
          actionButton("clear_points", "Clear"),
          hr(style = "border-top: 1px solid #999999;"),
          splitLayout(selectInput("palette", "Color palette", choices = hcl.pals(), selected = "OrRd"),
                      checkboxInput("rev", "Reverse color", value = TRUE),
                      cellWidths = c("60%", "40%"), cellArgs = list(style = "padding-right: 5px; vertical-align: middle;")),
          sliderInput("alpha", "Opacity", min = 0, max = 1, value = 0.75, step = 0.05),
          width = 3
        ),
        mainPanel(
           plotOutput("contourPlot", click = "case_point", height = 850)
        )
    )
)

# standardized scale
lim <- 3
grid_x1 <- grid_x2 <- seq(-lim, lim, length.out = 50)
grid <- expand.grid(x1 = grid_x1, x2 = grid_x1)

server <- function(input, output) {
  
  click <- reactiveVal(NULL)
  
  observeEvent(input$case_point, {
    pos <- input$case_point
    new_point <- data.frame(x1 = pos$x, x2 = pos$y)
    click(rbind(click(), new_point))
  })
  
  observeEvent(input$clear_points, {
    click(NULL)
  })
  
  output$contourPlot <- renderPlot({
    
    grid$rs1 <- risk_score(grid$x1, grid$x2, input$w1_rs1, input$w2_rs1, input$w0_rs1)
    grid$rs2 <- risk_score(grid$x1, grid$x2, input$w1_rs2, input$w2_rs2, input$w0_rs2)
    
    pt <- click()
    p <- ggplot(grid, aes(x1, x2)) +
      labs(x = quote(X[1]), y = quote(X[2])) +
      coord_fixed(xlim = c(-lim, lim), ylim = c(-lim, lim)) +
      scale_x_continuous(breaks = -lim:lim) +
      scale_y_continuous(breaks = -lim:lim) +
      theme_bw() +
      theme(axis.title = element_text(size = 20))
    
    if (!is.null(pt)) {
      
      if(input$std & nrow(pt) > 1) {
        pt_s <- scale(pt[c("x1", "x2")])
        colnames(pt_s) <- c("x1_s", "x2_s")
        pt <- cbind(pt, pt_s)
        
        grid_s <- scale(grid[c("x1", "x2")], center = attr(pt_s, "scaled:center"), scale = attr(pt_s, "scaled:scale"))
        colnames(grid_s) <- c("x1_s", "x2_s")
        grid <- cbind(grid, grid_s)
        
        grid$rs1 <- risk_score(grid$x1_s, grid$x2_s, input$w1_rs1, input$w2_rs1, input$w0_rs1)
        grid$rs2 <- risk_score(grid$x1_s, grid$x2_s, input$w1_rs2, input$w2_rs2, input$w0_rs2)
        
        # Update grid data
        p <- p %+% grid
      } else {
        grid[c("x1_s", "x2_s")] <- grid[c("x1", "x2")]
        pt[c("x1_s", "x2_s")] <- pt[c("x1", "x2")]
      }
      
      if(input$dist == "dist") {
        d <- as.matrix(pdist::pdist(grid[c("x1_s", "x2_s")], pt[c("x1_s", "x2_s")]))
        if(input$square) d <- d^2
        sim <- d * (-1)
      } else {
        pt$rs1 <- risk_score(pt$x1_s, pt$x2_s, input$w1_rs1, input$w2_rs1, input$w0_rs1)
        pt$rs2 <- risk_score(pt$x1_s, pt$x2_s, input$w1_rs2, input$w2_rs2, input$w0_rs2)
        
        if(input$dist == "dist_rs") {
          d <- as.matrix(pdist::pdist(grid[c("rs1", "rs2")], pt[c("rs1", "rs2")]))
          if(input$square) d <- d^2
          sim <- d * (-1)
        } else if(input$dist == "prod_rs") {
          sim <- as.matrix(grid[c("rs1", "rs2")]) %*% t(pt[c("rs1", "rs2")])
          sim <- sim / input$sf
          sim <- sim - apply(sim, 1, max)
        }
      }
      
      if(input$trans == "exp") {
        att <- proportions(exp(sim), margin = 1)
      }
      
      colnames(att) <- sprintf("Case%02d", seq_len(ncol(att)))
      stopifnot(all(abs(rowSums(att) - 1) < 1e-9))
      
      grid2 <- cbind(grid, att)
      grid2_long <- tidyr::pivot_longer(grid2, starts_with("Case"), names_to = "case", values_to = "weight")
      pt$id <- seq_len(nrow(pt))
      
      if(ncol(att) > 1) {
        p <- p +
          geom_contour_filled(aes(z = weight), data = grid2_long, alpha = input$alpha,
                              breaks = c(-Inf, seq(0.1, 0.9, 0.1), Inf)) + # weights may overflow
          scale_fill_manual(name = "Weight",
                            values = hcl.colors(10, input$palette, rev = input$rev),
                            labels = \(x) sub("\\(([0-9.]+), ([0-9.]+)\\]", "\\1－\\2",
                                              sub("Inf", "1.0",
                                                  sub("-Inf", "0.0", x))),
                            guide = guide_legend(reverse = TRUE)) +
          facet_wrap(~ case)
      }
      
      p <- p +
        geom_point(data = pt, size = 3, shape = 21, fill = "cyan", colour = "blue") +
        geom_text(aes(label = id), data = pt, vjust = -1, fontface = 2, color = "blue")
    }
    
    if(var(grid$rs1) > 0)
      p <- p + geomtextpath::geom_textcontour(aes(z = rs1), colour = "blue", alpha = input$alpha)
    if(var(grid$rs2) > 0)
      p <- p + geomtextpath::geom_textcontour(aes(z = rs2), colour = "green3", alpha = input$alpha)
    
    return(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

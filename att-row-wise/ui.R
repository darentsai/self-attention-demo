navbarPage(
  title = "Row-wise Attention",
  theme = bs_theme(preset = "flatly"),
  header = list(
    withMathJax(),
    tags$head(tags$style(HTML(
      ".shiny-split-layout {
        margin-bottom: -20px !important;
      }
      .irs-bar, .irs-line {
        height: 4px !important;
        top: 30px !important;
      }
      .irs-bar {
        background: #1EB18A !important;
      }
      .irs-single, .irs-from, .irs-to, .irs-min, .irs-max {
        top: 6px !important;
      }
      .irs-handle {
        width: 12px !important;
        height: 12px !important;
        top: 25px !important;
        background-color: #1EB18A !important;
        border: 1px solid black !important;
      }")))
  ),
  navbarMenu(
    title = "Idea",
    tabPanel(
      title = "Attention Score",
      sidebarLayout(
        sidebarPanel(
          h5(strong("Risk Score 1")),
          splitLayout(p("\\(w_1\\)"), sliderInput("w1_rs1", NULL, min = -5, max = 5, value = 1, step = 0.1),
                      cellWidths = c("12%", "88%"), cellArgs = list(style = "vertical-align: middle;")),
          splitLayout(p("\\(w_2\\)"), sliderInput("w2_rs1", NULL, min = -5, max = 5, value = 0, step = 0.1),
                      cellWidths = c("12%", "88%"), cellArgs = list(style = "vertical-align: middle;")),
          splitLayout(p("(bias)"), sliderInput("w0_rs1", NULL, min = -5, max = 5, value = 0, step = 0.1),
                      cellWidths = c("12%", "88%"), cellArgs = list(style = "vertical-align: middle;")),
          h5(strong("Risk Score 2")),
          splitLayout(p("\\(w_1\\)"), sliderInput("w1_rs2", NULL, min = -5, max = 5, value = 0, step = 0.1),
                      cellWidths = c("12%", "88%"), cellArgs = list(style = "vertical-align: middle;")),
          splitLayout(p("\\(w_2\\)"), sliderInput("w2_rs2", NULL, min = -5, max = 5, value = 1, step = 0.1),
                      cellWidths = c("12%", "88%"), cellArgs = list(style = "vertical-align: middle;")),
          splitLayout(p("(bias)"), sliderInput("w0_rs2", NULL, min = -5, max = 5, value = 0, step = 0.1),
                      cellWidths = c("12%", "88%"), cellArgs = list(style = "vertical-align: middle;")),
          hr(),
          radioButtons("dist", strong("Distance/Similarity Measure"),
                       c("Euclid. Dist." = "dist",
                         "Euclid. Dist. of Risk Scores: \\(\\text{-} \\Vert p_1 \\text{-} p_2 \\Vert_2\\)" = "dist_rs",
                         "Dot Prod. of Risk Scores: \\(p_1 \\cdot p_2\\)" = "prod_rs")),
          shinyWidgets::materialSwitch("std", "Standardization", value = FALSE, status = "info", inline = TRUE),
          shinyWidgets::materialSwitch("square", "Squared Dist.", value = FALSE, status = "info", inline = TRUE),
          splitLayout(
            p("Scaling factor", br(), "(divisor)"),
            sliderInput("sf", NULL, min = 0, max = 5, value = 1.4, step = 0.1),
            cellWidths = c("30%", "70%"), cellArgs = list(style = "vertical-align: middle;")
          ),
          actionButton("clear_points", "Clear"),
          hr(),
          selectInput("palette", "Color palette", selected = "Reds 3", choices = hcl.pals()),
          checkboxInput("rev", "Reverse color", value = TRUE),
          splitLayout(
            p("Opacity"),
            sliderInput("alpha", NULL, min = 0, max = 1, value = 0.5, step = 0.05),
            cellWidths = c("20%", "80%"), cellArgs = list(style = "vertical-align: middle;")
          ),
          width = 3
        ),
        mainPanel(
          plotOutput("idea_plot", click = "case_point", height = 850),
          width = 9
        )
      )
    ),
    tabPanel(
      title = "Distance",
      sidebarLayout(
        sidebarPanel(
          h5(strong("Risk Score 1"), "\\((Z_1)\\)"),
          splitLayout(p("\\(w_1\\)"), sliderInput("w1_rs1x", NULL, min = -5, max = 5, value = 1, step = 0.1),
                      cellWidths = c("12%", "88%"), cellArgs = list(style = "vertical-align: middle;")),
          splitLayout(p("\\(w_2\\)"), sliderInput("w2_rs1x", NULL, min = -5, max = 5, value = 0, step = 0.1),
                      cellWidths = c("12%", "88%"), cellArgs = list(style = "vertical-align: middle;")),
          splitLayout(p("(bias)"), sliderInput("w0_rs1x", NULL, min = -5, max = 5, value = 0, step = 0.1),
                      cellWidths = c("12%", "88%"), cellArgs = list(style = "vertical-align: middle;")),
          shinyWidgets::materialSwitch("relu_rs1x", "ReLu", value = FALSE, status = "info", inline = FALSE),
          hr(),
          h5(strong("Risk Score 2"), "\\((Z_2)\\)"),
          splitLayout(p("\\(w_1\\)"), sliderInput("w1_rs2x", NULL, min = -5, max = 5, value = 0, step = 0.1),
                      cellWidths = c("12%", "88%"), cellArgs = list(style = "vertical-align: middle;")),
          splitLayout(p("\\(w_2\\)"), sliderInput("w2_rs2x", NULL, min = -5, max = 5, value = 1, step = 0.1),
                      cellWidths = c("12%", "88%"), cellArgs = list(style = "vertical-align: middle;")),
          splitLayout(p("(bias)"), sliderInput("w0_rs2x", NULL, min = -5, max = 5, value = 0, step = 0.1),
                      cellWidths = c("12%", "88%"), cellArgs = list(style = "vertical-align: middle;")),
          shinyWidgets::materialSwitch("relu_rs2x", "ReLu", value = FALSE, status = "info", inline = FALSE),
          hr(),
          h5(strong("Risk Score 3"), "\\((Z_3)\\)"),
          splitLayout(p("\\(w_1\\)"), sliderInput("w1_rs3x", NULL, min = -5, max = 5, value = 0, step = 0.1),
                      cellWidths = c("12%", "88%"), cellArgs = list(style = "vertical-align: middle;")),
          splitLayout(p("\\(w_2\\)"), sliderInput("w2_rs3x", NULL, min = -5, max = 5, value = 0, step = 0.1),
                      cellWidths = c("12%", "88%"), cellArgs = list(style = "vertical-align: middle;")),
          splitLayout(p("(bias)"), sliderInput("w0_rs3x", NULL, min = -5, max = 5, value = 0, step = 0.1),
                      cellWidths = c("12%", "88%"), cellArgs = list(style = "vertical-align: middle;")),
          shinyWidgets::materialSwitch("relu_rs3x", "ReLu", value = FALSE, status = "info", inline = FALSE),
          hr(),
          selectInput("palette.2", "Color palette", selected = "Inferno", choices = hcl.pals()),
          checkboxInput("rev.2", "Reverse color", value = TRUE),
          splitLayout(
            p("Opacity"),
            sliderInput("alpha.2", NULL, min = 0, max = 1, value = 0.5, step = 0.05),
            cellWidths = c("20%", "80%"), cellArgs = list(style = "vertical-align: middle;")
          ),
          width = 3
        ),
        mainPanel(
          plotOutput("idea_plot.2", height = 850),
          width = 9
        )
      )
    )
  ),
  tabPanel(
    title = "Practice",
    sidebarLayout(
      sidebarPanel(
        input_task_button("build", "Build", style = "width: 49%;",
                          label_busy = "Building..."),
        input_task_button("train", "Train", icon = icon("circle-play"), style = "width: 49%;",
                          label_busy = "Training..."),
        hr(),
        selectInput("data", "Data", selected = "Spirals",
                    choices = c("Spirals", "Circle", "Ring", "2 Clusters",
                                "2x2 Grid", "3x3 Grid", "Bivariate Normal")),
        conditionalPanel(
          condition = "input.data == 'Spirals'",
          sliderInput("cycles", "Number of cycles", min = 1, max = 2, value = 1.2, step = 0.1)
        ),
        conditionalPanel(
          condition = "['Circle', 'Ring'].includes(input.data)",
          sliderInput("radius", "Radius", min = 0.5, max = 2, value = 0.7, step = 0.1)
        ),
        conditionalPanel(
          condition = "['Spirals', 'Circle', 'Ring'].includes(input.data)",
          sliderInput("noise", "Noise", min = 0.05, max = 0.5, value = 0.1, step = 0.05)
        ),
        conditionalPanel(
          condition = "input.data == 'Bivariate Normal'",
          sliderInput("sd1", "SD 1", min = 0.1, max = 2, value = 1, step = 0.1),
          sliderInput("sd2", "SD 2", min = 0.1, max = 2, value = 1, step = 0.1),
          sliderInput("angle", "Angle", min = 0, max = 90, value = 0, step = 5)
        ),
        hr(),
        radioButtons("kernel", "Kernel",
                     c("Dot-product Attention" = "dp",
                       "RBF Kernel" = "rbf")),
        shinyWidgets::materialSwitch("key_augment", "Augment Key with Y", value = TRUE, status = "info", inline = FALSE),
        hr(),
        sliderInput("proj_dim", "Projected Dimension", min = 2, max = 10, value = 5, step = 1),
        sliderInput("n_head", "Number of Heads", min = 0, max = 5, value = 1, step = 1),
        numericInput("lr", label = "Learning Rate", min = 0.001, max = 1, value = 0.01, step = 0.002),
        shinyWidgets::sliderTextInput("n_batch", "Batch Size", selected = 32L, choices = 2^(0:7), grid = TRUE),
        sliderInput("n_epoch", "Number of Epochs", min = 0, max = 100, value = 20, step = 5),
        sliderInput("frac_val", "Fraction of Validation Set", min = 0, max = 0.5, value = 0.2, step = 0.05),
        width = 3
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Demo", plotOutput("demo_plot", width = 850, height = 750)),
          tabPanel("Attention", plotOutput("att_plot", click = "att_point",
                                           width = 850, height = 750)),
          tabPanel("Loss", plotOutput("loss_plot", width = 850, height = 750)),
          tabPanel("Info", verbatimTextOutput("info"))
        ),
        width = 9
      )
    )
  )
)

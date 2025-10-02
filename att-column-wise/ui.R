navbarPage(
  title = "Column-wise Attention",
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
      }
      .tooltip-inner {
        max-width: none;
      }")))
  ),
  tabPanel(
    title = "Idea",
    sidebarLayout(
      sidebarPanel(
        splitLayout(
          p("\\(Q\\)"),
          sliderInput("wQ", strong("Weight"), min = -5, max = 5, value = 1, step = 0.1, width = "95%"),
          sliderInput("bQ", strong("Bias"), min = -5, max = 5, value = 0, step = 0.1, width = "95%"),
          cellWidths = c("6%", "47%", "47%"), cellArgs = list(style = "vertical-align: middle;")
        ),
        hr(),
        h5(em(strong("Axis 1"))),
        splitLayout(
          p("\\(K_1\\)"),
          sliderInput("wK1", NULL, min = -5, max = 5, value = 1, step = 0.1, width = "95%"),
          sliderInput("bK1", NULL, min = -5, max = 5, value = 0, step = 0.1, width = "95%"),
          cellWidths = c("6%", "47%", "47%"), cellArgs = list(style = "vertical-align: middle;")
        ),
        splitLayout(
          p("\\(V_1\\)"),
          sliderInput("wV1", NULL, min = -5, max = 5, value = 1, step = 0.1, width = "95%"),
          sliderInput("bV1", NULL, min = -5, max = 5, value = 0, step = 0.1, width = "95%"),
          cellWidths = c("6%", "47%", "47%"), cellArgs = list(style = "vertical-align: middle;")
        ),
        hr(),
        h5(em(strong("Axis 2"))),
        splitLayout(
          p("\\(K_2\\)"),
          sliderInput("wK2", NULL, min = -5, max = 5, value = 1, step = 0.1, width = "95%"),
          sliderInput("bK2", NULL, min = -5, max = 5, value = 0, step = 0.1, width = "95%"),
          cellWidths = c("6%", "47%", "47%"), cellArgs = list(style = "vertical-align: middle;")
        ),
        splitLayout(
          p("\\(V_2\\)"),
          sliderInput("wV2", NULL, min = -5, max = 5, value = -1, step = 0.1, width = "95%"),
          sliderInput("bV2", NULL, min = -5, max = 5, value = 0, step = 0.1, width = "95%"),
          cellWidths = c("6%", "47%", "47%"), cellArgs = list(style = "vertical-align: middle;")
        ),
        hr(),
        h5(em(strong("Axis 3"))),
        splitLayout(
          p("\\(K_3\\)"),
          sliderInput("wK3", NULL, min = -5, max = 5, value = 1, step = 0.1, width = "95%"),
          sliderInput("bK3", NULL, min = -5, max = 5, value = 0, step = 0.1, width = "95%"),
          cellWidths = c("6%", "47%", "47%"), cellArgs = list(style = "vertical-align: middle;")
        ),
        splitLayout(
          p("\\(V_3\\)"),
          sliderInput("wV3", NULL, min = -5, max = 5, value = 0.5, step = 0.1, width = "95%"),
          sliderInput("bV3", NULL, min = -5, max = 5, value = 2, step = 0.1, width = "95%"),
          cellWidths = c("6%", "47%", "47%"), cellArgs = list(style = "vertical-align: middle;")
        ),
        hr(),
        splitLayout(
          p("Scaling factor", br(), "(divisor)"),
          sliderInput("sf", NULL, min = 0, max = 5, value = 1, step = 0.1),
          cellWidths = c("25%", "75%"), cellArgs = list(style = "vertical-align: middle;")
        ),
      ),
      mainPanel(
        plotOutput("idea_plot", width = "90%", height = 800)
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
        sliderInput("n_axis", "Number of Axes", min = 1, max = 10, value = 5, step = 1),
        radioButtons("str", "Structure", choiceNames = list(
          tagList("Separate affine transformations for \\((K, V)\\)",
                  tooltip(bsicons::bs_icon("info-circle"),
                          "Number of trainable paremeters", br(),
                          "\\( [Q]2 + [K](2 \\times axis) + [V](2 \\times axis) \\)", br(),
                          "\\( = 4 \\times axis + 2 \\)")),
          tagList("Shared affine transformation for \\((K, V)\\)",
                  tooltip(bsicons::bs_icon("info-circle"),
                          "Number of trainable paremeters", br(),
                          "\\( 2 \\times axis + head \\times ([Q]2 + [K]2 + [V]2) + [O](head + 1) \\)", br(),
                          "\\( = 2 \\times axis + 7 \\times head + 1 \\)"))
        ), choiceValues = list(1, 2)),
        conditionalPanel(
          condition = "input.str == 2",
          sliderInput("n_head", "Number of Heads", min = 0, max = 5, value = 1, step = 1)
        ),
        radioButtons("kernel", "Kernel",
                     c("Dot-product Attention" = "dp",
                       "RBF Kernel" = "rbf")),
        numericInput("lr", label = "Learning Rate", min = 0.001, max = 1, value = 0.01, step = 0.002),
        shinyWidgets::sliderTextInput("n_batch", "Batch Size", selected = 32L, choices = 2^(0:7), grid = TRUE),
        sliderInput("n_epoch", "Number of Epochs", min = 0, max = 100, value = 20, step = 5),
        sliderInput("frac_val", "Fraction of Validation Set", min = 0, max = 0.5, value = 0.2, step = 0.05)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Demo", plotOutput("demo_plot", width = 750, height = 700)),
          tabPanel("Loss", plotOutput("loss_plot", width = 750, height = 700)),
          tabPanel("Info", verbatimTextOutput("info"))
        )
      )
    )
  )
)

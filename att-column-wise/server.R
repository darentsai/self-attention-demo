function(input, output, session) {
  
  #--- Idea ---#
  
  # standardized scale
  lim <- 3
  grid_x <- seq(-lim, lim, length.out = 100)
  
  output$idea_plot <- renderPlot({
    res <- att_3axis(grid_x, sf = input$sf, input$wQ, input$bQ,
                     input$wK1, input$bK1, input$wV1, input$bV1,
                     input$wK2, input$bK2, input$wV2, input$bV2,
                     input$wK3, input$bK3, input$wV3, input$bV3)
    layout(matrix(1:2, nrow = 2), heights = c(1.5, 1))
    par(mar = c(0, 4.2, 0.1, 15), cex = 1.3)
    matplot(grid_x, attr(res, "value"), type = 'l', col = 2:4, lty = 2:4, lwd = 3,
            xaxt = "n", ylab = "Output")
    lines(grid_x, res, lwd = 3)
    grid(lwd = 1.5)
    legend("bottomright", legend = 1:3, title = "Axis", col = 2:4, lty = 2:4, lwd = 3, title.font = 2,
           inset = c(-0.15, 0), xpd = TRUE)
    par(mar = c(4.2, 4.2, 0.5, 15))
    matplot(grid_x, attr(res, "weight"), type = 'l', col = 2:4, lty = 2:4, lwd = 3,
            xlab = "x", ylab = "Attention Score")
    grid(lwd = 1.5); axis(1, -lim:lim)
  })
  
  #--- Practice ---#
  
  observe({
    if (input$sf == 0)
      updateSliderInput(session, "sf", value = 0.1)
    if (input$n_head < 1)
      updateSliderInput(session, "n_head", value = 1)
    if (input$n_epoch < 1)
      updateSliderInput(session, "n_epoch", value = 1)
  })
  
  n <- 1000
  x <- matrix(runif(n, -11, 11))
  y <- sin(x) * 2 + 0.5 * x + rnorm(n)
  grid <- matrix(seq(-11, 11, by = 0.1))
  
  init_plot <- function(axis = NULL, epoch = 0) {
    par(mar = c(5, 5, 1, 0))
    plot(x, y, asp = 1, type = 'n'); grid(lwd = 1.5)
    if(!is.null(axis)) Map(\(a, b) abline(a, b, col = 3, lwd = 2), axis[[2]], axis[[1]])
    curve(sin(x) * 2 + 0.5 * x, add = TRUE, col = 4, lwd = 2)
    points(x, y, cex = 0.7)
    legend("topleft", paste("epoch:", epoch), cex = 2, bty = "n", x.intersp = 0)
  }
  
  output$demo_plot <- renderPlot({
    init_plot()
  })
  
  model_update <- reactiveVal(NULL)
  epoch_update <- reactiveVal(NULL)
  
  observe({
    std_layer <- layer_normalization(axis = -1L, name = "std_layer")
    std_layer %>% adapt(x)
    inputs <- layer_input(shape = c(1L))
    inputs_std <- inputs %>% std_layer()
    
    if(input$str == 1) {
      
      q <- inputs_std %>% 
        layer_dense(units = 1, activation = "linear", name = "query_dense") %>%
        layer_reshape(c(1L, 1L))
      k <- inputs_std %>% 
        layer_dense(units = input$n_axis, activation = "linear", name = "key_dense") %>%
        layer_reshape(c(input$n_axis, 1L))
      v <- inputs_std %>% 
        layer_dense(units = input$n_axis, activation = "linear", name = "value_dense") %>%
        layer_reshape(c(input$n_axis, 1L))
      
      att_layer <- layer_multi_head_attention_rbf(
        num_heads = 1L, key_dim = 1L,
        use_bias = FALSE, kernel_initializer = initializer_ones()
      )
      
      outputs <- att_layer(query = q, key = k, value = v,
                           rbf = (input$kernel == "rbf")) %>%
        layer_flatten()
      
      for(layer in c("query", "key", "value", "output")) {
        att_layer[[paste0(layer, "_dense")]]$trainable <- FALSE
      }
      
    } else if(input$str == 2) {
      
      att_layer <- layer_multi_head_attention_rbf(
        num_heads = input$n_head, key_dim = 1L
      )
      
      outputs <- inputs_std %>%
        layer_dense(units = input$n_axis, activation = "linear") %>%
        layer_reshape(c(input$n_axis, 1L)) %>%
        att_layer(query = layer_lambda(., \(x) x[, 1, , drop = FALSE]),
                  key = ., value = .,
                  rbf = (input$kernel == "rbf")) %>%
        layer_flatten()
    }
    
    model <- keras_model(inputs, outputs)
    model_update(model)
    epoch_update(0L)
    
    output$info <- renderPrint({ summary(model) })
    output$demo_plot <- renderPlot({ init_plot() })
  }) |>
    bindEvent(input$build)
  
  observe({
    if(input$build) {
      model <- model_update()
      
      model %>% compile(
        optimizer = optimizer_adam(learning_rate = input$lr),
        loss = "mean_squared_error"
      )
      
      history <- model %>% fit(
        x, y,
        epochs = input$n_epoch, batch_size = input$n_batch,
        validation_split = input$frac_val,
        verbose = 0, view_metrics = 0
      )
      
      model_update(model)
      epoch_update(epoch_update() + input$n_epoch)
      
      pred <- model %>%
        predict(grid, verbose = 0)
      
      if(input$str == 1) {
        x_mean <- as.numeric(get_layer(model, "std_layer")$mean)
        x_var <- as.numeric(get_layer(model, "std_layer")$variance)
        coef_v <- get_weights(get_layer(model, "value_dense"))
        coef_v[[1]] <- coef_v[[1]] / sqrt(x_var)
        coef_v[[2]] <- coef_v[[2]] - c(coef_v[[1]] * x_mean)
      } else {
        coef_v <- NULL
      }
      
      output$demo_plot <- renderPlot({
        init_plot(axis = coef_v, epoch = epoch_update())
        lines(grid, pred, col = 2, lwd = 2)
      })
      
      output$loss_plot <- renderPlot({
        plot(history, method = "ggplot2", smooth = FALSE) +
          geom_line(linewidth = 1) +
          geom_point(shape = 21, size = 2, colour = "black") +
          theme(text = element_text(size = 15))
      })
    } else {
      showNotification("Please build a model!",
                       type = "error", duration = 1)
    }
  }) |>
    bindEvent(input$train)
}

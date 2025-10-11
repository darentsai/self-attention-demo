function(input, output, session) {

  #--- Idea ---#
  
  # standardized scale
  lim <- 3
  grid_x1 <- grid_x2 <- seq(-lim, lim, length.out = 50)
  grid <- expand.grid(x1 = grid_x1, x2 = grid_x1)
  
  set_pt <- reactiveVal(NULL)
  
  observeEvent(input$case_point, {
    pos <- input$case_point
    new_point <- data.frame(x1 = pos$x, x2 = pos$y)
    set_pt(rbind(set_pt(), new_point))
  })
  
  observeEvent(input$clear_points, {
    set_pt(NULL)
  })
  
  output$idea_plot <- renderPlot({
    
    grid$rs1 <- risk_score(grid$x1, grid$x2, input$w1_rs1, input$w2_rs1, input$w0_rs1)
    grid$rs2 <- risk_score(grid$x1, grid$x2, input$w1_rs2, input$w2_rs2, input$w0_rs2)
    
    pt <- set_pt()
    p <- ggplot(grid, aes(x1, x2)) +
      labs(x = quote(X[1]), y = quote(X[2])) +
      coord_fixed(xlim = c(-lim, lim), ylim = c(-lim, lim)) +
      scale_x_continuous(breaks = -lim:lim) +
      scale_y_continuous(breaks = -lim:lim) +
      theme_bw(base_size = 13) +
      theme(title = element_text(size = 20),
            legend.key.height = unit(4, "cm"))
    
    if(!is.null(pt)) {
      
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
        p <- p + list(grid)
        
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
        }
      }
      
      sim <- sim / input$sf
      sim <- sim - apply(sim, 1, max)
      att <- proportions(exp(sim), margin = 1)
      colnames(att) <- sprintf("Case%02d", seq_len(ncol(att)))
      stopifnot(all(abs(rowSums(att) - 1) < 1e-9))
      
      grid_long <- cbind(grid, att) %>%
        tidyr::pivot_longer(starts_with("Case"), names_to = "case", values_to = "weight")
      
      pt$id <- seq_len(nrow(pt))
      
      if(ncol(att) > 1) {
        p <- p +
          geom_contour_filled(aes(z = weight), data = grid_long,
                              binwidth = 0.05, alpha = input$alpha) +
          scale_fill_discrete(name = "Weight",
                              palette = \(n) hcl.colors(n, input$palette, rev = input$rev),
                              guide = guide_coloursteps()) +
          facet_wrap(~ case)
      }
      
      p <- p +
        geom_point(data = pt, size = 2.5, shape = 21, fill = "gold", colour = "black") +
        geom_text(aes(label = id), data = pt, vjust = -1, size = 5, color = "black")
    }
    
    if(var(grid$rs1) > 0)
      p <- p + geomtextpath::geom_textcontour(
        aes(z = rs1), colour = "blue", alpha = input$alpha
      )
    if(var(grid$rs2) > 0)
      p <- p + geomtextpath::geom_textcontour(
        aes(z = rs2), colour = "green2", alpha = input$alpha
      )
    
    return(p)
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
  
  set_x <- reactiveVal(NULL)
  set_y <- reactiveVal(NULL)
  set_xval <- reactiveVal(NULL)
  set_yval <- reactiveVal(NULL)
  set_grid2 <- reactiveVal(NULL)
  set_pt2 <- reactiveVal(NULL)
  
  observeEvent(input$att_point, {
    pos <- input$att_point
    new_point <- data.frame(x1 = pos$x, x2 = pos$y)
    set_pt2(new_point)
  })
  
  observe({
    if(input$data == "Spirals") {
      dat <- mlbench::mlbench.spirals(2500, cycles = input$cycles, sd = input$noise)
      x <- dat$x
      y <- sapply(levels(dat$classes), \(x) +(dat$classes == x))
    } else if(input$data == "Circle") {
      x <- MASS::mvrnorm(3000, c(0, 0), diag(2, 2))
      x <- x[rowSums(abs(x) > 3) == 0, ]
      y <- rbinom(nrow(x), size = 1,
                  ring_gauss(x[, 1], x[, 2], r1 = 0, r2 = input$radius, sigma = input$noise))
      y <- cbind(y, 1-y)
    } else if(input$data == "Ring") {
      x <- MASS::mvrnorm(3000, c(0, 0), diag(2, 2))
      x <- x[rowSums(abs(x) > 3) == 0, ]
      y <- rbinom(nrow(x), size = 1,
                  ring_gauss(x[, 1], x[, 2], r1 = input$radius, r2 = input$radius+0.2, sigma = input$noise))
      y <- cbind(y, 1-y)
    } else if(input$data == "2 Clusters") {
      R <- matrix(c(1, -0.5, -0.5, 1), 2)
      V1 <- diag(c(100, 0.1))
      V2 <- diag(c(50, 0.05))
      x <- rbind(
        MASS::mvrnorm(2000, c(1000, 0.1), V1 %*% R %*% V1),
        MASS::mvrnorm(500, c(1150, 0.25), V2 %*% R %*% V2)
      )
      y <- rep(0:1, c(2000, 500))
      y <- cbind(y, 1-y)
    } else if(input$data == "2x2 Grid") {
      x <- cbind(runif(2500, 0, 2), runif(2500, 0, 2))
      y <- +((findInterval(x[, 1], 1) + findInterval(x[, 2], 1)) %in% c(0L, 2L))
      y <- cbind(y, 1-y)
    } else if(input$data == "3x3 Grid") {
      x <- cbind(runif(2500, 0, 3), runif(2500, 0, 3))
      y <- +((findInterval(x[, 1], 1:2) + findInterval(x[, 2], 1:2)) %in% c(0, 2, 4))
      y <- cbind(y, 1-y)
    } else if(input$data == "Bivariate Normal") {
      x <- MASS::mvrnorm(3000, c(0, 0), diag(2, 2))
      x <- x[rowSums(abs(x) > 3) == 0, ]
      y <- rbinom(nrow(x), size = 1, prob = mvdnorm(x, c(0, 0), c(input$sd1, input$sd2),
                                                    angle = input$angle, scale = TRUE))
      y <- cbind(y, 1-y)
    }
    
    val_ind <- sample(nrow(x), nrow(x) * input$frac_val)
    xval <- x[val_ind, ]
    yval <- y[val_ind, ]
    x <- x[-val_ind, ]
    y <- y[-val_ind, ]
    
    # Compare attention scores of different labels at the same location
    if(input$data == "2 Clusters") {
      x <- rbind(x, c(900, 0), c(900, 0),
                 c(1000, 0.1), c(1000, 0.1),
                 c(1100, 0.2), c(1100, 0.2),
                 c(1150, 0.25), c(1150, 0.25),
                 c(1200, 0.3), c(1200, 0.3))
      y <- rbind(y, c(0, 1), c(1, 0),
                 c(0, 1), c(1, 0),
                 c(0, 1), c(1, 0),
                 c(1, 0), c(0, 1),
                 c(1, 0), c(0, 1))
    }
    
    grid2_x1 <- seq(min(x[, 1]), max(x[, 1]), len = 100)
    grid2_x2 <- seq(min(x[, 2]), max(x[, 2]), len = 100)
    grid2 <- as.matrix(expand.grid(grid2_x1, grid2_x2))
    
    set_x(x)
    set_y(y)
    set_xval(xval)
    set_yval(yval)
    set_grid2(grid2)
  })
  
  init_plot <- function(x = set_x(), y = set_y(), bg = list(), epoch = 0,
                        datname = input$data, circle.col = 1) {
    span <- sort(apply(x, 2, \(v) diff(range(v))))
    offset <- sqrt(-log(0.5) * input$noise)
    dat <- setNames(data.frame(x, y[, 1]), c("x1", "x2", "y"))
    
    p <- ggplot(dat, aes(x = x1, y = x2)) +
      bg + geom_point(fill = ifelse(dat$y == 1, 2, 4), shape = 21) +
      labs(x = quote(X[1]), y = quote(X[2]), title = paste("epoch:", epoch)) +
      theme_bw(base_size = 15) +
      theme(text = element_text(size = 20),
            legend.key.height = unit(4, "cm"))
    
    if(!length(bg))
      p <- p + theme(plot.margin = margin(r = 4, unit = "cm"))
    if(span[2] / span[1] < 2)
      p <- p + coord_fixed()
    
    if(datname == "Circle") {
      p <- p +
        ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = input$radius + offset),
                             n = 50, linewidth = 0.2, color = circle.col)
    } else if(datname == "Ring") {
      p <- p +
        ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = max(0, input$radius - offset)),
                             n = 50, linewidth = 0.2, color = circle.col) +
        ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = input$radius + 0.2 + offset),
                             n = 50, linewidth = 0.2, color = circle.col)
    }
    
    return(p)
  }
  
  output$demo_plot <- renderPlot({ init_plot() })
  output$att_plot <- renderPlot({ init_plot() })
  
  set_model <- reactiveVal(NULL)
  set_data <- reactiveVal(NULL)
  set_epoch <- reactiveVal(NULL)
  
  observe({
    x <- set_x()
    y <- set_y()
    n_head <- input$n_head
    
    std_layer <- layer_normalization(axis = -1L, name = "std_layer")
    std_layer %>% adapt(x)
    y2 <- scale(y)
    key <- op_expand_dims(
      cbind(std_layer(x), y2[, 1],
            std_layer(x) * op_tile(op_expand_dims(y2[, 1], -1), tail(shape(.), 1))),
      axis = 1
    )
    
    inputs <- layer_input(shape = c(2L))
    att_layer <- layer_multi_head_attention_rbf(
      num_heads = n_head, key_dim = input$proj_dim,
      value_dim = 2L, output_shape = shape(2L)
    )
    
    outputs <- inputs %>%
      std_layer() %>%
      layer_reshape(c(1L, 2L)) %>%
      att_layer(query = .,
                key = key,
                value = op_expand_dims(y, 1),
                return_attention_scores = TRUE,
                rbf = (input$kernel == "rbf"))
    
    att_vd <- get_weights(att_layer$value_dense)
    for(i in 1:n_head) {
      att_vd[[1]][, i, ] <- diag(2)
    }
    att_vd[[2]][] <- 0
    set_weights(att_layer$value_dense, att_vd)
    att_layer$value_dense$trainable <- FALSE
    
    att_od <- get_weights(att_layer$output_dense)
    for(i in 1:n_head) {
      att_od[[1]][i, , ] <- diag(2) / n_head
    }
    att_od[[2]][] <- 0
    set_weights(att_layer$output_dense, att_od)
    att_layer$output_dense$trainable <- FALSE
    
    # constraint_sum2one <- Constraint("sum2one", call = \(w) {
    #   a <- w[, 1L, 1L]
    #   a <- op_softmax(a)
    #   n_head <- as.integer(shape(w)[1])
    #   
    #   I_array <- op_stack(lapply(1:n_head, \(i) op_eye(2L)))
    #   a_array <- op_reshape(a, c(n_head, 1L, 1L))
    #   a_array * I_array
    # })
    # 
    # att_layer$output_dense$kernel$constraint <- constraint_sum2one()
    # att_layer$output_dense$bias$trainable <- FALSE
    
    model <- keras_model(inputs = inputs,
                         outputs = lapply(outputs, layer_flatten))
    
    set_model(model)
    set_data(list(x, y))
    set_epoch(0L)
    set_pt2(matrix(colMeans(x), nrow = 1))
    
    output$info <- renderPrint({ summary(model) })
    output$demo_plot <- renderPlot({ init_plot() })
    output$att_plot <- renderPlot({ init_plot() })
  }) |>
    bindEvent(input$build)
  
  observe({
    x <- set_x()
    y <- set_y()
    
    if(input$build & identical(set_data(), list(x, y))) {
      model <- set_model()
      model %>% compile(
        optimizer = optimizer_adam(learning_rate = input$lr),
        loss = list("categorical_crossentropy", NULL),
        metrics = list(metric_auc(multi_label = TRUE, num_thresholds = 500), NULL)
      )
      
      xval <- set_xval()
      yval <- set_yval()
      grid2 <- set_grid2()
      
      history <- model %>% fit(
        x, y,
        epochs = input$n_epoch, batch_size = input$n_batch,
        validation_data = list(xval, yval),
        verbose = 0, view_metrics = 0
      )
      
      set_model(model)
      set_epoch(set_epoch() + input$n_epoch)

      pred <- model %>%
        predict(grid2, verbose = 0)
      
      pred_prob <- pred[[1]]
      att_score <- pred[[2]]
      pred_prob <- pmin(pmax(pred_prob, 0), 1)
      
      # Average weights across all heads
      n_head <- input$n_head
      if(n_head > 1) {
        att_score <- Reduce('+', lapply(seq_len(n_head), \(i) {
          att_score[, seq_len(nrow(x)) + nrow(x) * (i - 1), drop = FALSE]
        })) / n_head
      }
      
      datname_built <- input$data
      
      output$demo_plot <- renderPlot({
        bg_dat <- setNames(data.frame(grid2, pred_prob), c("x1", "x2", "z"))
        bg <- list(geom_contour_filled(aes(z = z), data = bg_dat,
                                       breaks = c(-Inf, seq(0.05, 0.95, 0.05), Inf)), # weights may overflow
                   scale_fill_discrete(name = format("Probability", width = 12),
                                       palette = \(n) hcl.colors(n, "Blue-Red 3"),
                                       drop = FALSE, guide = guide_coloursteps()))
        
        init_plot(x = x, y = y, bg = bg, epoch = set_epoch(),
                  datname = datname_built)
      })
      
      output$att_plot <- renderPlot({
        pt2 <- set_pt2()
        xs <- scale(x)
        d <- as.matrix(pdist::pdist(xs, scale(pt2, attr(xs,"scaled:center"), attr(xs,"scaled:scale"))))
        i <- which(d == min(d))
        if(length(i) > 1) i <- sample(i, 1)
        bg_dat <- setNames(data.frame(grid2, att_score[, i]), c("x1", "x2", "z"))
        bg <- list(geom_contour_filled(aes(z = z), data = bg_dat,
                                       breaks = c(seq(0, quantile(att_score, 0.995), len = 20), Inf)),
                   scale_fill_discrete(name = format("Weight", width = 12),
                                       palette = \(n) hcl.colors(n, "Inferno"),
                                       drop = FALSE, guide = guide_coloursteps(),
                                       labels = \(x) sprintf("%.4f", x)))
        
        init_plot(x = x, y = y, bg = bg, epoch = set_epoch(),
                  datname = datname_built, circle.col = "white") +
          geom_point(data = setNames(as.data.frame(x[i, , drop = FALSE]), c("x1", "x2")),
                     shape = 23, size = 5, stroke = 1.5,
                     fill = ifelse(y[i, 1] == 1, "deeppink", "deepskyblue"))
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

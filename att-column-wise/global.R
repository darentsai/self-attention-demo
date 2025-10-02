library(shiny); library(bslib)
library(ggplot2)
library(keras3)
library(reticulate)

source_python(
  "https://raw.githubusercontent.com/darentsai/keras/refs/heads/master/keras/src/layers/attention/multi_head_attention_rbf.py",
  envir = NULL
)
layer_multi_head_attention_rbf <- py$MultiHeadAttentionRBF

att_3axis <- function(x, wQ, bQ,
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

MyAttentionLayer <- Layer(
  classname = "MyAttentionLayer",
  initialize = function(dim) {
    super$initialize()
    self$dim <- dim
  },
  build = function(input_shape) {
    self[["Q_dense"]] <- layer_dense(units = 1)
    for(i in seq_len(self$dim)) {
      self[[paste0("K_dense", i)]] <- layer_dense(units = 1)
      self[[paste0("V_dense", i)]] <- layer_dense(units = 1)
    }
  },
  call = function(x) {
    Q <- self[["Q_dense"]](x)
    K <- lapply(seq_len(self$dim), \(i) self[[paste0("K_dense", i)]](x))
    V <- lapply(seq_len(self$dim), \(i) self[[paste0("V_dense", i)]](x))

    score <- lapply(K, \(ki) op_sum(Q * ki, axis = -1L, keepdims = TRUE))
    scores <- op_concatenate(score, axis = -1L)
    weights <- op_softmax(scores)
    weights <- op_expand_dims(weights, axis = -1L) # [batch, dim] -> [batch, dim, units=1]
    V_stack <- op_stack(V, axis = 2L) # [batch, units=1] -> [batch, dim, units=1]
    output <- op_sum(weights * V_stack, axis = 2L)
    return(output)
  }
)

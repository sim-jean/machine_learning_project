k2ml <- function(X, W, Y, K, SL.library.X, SL.library.Y, family.X = gaussian(), family.Y = gaussian()) {
  require("dplyr")
# This function applies the k-fold debiased machine learning method describe in Chernozukhov (2018). 
# Wrote by Raphaël Huleux for the Machine Learning Class at PSE, 2020, with Hannah Bull and Philip Ketz.

# We set the vectors that we will fill with the main loop
  beta_total = c()
  psi_stack = c()
  res_stack = c()
  
  size <- length(Y)/K # This measures the size of each sample, depending on the number of folds k
  k_index <- split(sample(length(Y)), ceiling(seq_along(Y)/size)) # We take a random indexing and split in k folds
  
  # We create the list that will contain the different folds
  W_k <- vector("list", K)
  X_k <- vector("list", K)
  Y_k <- vector("list", K)
  
  # This loop fills the list with the different vectors
  for (i in 1:K) { 
    W_k[[i]] = W[k_index[[i]],]
    X_k[[i]] = X[k_index[[i]]]
    Y_k[[i]] = Y[k_index[[i]]]
  }

  # We then start the main loop 
  for (i in 1:K) {
    W1 <- W[0,] %>% as.data.frame() 
    X1 <- c()
    Y1 <- c()
    
    for (j in 1:K){
      if(j == i){} # if j = i, we don't use this fold for training and will use it for predicting 
      else{ # if j =/= i, we merge the folds together and will use those to train the model
        W1 <- rbind(W1, as.data.frame(W_k[[j]]))
        X1 <- c(X1, X_k[[j]])
        Y1 <- c(Y1, Y_k[[j]])
      }
    }
    sl_x = SuperLearner(Y = X1, # we estimate E(X|W)
                        X = as.data.frame(W1), 
                        newX= as.data.frame(W_k[[i]]), # We predict with the remaining fold
                        family = family.X, 
                        SL.library = SL.library.X, 
                        cvControl = list(V=0))
    
    sl_y = SuperLearner(Y = Y1, # We estimate E(Y|W)
                        X = as.data.frame(W1), 
                        newX= as.data.frame(W_k[[i]]), # We predict with the remaining fold
                        family = family.Y, 
                        SL.library = SL.library.Y, 
                        cvControl = list(V=0))
    x_hat <- sl_x$SL.predict
    y_hat <- sl_y$SL.predict
    
    res_x <- X_k[[i]] - x_hat # We compute the residual for x and y, i indicating the fold that wasn't used for training the model
    res_y <- Y_k[[i]] - y_hat
    
    beta_i = (mean(res_x*res_y))/(mean(res_x*res_x)) # We apply the Frisch-Waugh theorem
    
    psi_stack <- c(psi_stack, (res_y-res_x*beta_i))
    res_stack <- c(res_stack, res_x)
    
    beta_total = c(beta_total, beta_i) # We add the computed beta to a vector of betas
  }
  
  final_beta <- mean(beta_total) # We compute the mean
  se = sqrt(mean(res_stack^2)^(-2)*mean(res_stack^2*psi_stack^2))/sqrt(length(Y))
  
  return(c(final_beta,se))
}

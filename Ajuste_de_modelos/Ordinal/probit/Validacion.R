# Función para categorizar en la matriz de confusión
categorizar <- function(x, Qp) {
  if (x < Qp[1]) {
    return("1")
  } else if (x >=  Qp[1] && x <  Qp[2]) {
    return("2")
  } else if (x >=  Qp[2] && x <  Qp[3]) {
    return("3")
  } else if (x >=  Qp[3] && x <  Qp[4]) {
    return("4")
  } else {
    return("5")
  }
}

#Funciones para obtener los gráficos de accuracy
get_diagonal <- function(mat, shift) {
  n <- nrow(mat)
  m <- ncol(mat)
  if (shift >= 0) {
    return(diag(mat[ , (1+shift):m, drop = FALSE]))
  } else {
    return(diag(mat[(1-shift):n, , drop = FALSE]))
  }
}


CM_cumsum <- function(conf_matrix){
  cumulative_sums <- numeric()
  max_shift <- nrow(conf_matrix) - 1
  
  for (shift in 0:max_shift) {
    main_diag <- sum(get_diagonal(conf_matrix, shift))  # Above or main diagonal
    off_diag <- if (shift > 0) sum(get_diagonal(conf_matrix, -shift)) else 0  # Below main diagonal
    cumulative_sums <- c(cumulative_sums, sum(cumulative_sums[length(cumulative_sums)], main_diag, off_diag, na.rm = TRUE))
  }
  return(cumulative_sums)
}


validacion = function(fit_mass, fit_ord, hat_X, y){
  ## Multicolinealidad ----
  multicol = car::vif(fit_mass)
  print(multicol)
  
  ## residuales ----
  # Obtain the SBS/probability-scale residuals
  pres <- presid(fit_mass)
  
  p1 <- ggplot(data.frame(y = pres), aes(sample = y)) +
    stat_qq(distribution = qunif, dparams = list(min = -1, max = 1), alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "tomato2", linewidth = 1, linetype= "dashed") +
    xlab("Sample quantile") +
    ylab("Theoretical quantile") + ggtitle("QQ-plot - (SBS)"); print(p1)
  
  set.seed(101) # for reproducibility
  sres <- resids(fit_mass)
  
  p2 <- ggplot(data.frame(y = sres), aes(sample = y)) +
    stat_qq(distribution = qunif, dparams = list(min = -1, max = 1), alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "tomato2", linewidth = 1, linetype= "dashed") +
    xlab("Sample quantile") +
    ylab("Theoretical quantile") + ggtitle("QQ-plot - (Surrogate)"); print(p2)
  
  residual <- grid.arrange(p1, p2, ncol = 2); residual
  
  ## Matriz de confusión ----
  beta <- fit_ord$beta
  hat_y <- hat_X %*% beta
  Qp <- fit_ord$alpha #los puntos de corte interceptos estimados
  
  categorias <- sapply(hat_y, categorizar, Qp)
  pred <- as.data.frame(cbind(y, categorias, hat_y))
  
  #matriz de confusión con los valores reales en las filas, predichos columnas
  CM <- table(pred$y, pred$categorias); print(CM) 
  
  ## Accuracy ----
  Accuracy <- cbind(CM_cumsum(CM)/nrow(hat_X), c(1:5))
  
  plot(y=Accuracy[,1], x=Accuracy[,2])
  plot(x= c(1:5), y=Accuracy[,1], type = "o", col = "blue", pch = 16, ylim = c(0, 1), 
       xlab = "Distancia", ylab = "Presicion", main = "")
  
  results = list(multicol, residual, confusion = CM, Accuracy)
  return(results)
}

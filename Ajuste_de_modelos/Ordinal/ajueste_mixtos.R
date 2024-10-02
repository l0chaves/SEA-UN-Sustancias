valores_ajustados <- qnorm(fitted(fit3K_ord))
summary(valores_ajustados)

# Imprimir los valores ajustados para ver qué obtenemos
print(valores_ajustados)

# Extraer los puntos de corte (thresholds) del modelo
cortes <- fit3K_ord$Theta
cortes <- c(-Inf, cortes, Inf)  # Agregar -Inf y Inf para comparar adecuadamente

# Función para asignar categoría basada en los puntos de corte
asignar_categoria <- function(valor_ajustado, cortes) {
  # Buscar el intervalo en el que cae el valor ajustado
  categoria <- findInterval(valor_ajustado, cortes)
  return(categoria)
}

# Aplicar la función a cada valor ajustado
categorias_predichas <- sapply(valores_ajustados, asignar_categoria, cortes = cortes)
categorias_predichas2 <- categorias(categorias_predichas1)
# Mostrar las categorías predichas
print(categorias_predichas)

categorias <- function(vector) {
  vectorC <- vector
  # Reemplazar 5 por 1 y 4 por 2
  vectorC[vector == 5] <- 1
  vectorC[vector == 4] <- 2
  vectorC[vector == 2] <- 4
  vectorC[vector == 1] <- 5
  return(vectorC)
}

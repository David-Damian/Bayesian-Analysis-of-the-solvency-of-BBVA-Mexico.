# Librerías que usaremos
library(patchwork)
library(tidyverse)
library(kableExtra)
library(R2OpenBUGS)
library(R2jags)
library(gridExtra)
library(pander)
library(readxl)
library(rmarkdown)

library(GGally)
library(zoo)
library(lubridate)
library(data.table)
library(tseries)
library(gridExtra)
library(tseries)
library(ggfortify)
library(FinTS)

# ------------------------------------------------------------------------

# Resumen gráfico de cadenas de Markov
# Grid con: trazas de las cadenas generadas, 
# promedios ergodicos de cada cadena
# histogramas
# correlograma
resumen_grafico <- function(z1,z2, titulo){
chains_beta0 <- tibble(chain1=z1, chain2=z2, iter=1:length(z1)) |> pivot_longer(cols=1:2, names_to='cadena', values_to='valores')|>
arrange(cadena)

# Crear un arreglo 3D vacío con las dimensiones deseadas
x <- array(NA, dim = c(length(z1), 2, 1))

# Asignar las cadenas a cada dimensión
x[, 1, 1] <- z1
x[, 2, 1] <- z2

# Nombrar las dimensiones
dimnames(x) <- list(NULL, c("chain:1", "chain:2"), c("parametro1"))
# trace plots of the betas
color_scheme_set("viridisD")
traza <- mcmc_trace(x, regex_pars = "parametro") +
  labs(title= titulo) +
  xlab('iteracion') +
  ylab('valor') +
  sin_lineas

# Calcular la media acumulada de los datos por cadena
data_cummean <- chains_beta0 |>
  group_by(cadena) |>
  mutate(cummean_valores = cummean(valores))

# Graficar la media acumulada de los datos por cadena
ergodicos <- ggplot(data_cummean, aes(x = iter, y = cummean_valores, color = cadena)) +
  geom_line() +
  sin_lineas +
  labs(x = "Iteraciones", y = "Media acumulada", color = "Cadena") +
  ggtitle("Promedios ergódicos acumulados")

hist1 <- z1 |> as_tibble() |>
        mcmc_hist() +
        labs(x = titulo , y = "Frequency") +
        ggtitle("Histograma de cadena 1") + 
        sin_lineas

hist2 <- z2 |> as_tibble() |>
        mcmc_hist() +
        labs(x = titulo , y = "Frequency") +
        ggtitle("Histograma de cadena 2") + 
        sin_lineas

ac_z1 <- acf(z1, plot = FALSE)

# Convertir la función de autocorrelación en un marco de datos
ac_df1 <- data.frame(lag = ac_z1$lag, acf = ac_z1$acf)

# Graficar la función de autocorrelación utilizando ggplot2
acfz1 <- ggAcf(z1) + sin_lineas
ac_z2 <- acf(z2, plot = FALSE)

# Convertir la función de autocorrelación en un marco de datos
ac_df2 <- data.frame(lag = ac_z2$lag, acf = ac_z2$acf)

# Graficar la función de autocorrelación utilizando ggplot2
acfz2 <- ggAcf(z2) + sin_lineas

grid.arrange(traza, ergodicos, hist1, hist2, acfz1, acfz2, nrow = 3)
}

# Tablas en formato Latex a partir de un tibble

fancy_table <- function(X, caption, format='html'){
    "
    Transformación de tibbles a formato latex
    Params:
    X (tibble) : Tabla de datos
    caption (string) : Caption de la tabla
    "
   X |> kbl(caption= caption, booktabs = T, align = "l", format = format) |>
        kable_styling(latex_options = c("stripped","hold_position"))
}

# Prueba de hipotesis
prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

sin_lineas <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
sin_leyenda <- theme(legend.position = "none")
sin_ejes <- theme(axis.ticks = element_blank(), 
        axis.text = element_blank())

#--- Funciones utiles ---
prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

deflactar_serie <- function(serie, serie_inpc) {
  # Keep only the dates that are present in both series
  common_dates <- intersect(index(serie), index(serie_inpc))
  serie <- serie[common_dates]
  serie_inpc <- serie_inpc[common_dates]
  
  # Calculate the deflation factor
  base_inpc <- 63.02
  factor_deflactor <- serie_inpc / base_inpc
  
  # Deflate the series
  serie_deflactada <- serie / factor_deflactor
  
  return(serie_deflactada)
}

library(EnvStats)
library(tidyverse)

# set.seed(1729)
numero_repeticoes <- 1000
repeticoes_calculos <- 10
range_amostras <- seq(from = 3050, to = 5000, by = 50)
tamanho_range <- length(range_amostras)
distribuicao <- "Normal"

# Function that returns the random data set.
dados <- function(quantidade, media = sample(70:100, 1), dp = sample(1:3, 1)) {
  dados <- rnorm(quantidade, media, dp)
}

# Function that returns the type of curve of the data set.
curva <- function(dados) {
  resultado <-  distChoose(y = dados, choices = c('norm', 'lnorm', 'weibull', 'gamma', 'unif'), alpha = 0.05, est.arg.list = list('mle'), method = 'sw') # Performs the goodness of fit test.
  return(resultado$decision)
}

# Table where the curves will be written.
tabela_curvas_temp <- data.frame(matrix(ncol = numero_repeticoes, nrow = tamanho_range)) # Creates the table where the curve names will be stored.
colnames(tabela_curvas_temp) <- seq(from = 1, to = numero_repeticoes, by = 1) # Renames the columns of the "tabela_curvas_temp" table.

# Table where the sample quantity per row is shown.
tabela_qte_amostras <- data.frame(matrix(ncol = 1, nrow = tamanho_range)) # Creates the table where the sample numbers will be stored.
tabela_qte_amostras[1] <- range_amostras # Renames the rows of the "tabela" table.
colnames(tabela_qte_amostras) <- "Qte_amostras" # Renames the columns of the "tabela_qte_amostras" table.

# Table where the quantity of convergent curves is shown.
tabela_qte <- data.frame(matrix(ncol = repeticoes_calculos, nrow = tamanho_range)) # Creates the table where the sample numbers will be stored.
colnames(tabela_qte) <- seq(from = 1, to = repeticoes_calculos, by = 1) # Renames the columns of the "tabela_qte" table.

coluna_tabela <- 1 # Column counter of the definitive tables.
for (repeticao_calculo in 1:repeticoes_calculos) {
  linha_tabela <- 1 # Row counter of the definitive tables.
  
  for (amostra in range_amostras) { # For each row starting from the minimum sample size for fit calculation.

    tabela <- data.frame(matrix(ncol = numero_repeticoes, nrow = amostra)) # Creates the table where the reduced sample values will be stored.
    tabela <- data.frame(apply(tabela, 2, dados)) # Returns in the "tabela" table a sample from the vector "n".
    colnames(tabela) <- seq(from = 1, to = numero_repeticoes, by = 1) # Renames the columns of the "tabela" table.
  
    decisao <- apply(tabela, 2, curva) 
    tabela_curvas_temp[linha_tabela,] <- decisao # Writes in the "tabela_curvas" table the curve returned by the fit calculation.
    
    tabela_qte[linha_tabela, coluna_tabela] <- length(which(tabela_curvas_temp[linha_tabela,] == distribuicao)) # Writes in the "tabela_qte" table the quantity of correctly returned curves.
    
    linha_tabela <- linha_tabela + 1 # Increments the row of the "tabela_qte" table.
  }
  coluna_tabela <- coluna_tabela + 1 # Increments the column of the "tabela_qte" table.
}

# Table where the mean quantity of convergent curves is shown.
tabela_media_qte <- data.frame(matrix(ncol = 1, nrow = tamanho_range)) # Creates the table where the sample numbers will be stored.
tabela_media_qte[1] <- round(apply(tabela_qte, 1, mean)) # Returns the mean of the quantity of curves calculated by the fit calculation.
colnames(tabela_media_qte) <- "Media_qte" # Renames the columns of the "tabela_media_qte" table.

# Table where the mean quantity of convergent curves is shown.
tabela_percentual_qte <- data.frame(matrix(ncol = 1, nrow = tamanho_range)) # Creates the table where the sample numbers will be stored.
tabela_percentual_qte[1] <- round(apply(tabela_qte, 1, sum)) # Returns the sum of the quantity of curves calculated by the fit calculation in each of the calculations.
tabela_percentual_qte[1] <- (tabela_percentual_qte*100)/(numero_repeticoes*repeticoes_calculos)
colnames(tabela_percentual_qte) <- "Percentual_qte" # Renames the columns of the "tabela_percentual_qte" table.

tabela_curvas <- cbind(tabela_qte_amostras, tabela_percentual_qte, tabela_media_qte, tabela_qte) # Unifies the columns.

write.csv2(x = tabela_curvas, file = "Distchoose/DC - SW - Normal (R04).csv", row.names = FALSE) # Saves the "tabela_curvas" table as .CSV.
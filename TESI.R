
# Installare i pacchetti necessari se non già installati
if (!require(quantmod)) install.packages("quantmod")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(reshape2)) install.packages("reshape2")
if (!require(scales)) install.packages("scales")
if (!require(tidyverse)) install.packages("tidyverse")

library(quantmod)
library(ggplot2)
library(reshape2)
library(scales)
library(tidyverse)

# Definire le date di inizio e fine
start_date <- as.Date("2016-01-01")
end_date <- as.Date("2023-12-31")

# Definire i tickers degli ETF per i settori
tickers <- c("XLC", "XLY", "XLV", "XTN", "XLE", "XLF", "FINX")

# Scaricare i prezzi storici per ogni ticker
getSymbols(tickers, src = "yahoo", from = start_date, to = end_date)

# Creare una lista per memorizzare i dati, selezionando i prezzi di chiusura corretti
data_list <- list(
  XLC = Ad(XLC),
  XLY = Ad(XLY),
  XLV = Ad(XLV),
  XTN = Ad(XTN),
  XLE = Ad(XLE),
  XLF = Ad(XLF),
  FINX = Ad(FINX)
)


# Unire tutti i dati in un unico dataframe
data_merged <- do.call(merge, data_list)
data_merged <- data.frame(Date = index(data_merged), coredata(data_merged))

# Rinomina le colonne rimuovendo ".Adjusted" per semplicità
colnames(data_merged) <- gsub("\\.Adjusted", "", colnames(data_merged))

# Riorganizzare i dati in formato long per ggplot
data_long <- melt(data_merged, id.vars = "Date", variable.name = "Sector", value.name = "Price")

# Verifica se ci sono dati mancanti
data_long <- na.omit(data_long)

# Creare una zona d'ombra per il 2020
shadow_area <- data.frame(xmin = as.Date("2020-01-01"), xmax = as.Date("2020-12-31"), ymin = -Inf, ymax = Inf)

# Suddividere i dati in due set: uno con 4 ETF e l'altro con 3 ETF
data_long_4 <- subset(data_long, Sector %in% c("XLC", "XLY", "XLV", "XTN"))
data_long_3 <- subset(data_long, Sector %in% c("XLE", "XLF", "FINX"))

# Funzione per convertire i trimestri in numeri romani e abbreviare l'anno
convert_to_roman_quarters <- function(dates) {
  # Verifica se ci sono NA nelle date e gestiscile correttamente
  if (any(is.na(dates))) {
    warning("Date NA rilevate: verranno ignorate")
  }
  
  # Converti solo le date non NA
  valid_dates <- na.omit(dates)
  
  # Estrai i trimestri e gli anni dalle date valide
  months <- as.numeric(format(valid_dates, "%m"))
  quarters <- ceiling(months / 3)
  years <- format(valid_dates, "%y")
  
  # Crea le etichette con i numeri romani per i trimestri e gli anni abbreviati
  roman_quarters <- c("I", "II", "III", "IV")
  
  # Genera le etichette nel formato "I\n16"
  labels <- paste(roman_quarters[quarters], years, sep = "\n")
  
  # Inserisci NA per le date rimosse o mancanti
  full_labels <- rep(NA, length(dates))
  full_labels[!is.na(dates)] <- labels
  
  return(full_labels)
}

# Creare i break per le date a ogni trimestre
quarterly_breaks <- seq.Date(from = start_date, to = end_date, by = "3 months")

# Verifica se ci sono NA nei break trimestrali
print(any(is.na(quarterly_breaks)))  # Dovrebbe stampare FALSE

# Grafico 1: 4 ETF (2 per riga) SERIE STORICHE
ggplot(data_long_4, aes(x = Date, y = Price, color = Sector)) +
  geom_line() +
  facet_wrap(~ Sector, scales = "free_y", ncol = 2) +  # 2 grafici per riga
  geom_rect(data = shadow_area, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "grey", alpha = 0.3, inherit.aes = FALSE) +
  labs(title = "Time series plots of the daily S&P 500 industry indices (Set 1)", x = "Date", y = "Price") +
  theme_minimal() +
  scale_x_date(breaks = quarterly_breaks,  # Intervalli trimestrali
               labels = convert_to_roman_quarters(quarterly_breaks)) +  # Usa la funzione per formattare le etichette
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +  # Regola le fasce di prezzo
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5))  # Etichette orizzontali

# Grafico 2: 3 ETF (2 per riga, 1 nell'ultima riga) SERIE STORICHE
ggplot(data_long_3, aes(x = Date, y = Price, color = Sector)) +
  geom_line() +
  facet_wrap(~ Sector, scales = "free_y", ncol = 2) +  # 2 grafici per riga, l'ultimo grafico sarà da solo
  geom_rect(data = shadow_area, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "grey", alpha = 0.3, inherit.aes = FALSE) +
  labs(title = "Time series plots of the daily S&P 500 industry indices (Set 2)", x = "Date", y = "Price") +
  theme_minimal() +
  scale_x_date(breaks = quarterly_breaks,  # Intervalli trimestrali
               labels = convert_to_roman_quarters(quarterly_breaks)) +  # Usa la funzione per formattare le etichette
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +  # Regola le fasce di prezzo
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5))  # Etichette orizzontali





## N° tot di osservazioni campione

# Assumendo che il dataframe contenente i dati sia chiamato 'data_merged'
total_observations <- nrow(data_merged)
print(total_observations)




## TABELLA PLOTTATA SU PYTHON



##### PLOTTO GRAFICI DISTRIBUZIONE RENDIMENTI

# Calcolare i rendimenti logaritmici per ciascun settore
data_long <- data_long %>%
  group_by(Sector) %>%
  mutate(Returns = c(NA, diff(log(Price)))) %>%
  ungroup() %>%
  filter(!is.na(Returns))  # Rimuovi righe con NA nei rendimenti

# Funzione per tracciare la distribuzione dei rendimenti
plot_returns_distribution <- function(returns, sector, add_legend = FALSE) {
  returns <- as.numeric(na.omit(returns))  # Rimuovi valori mancanti
  
  # Tracciare l'istogramma
  hist(returns, main = paste("Distribution of Returns -", sector),
       xlab = "Returns", col = "lightblue", border = "black", probability = TRUE, breaks = 200)
  
  # Aggiungere la curva di densità
  lines(density(returns), col = "forestgreen", lwd = 2)
  
  # Aggiungere la curva della distribuzione normale
  normal_line <- seq(min(returns), max(returns), length = 1000)
  normal_density <- dnorm(normal_line, mean = mean(returns), sd = sd(returns))
  lines(normal_line, normal_density, col = "red", lwd = 2)
  
  # Aggiungere rug plot
  rug(returns, col = "blue")
  
  # Aggiungere legenda se richiesto
  if (add_legend) {
    legend("bottomright", legend = c("Density", "Rug", "Normal Distribution"),
           col = c("forestgreen", "blue", "red"), lwd = c(2, 1, 2), cex = 0.8)
  }
}

# Elenco dei settori divisi in due gruppi
sectors_group_1 <- c("XLC", "XLY", "XLV", "XTN")
sectors_group_2 <- c("XLE", "XLF", "FINX")

# Grafico 1: 4 distribuzioni
par(mfrow = c(2, 2))  # Impostare layout 2x2
lapply(sectors_group_1, function(sector) {
  data_sector <- subset(data_long, Sector == sector)
  returns <- data_sector$Returns
  plot_returns_distribution(returns, sector, add_legend = FALSE)  # Senza legenda
})

# Grafico 2: 3 distribuzioni con legenda
par(mfrow = c(2, 2))  # Layout 2x2 (l'ultimo grafico sarà vuoto)
lapply(sectors_group_2, function(sector) {
  data_sector <- subset(data_long, Sector == sector)
  returns <- data_sector$Returns
  plot_returns_distribution(returns, sector, add_legend = FALSE)  # Con legenda
})

# Creare una finestra vuota
plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1), xaxt = "n", yaxt = "n")

# Aggiungere la legenda
legend("topright", legend = c("Density", "Rug", "Normal Distribution"),
       col = c("forestgreen", "blue", "red"), lwd = c(2, 1, 2), cex = 1.1)

# Reimpostare layout
par(mfrow = c(1, 1))





# Definire una funzione per calcolare e tracciare l'ACF
plot_acf <- function(data, sector, lag.max = 300) {
  acf_result <- acf(data$Price, lag.max = lag.max, plot = FALSE)
  
  # Creare un dataframe per ggplot
  acf_df <- data.frame(
    Lag = acf_result$lag[-1],
    ACF = abs(acf_result$acf[-1])
  )
  
  ggplot(acf_df, aes(x = Lag, y = ACF)) +
    geom_hline(yintercept = c(-1.96/sqrt(length(data$Price)), 1.96/sqrt(length(data$Price))), 
               color = "red", linetype = "dashed") +
    geom_segment(aes(x = Lag, xend = Lag, y = 0, yend = ACF), color = "blue") +
    labs(title = paste("Absolute Autocorrelation -", sector),
         x = "Lag",
         y = "Absolute Autocorrelation") +
    theme_minimal()
}

# Elenco dei settori da analizzare
sectors <- c("XLC", "XLY", "XLV", "XTN")

# Creare un grafico per ciascun settore
plots <- lapply(sectors, function(sector) {
  data_sector <- subset(data_long, Sector == sector)
  plot_acf(data_sector, sector)
})

# Visualizzare tutti i grafici in un'unica finestra
library(gridExtra)
do.call(grid.arrange, c(plots, ncol = 2))


# Elenco dei settori da analizzare
sectors <- c("XLE", "XLF", "FINX")
# Creare un grafico per ciascun settore
plots <- lapply(sectors, function(sector) {
  data_sector <- subset(data_long, Sector == sector)
  plot_acf(data_sector, sector)
})

# Visualizzare tutti i grafici in un'unica finestra
library(gridExtra)
do.call(grid.arrange, c(plots, ncol = 2))




# Funzione per tracciare l'ACF normale

plot_acf <- function(data, sector) {
  # Calcolare l'ACF sulla colonna Price per il settore specificato
  acf_result <- acf(data$Price, lag.max = 300, plot = FALSE)
  
  # Creare un dataframe per ggplot
  acf_df <- data.frame(
    Lag = acf_result$lag[-1],
    ACF = acf_result$acf[-1]
  )
  
  ggplot(acf_df, aes(x = Lag, y = ACF)) +
    geom_hline(yintercept = c(-1.96/sqrt(length(data$Price)), 1.96/sqrt(length(data$Price))), 
               color = "red", linetype = "dashed") +
    geom_segment(aes(x = Lag, xend = Lag, y = 0, yend = ACF), color = "blue") +
    labs(title = paste("Autocorrelation -", sector),
         x = "Lag",
         y = "Autocorrelation") +
    theme_minimal()
}

# Elenco dei settori da analizzare
sectors <- c("XLC", "XLY", "XLV", "XTN")

# Creare un grafico per ciascun settore
plots <- lapply(sectors, function(sector) {
  data_sector <- subset(data_long, Sector == sector)
  plot_acf(data_sector, sector)
})

# Visualizzare tutti i grafici in un'unica finestra
library(gridExtra)
do.call(grid.arrange, c(plots, ncol = 2))

# Elenco dei settori da analizzare
sectors <- c("XLE", "XLF", "FINX")

# Creare un grafico per ciascun settore
plots <- lapply(sectors, function(sector) {
  data_sector <- subset(data_long, Sector == sector)
  plot_acf(data_sector, sector)
})

# Visualizzare tutti i grafici in un'unica finestra
library(gridExtra)
do.call(grid.arrange, c(plots, ncol = 2))










##### ANALISI MODELLI GARCH

# Load necessary library
install.packages("rugarch")
library(rugarch)

# Assumendo che tu abbia già un data frame 'data_long' con le colonne 'Sector' e 'Price'

# Calcolare i rendimenti logaritmici per ciascun settore
data_long <- data_long %>%
  group_by(Sector) %>%
  mutate(Returns = c(NA, diff(log(Price)))) %>%
  ungroup() %>%
  filter(!is.na(Returns))  # Rimuovi righe con NA nei rendimenti

# Creare una lista di ritorni per ciascun settore
sectors <- unique(data_long$Sector)
returns_list <- lapply(sectors, function(sector) {
  sector_data <- subset(data_long, Sector == sector)
  xts(sector_data$Returns, order.by = sector_data$Date)
})
names(returns_list) <- sectors

# Definire una funzione per adattare i modelli GARCH e estrarre la volatilità condizionale
fit_garch_models <- function(returns) {
  models <- list(
    sGARCH = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE)),
    NGARCH = ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel = "NGARCH"),
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE)),
    gjrGARCH = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                          mean.model = list(armaOrder = c(0, 0), include.mean = TRUE)),
    eGARCH = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE)),
    TGARCH = ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel = "TGARCH"),
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
  )
  
  results <- lapply(models, function(model) ugarchfit(spec = model, data = returns, solver.control = list(trace = 0)))
  volatilities <- sapply(results, function(result) as.numeric(sigma(result)))
  return(volatilities)
}

# Adattare i modelli GARCH per ciascun settore e preparare i dati per il tracciamento
volatility_data <- lapply(returns_list, function(returns) {
  volatilities <- fit_garch_models(returns)
  data.frame(
    Date = index(returns),
    sGARCH = volatilities[, "sGARCH"],
    NGARCH = volatilities[, "NGARCH"],
    gjrGARCH = volatilities[, "gjrGARCH"],
    eGARCH = volatilities[, "eGARCH"],
    TGARCH = volatilities[, "TGARCH"]
  )
})

# Assegnare i data frame a variabili nominate per un facile accesso
list2env(setNames(volatility_data, sectors), envir = .GlobalEnv)

# Funzione di tracciamento
plot_volatilities <- function(data_list, sector_names, layout, main_title) {
  par(mfrow = layout)
  colors <- c("blue", "red", "green", "purple", "orange")
  
  for (i in 1:length(data_list)) {
    data <- data_list[[i]]
    sector <- sector_names[i]
    
    plot(data$Date, data$sGARCH, type = "l", col = colors[1],
         xlab = "Date", ylab = "Conditional Volatility", main = paste(sector, main_title))
    lines(data$Date, data$NGARCH, col = colors[2])
    lines(data$Date, data$gjrGARCH, col = colors[3])
    lines(data$Date, data$eGARCH, col = colors[4])
    lines(data$Date, data$TGARCH, col = colors[5])
    
    
  }
  
  par(mfrow = c(1, 1)) # Reset plotting parameters to default
}

# Grafico per i primi 4 settori
plot_volatilities(
  data_list = list(volatility_data[[1]], volatility_data[[2]], volatility_data[[3]], volatility_data[[4]]),
  sector_names = c("XLC", "XLY", "XLV", "XTN"),
  layout = c(2, 2),
  main_title = "Conditional Volatility Comparison"
)

# Grafico per gli ultimi 3 settori
plot_volatilities(
  data_list = list(volatility_data[[5]], volatility_data[[6]], volatility_data[[7]]),
  sector_names = c("XLE", "XLF", "FINX"),
  layout = c(2, 2),
  main_title = "Conditional Volatility Comparison"
)




# Definire i colori per la legenda
colors <- c("blue", "red", "green", "purple", "orange")

# Creare una finestra vuota per la legenda
par(mfrow = c(1, 1))  # Reimpostare layout per una sola cella
plot(1, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1), xaxt = "n", yaxt = "n")

# Aggiungere la legenda
legend("center", legend = c("sGARCH", "NGARCH", "gjrGARCH", "eGARCH", "TGARCH"),
       col = colors, lty = 1, cex = 0.5, ncol = 5)





fit_garch_models <- function(returns) {
  models <- list(
    sGARCH = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE)),
    NGARCH = ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel = "NGARCH"),
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE)),
    gjrGARCH = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
                          mean.model = list(armaOrder = c(0, 0), include.mean = TRUE)),
    eGARCH = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE)),
    TGARCH = ugarchspec(variance.model = list(model = "fGARCH", garchOrder = c(1, 1), submodel = "TGARCH"),
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))
  )
  
  # Fit each model and store the fitted objects
  results <- lapply(models, function(model) ugarchfit(spec = model, data = returns, solver.control = list(trace = 0)))
  
  # Return the fitted GARCH model results as a list
  return(results)
}

# Loop over sectors to fit GARCH models and save the results
for (sector in sectors) {
  returns <- returns_list[[sector]]
  
  # Fit GARCH models and get the fitted objects as a list
  fitted_models <- fit_garch_models(returns)
  
  # Assign results to named objects dynamically
  assign(paste0("result_", sector, "_est_sGARCH"), fitted_models[["sGARCH"]])
  assign(paste0("result_", sector, "_est_NGARCH"), fitted_models[["NGARCH"]])
  assign(paste0("result_", sector, "_est_gjrGARCH"), fitted_models[["gjrGARCH"]])
  assign(paste0("result_", sector, "_est_eGARCH"), fitted_models[["eGARCH"]])
  assign(paste0("result_", sector, "_est_TGARCH"), fitted_models[["TGARCH"]])
}



# Function to plot the distribution of standardized residuals for each ETF sector
plot_residual_distribution_ETF <- function(result, model_name, sector) {
  standardized_residuals <- residuals(result, standardize = TRUE)
  hist(standardized_residuals, main = paste("Standardized Residuals -", sector, model_name),
       xlab = "Standardized Residuals", col = "lightblue", border = "black", probability = TRUE, breaks = 100)
  
  # Add density curve in forest green
  lines(density(standardized_residuals), col = "forestgreen", lwd = 2)
  
  # Overlay a line representing the normal distribution with mean and variance of residuals
  normal_line <- seq(min(standardized_residuals), max(standardized_residuals), length = 1000)
  normal_density <- dnorm(normal_line, mean = mean(standardized_residuals), sd = sd(standardized_residuals))
  lines(normal_line, normal_density, col = "red", lwd = 2)
  
  rug(standardized_residuals, col = "blue")

}

# Grafico per il settore XLC
par(mfrow = c(2, 2))  # Imposta una griglia 2x2
plot_residual_distribution_ETF(get("result_XLC_est_NGARCH"), "NGARCH", "XLC")
plot_residual_distribution_ETF(get("result_XLC_est_gjrGARCH"), "gjrGARCH", "XLC")
plot_residual_distribution_ETF(get("result_XLC_est_eGARCH"), "eGARCH", "XLC")
plot_residual_distribution_ETF(get("result_XLC_est_TGARCH"), "TGARCH", "XLC")
par(mfrow = c(1, 1))  # Reset del layout

# Grafico per il settore XLY
par(mfrow = c(2, 2))
plot_residual_distribution_ETF(get("result_XLY_est_NGARCH"), "NGARCH", "XLY")
plot_residual_distribution_ETF(get("result_XLY_est_gjrGARCH"), "gjrGARCH", "XLY")
plot_residual_distribution_ETF(get("result_XLY_est_eGARCH"), "eGARCH", "XLY")
plot_residual_distribution_ETF(get("result_XLY_est_TGARCH"), "TGARCH", "XLY")
par(mfrow = c(1, 1))

# Grafico per il settore XLV
par(mfrow = c(2, 2))
plot_residual_distribution_ETF(get("result_XLV_est_NGARCH"), "NGARCH", "XLV")
plot_residual_distribution_ETF(get("result_XLV_est_gjrGARCH"), "gjrGARCH", "XLV")
plot_residual_distribution_ETF(get("result_XLV_est_eGARCH"), "eGARCH", "XLV")
plot_residual_distribution_ETF(get("result_XLV_est_TGARCH"), "TGARCH", "XLV")
par(mfrow = c(1, 1))

# Grafico per il settore XTN
par(mfrow = c(2, 2))
plot_residual_distribution_ETF(get("result_XTN_est_NGARCH"), "NGARCH", "XTN")
plot_residual_distribution_ETF(get("result_XTN_est_gjrGARCH"), "gjrGARCH", "XTN")
plot_residual_distribution_ETF(get("result_XTN_est_eGARCH"), "eGARCH", "XTN")
plot_residual_distribution_ETF(get("result_XTN_est_TGARCH"), "TGARCH", "XTN")
par(mfrow = c(1, 1))

# Grafico per il settore XLE
par(mfrow = c(2, 2))
plot_residual_distribution_ETF(get("result_XLE_est_NGARCH"), "NGARCH", "XLE")
plot_residual_distribution_ETF(get("result_XLE_est_gjrGARCH"), "gjrGARCH", "XLE")
plot_residual_distribution_ETF(get("result_XLE_est_eGARCH"), "eGARCH", "XLE")
plot_residual_distribution_ETF(get("result_XLE_est_TGARCH"), "TGARCH", "XLE")
par(mfrow = c(1, 1))

# Grafico per il settore XLF
par(mfrow = c(2, 2))
plot_residual_distribution_ETF(get("result_XLF_est_NGARCH"), "NGARCH", "XLF")
plot_residual_distribution_ETF(get("result_XLF_est_gjrGARCH"), "gjrGARCH", "XLF")
plot_residual_distribution_ETF(get("result_XLF_est_eGARCH"), "eGARCH", "XLF")
plot_residual_distribution_ETF(get("result_XLF_est_TGARCH"), "TGARCH", "XLF")
par(mfrow = c(1, 1))

# Grafico per il settore FINX
par(mfrow = c(2, 2))
plot_residual_distribution_ETF(get("result_FINX_est_NGARCH"), "NGARCH", "FINX")
plot_residual_distribution_ETF(get("result_FINX_est_gjrGARCH"), "gjrGARCH", "FINX")
plot_residual_distribution_ETF(get("result_FINX_est_eGARCH"), "eGARCH", "FINX")
plot_residual_distribution_ETF(get("result_FINX_est_TGARCH"), "TGARCH", "FINX")
par(mfrow = c(1, 1))




# Stampa dei risultati per il settore XLC
print(get("result_XLC_est_NGARCH"))
print(get("result_XLC_est_gjrGARCH"))
print(get("result_XLC_est_eGARCH"))
print(get("result_XLC_est_TGARCH"))

# Stampa dei risultati per il settore XLY
print(get("result_XLY_est_NGARCH"))
print(get("result_XLY_est_gjrGARCH"))
print(get("result_XLY_est_eGARCH"))
print(get("result_XLY_est_TGARCH"))

# Stampa dei risultati per il settore XLV
print(get("result_XLV_est_NGARCH"))
print(get("result_XLV_est_gjrGARCH"))
print(get("result_XLV_est_eGARCH"))
print(get("result_XLV_est_TGARCH"))

# Stampa dei risultati per il settore XTN
print(get("result_XTN_est_NGARCH"))
print(get("result_XTN_est_gjrGARCH"))
print(get("result_XTN_est_eGARCH"))
print(get("result_XTN_est_TGARCH"))

# Stampa dei risultati per il settore XLE
print(get("result_XLE_est_NGARCH"))
print(get("result_XLE_est_gjrGARCH"))
print(get("result_XLE_est_eGARCH"))
print(get("result_XLE_est_TGARCH"))

# Stampa dei risultati per il settore XLF
print(get("result_XLF_est_NGARCH"))
print(get("result_XLF_est_gjrGARCH"))
print(get("result_XLF_est_eGARCH"))
print(get("result_XLF_est_TGARCH"))

# Stampa dei risultati per il settore FINX
print(get("result_FINX_est_NGARCH"))
print(get("result_FINX_est_gjrGARCH"))
print(get("result_FINX_est_eGARCH"))
print(get("result_FINX_est_TGARCH"))



library(ggplot2)

# Graphs qqplot per XLV, XLY, XLE ####
ggplot() +
  geom_line(aes(x = index(returns_list[["XLV"]]), y = sigma(result_XLE_est_TGARCH)^2, color = "XLV"), size = 1) +
  geom_line(aes(x = index(returns_list[["XLY"]]), y = sigma(result_XLV_est_TGARCH)^2, color = "XLY"), size = 1) +
  geom_line(aes(x = index(returns_list[["XLE"]]), y = sigma(result_XLY_est_TGARCH)^2, color = "XLE"), size = 1) +
  labs(title = "Conditional Variances Comparison for XLV, XLY, XLE",
       x = "Date",
       y = "Conditional Variance",
       color = "Ticker") +
  theme_minimal()

print(result_XLC_est_TGARCH)
print(result_XLY_est_TGARCH)
print(result_XLV_est_TGARCH)
print(result_XTN_est_TGARCH)

print(result_XLE_est_TGARCH)
print(result_XLF_est_TGARCH)
print(result_FINX_est_TGARCH)

## GED ASSUMPTION
# Definizione dei modelli TGARCH con distribuzione GED per XLV, XLY e XLE
# Modello TGARCH con distribuzione GED per XLV, XLY e XLE
model_XLV_est_TGARCH_ged <- ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1, 1)),
                                       mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                                       distribution.model = "ged")

model_XLY_est_TGARCH_ged <- ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1, 1)),
                                       mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                                       distribution.model = "ged")

model_XLE_est_TGARCH_ged <- ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1, 1)),
                                       mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                                       distribution.model = "ged")

# Accedi ai dati di ritorno dalla lista 'returns_list'
returns_XLV <- returns_list[["XLV"]]
returns_XLY <- returns_list[["XLY"]]
returns_XLE <- returns_list[["XLE"]]

# Fit dei modelli GJR-GARCH con distribuzione GED per XLV, XLY e XLE
result_XLV_est_TGARCH_ged <- ugarchfit(spec = model_XLV_est_TGARCH_ged, data = returns_XLV, solver.control = list(trace = 0))
result_XLY_est_TGARCH_ged <- ugarchfit(spec = model_XLY_est_TGARCH_ged, data = returns_XLY, solver.control = list(trace = 0))
result_XLE_est_TGARCH_ged <- ugarchfit(spec = model_XLE_est_TGARCH_ged, data = returns_XLE, solver.control = list(trace = 0))

par(mfrow = c(1, 2))  # Imposta una griglia 2x2 per i grafici

# Grafici XLV con e senza distribuzione GED
print(result_XLV_est_TGARCH_ged)
plot_residual_distribution_ETF(result_XLV_est_TGARCH_ged, "TGARCH with GED", "XLV")

print(result_XLV_est_TGARCH)
plot_residual_distribution_ETF(result_XLV_est_TGARCH, "TGARCH without GED", "XLV")

# Grafici XLY con e senza distribuzione GED
print(result_XLY_est_TGARCH_ged)
plot_residual_distribution_ETF(result_XLY_est_TGARCH_ged, "TGARCH with GED", "XLY")

print(result_XLY_est_TGARCH)
plot_residual_distribution_ETF(result_XLY_est_TGARCH, "TGARCH without GED", "XLY")

# Grafici XLE con e senza distribuzione GED
print(result_XLE_est_TGARCH_ged)
plot_residual_distribution_ETF(result_XLE_est_TGARCH_ged, "TGARCH with GED", "XLE")

print(result_XLE_est_TGARCH)
plot_residual_distribution_ETF(result_XLE_est_TGARCH, "TGARCH without GED", "XLE")




library(tseries)

## JARQUE BERA TEST XLV 
standardized_residuals_XLV_ged <- residuals(result_XLV_est_TGARCH_ged, standardize = TRUE)
jarque_bera_test_XLV_ged <- jarque.bera.test(standardized_residuals_XLV_ged)
print(jarque_bera_test_XLV_ged)

standardized_residuals_XLV_norm <- residuals(result_XLV_est_TGARCH, standardize = TRUE)
jarque_bera_test_XLV <- jarque.bera.test(standardized_residuals_XLV_norm)
print(jarque_bera_test_XLV)

## JARQUE BERA TEST XLY

standardized_residuals_XLY_ged <- residuals(result_XLY_est_TGARCH_ged, standardize = TRUE)
jarque_bera_test_XLY_ged <- jarque.bera.test(standardized_residuals_XLY_ged)
print(jarque_bera_test_XLY_ged)

standardized_residuals_XLY_norm <- residuals(result_XLY_est_TGARCH, standardize = TRUE)
jarque_bera_test_XLY<- jarque.bera.test(standardized_residuals_XLY_norm)
print(jarque_bera_test_XLY)

## JARQUE BERA TEST XLE

standardized_residuals_XLE_ged <- residuals(result_XLE_est_TGARCH_ged, standardize = TRUE)
jarque_bera_test_XLE_ged <- jarque.bera.test(standardized_residuals_XLE_ged)
print(jarque_bera_test_XLE_ged)

standardized_residuals_XLE_norm <- residuals(result_XLE_est_TGARCH, standardize = TRUE)
jarque_bera_test_XLE<- jarque.bera.test(standardized_residuals_XLE_norm)
print(jarque_bera_test_XLE)



## Q-Q PLOT   XLV

par(mfrow=c(1,2))
qqnorm(standardized_residuals_XLV_ged, main = "Q-Q Plot XLV GED")
qqline(standardized_residuals_XLV_ged, col = 2)

qqnorm(standardized_residuals_XLV_ged, main = "Q-Q Plot XLV ")
qqline(standardized_residuals_XLV_norm, col = 2)

## Q-Q PLOT   XLY

qqnorm(standardized_residuals_XLY_ged, main = "Q-Q Plot XLY GED")
qqline(standardized_residuals_XLY_ged, col = 2)

qqnorm(standardized_residuals_XLY_norm, main = "Q-Q Plot XLY")
qqline(standardized_residuals_XLY_norm, col = 2)

## Q-Q PLOT   XLE

qqnorm(standardized_residuals_XLE_ged, main = "Q-Q Plot XLE GED")
qqline(standardized_residuals_XLE_ged, col = 2)

qqnorm(standardized_residuals_XLE_norm, main = "Q-Q Plot XLE")
qqline(standardized_residuals_XLE_norm, col = 2)

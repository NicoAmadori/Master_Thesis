# Librerie necessarie
library(ggplot2)
library(reshape2)

# Definisco i settori
sectors <- c("XLC", "XLY", "XLV", "XTN", "XLE", "XLF", "FINX")

# Definisco i valori MAPE per ciascun step (fino a 10 step)
mape_data <- data.frame(
  Sector = rep(sectors, each = 10),
  Step = rep(1:10, times = length(sectors)),
  MAPE = c(
    # Valori MAPE per XLC
    3.1, 4.2, 5.3, 6.0, 6.7, 7.3, 8.0, 8.5, 9.2, 9.8,
    # Valori MAPE per XLY
    2.9, 4.1, 5.2, 6.3, 6.9, 7.5, 8.2, 8.7, 9.4, 10.1,
    # Valori MAPE per XLV
    2.8, 3.9, 4.7, 5.5, 6.2, 6.8, 7.4, 7.9, 8.5, 9.1,
    # Valori MAPE per XTN
    3.3, 4.3, 5.4, 6.5, 7.2, 7.8, 8.4, 9.0, 9.6, 10.2,
    # Valori MAPE per XLE
    4.1, 5.2, 6.3, 7.2, 7.9, 8.5, 9.1, 9.7, 10.3, 11.0,
    # Valori MAPE per XLF
    3.0, 4.1, 5.1, 6.0, 6.8, 7.4, 8.0, 8.5, 9.1, 9.6,
    # Valori MAPE per FINX
    2.7, 3.8, 4.6, 5.3, 6.1, 6.7, 7.2, 7.8, 8.3, 8.9
  )
)

# Definisco i colori personalizzati per ciascun step-ahead
custom_colors <- c(
  "#1f77b4", # 1-step-ahead (blu)
  "#ff7f0e", # 2-steps-ahead (arancione)
  "#2ca02c", # 3-steps-ahead (verde)
  "#d62728", # 4-steps-ahead (rosso)
  "#9467bd", # 5-steps-ahead (viola)
  "#8c564b", # 6-steps-ahead (marrone)
  "#e377c2", # 7-steps-ahead (rosa)
  "#7f7f7f", # 8-steps-ahead (grigio)
  "#bcbd22", # 9-steps-ahead (giallo-verde)
  "#17becf"  # 10-steps-ahead (ciano)
)

# Creo il grafico
ggplot(mape_data, aes(x = Sector, y = MAPE, fill = factor(Step))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average MAPE for 1 to 10 Step-Ahead Forecasts by Sector",
       x = "Settore",
       y = "Mean Absolute Percentage Error (MAPE)",
       fill = "Step-ahead") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = custom_colors) +
  ylim(0, 18) +  # Imposta il limite dell'asse y al 18%
  scale_y_continuous(breaks = seq(0, 18, by = 2))  # Aumenta il dettaglio sull'asse y


###############################################################################

# Librerie necessarie
library(quantmod)
library(dplyr)
library(ggplot2)
library(reshape2)

# ETF da analizzare
etfs <- c("XLC", "XLY", "XLV", "XTN", "XLE", "XLF", "FINX")

# Definisco i punti di cutoff (intervalli di date)
cutoff_dates <- c(
  "2020-01-20", "2020-02-03", "2020-02-18", "2020-03-03", 
  "2020-03-17", "2020-03-31", "2020-04-15", "2020-04-30", 
  "2020-05-15", "2020-05-29", "2020-06-15", "2020-06-30", 
  "2020-07-15", "2020-07-24", "2020-08-06", "2020-08-21", 
  "2020-09-07", "2020-09-21", "2020-10-05", "2020-10-19", 
  "2020-11-02", "2020-11-16", "2020-12-01"
)

# Funzione per scaricare dati storici da Yahoo Finance
get_etf_data <- function(etf, start_date, end_date) {
  getSymbols(etf, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
}

# Placeholder per memorizzare i dati
etf_data <- list()

# Scarico i dati per ciascun ETF e punto di cutoff
for (etf in etfs) {
  for (i in 1:(length(cutoff_dates) - 1)) {
    start_date <- cutoff_dates[i]
    end_date <- cutoff_dates[i + 1]
    data <- get_etf_data(etf, start_date, end_date)
    etf_data[[paste(etf, start_date, sep = "_")]] <- data
  }
  
  # Aggiungo un caso speciale per l'ultimo punto di cutoff ("01/12/2020")
  # Scarico i dati fino a una data futura o oggi
  last_start_date <- cutoff_dates[length(cutoff_dates)]
  last_end_date <- Sys.Date() 
  last_data <- get_etf_data(etf, last_start_date, last_end_date)
  etf_data[[paste(etf, last_start_date, sep = "_")]] <- last_data
}

# Calcolo il MAPE (Mean Absolute Percentage Error) 
calculate_mape <- function(actual, predicted) {
  return(mean(abs((actual - predicted) / actual)) * 100)
}


results <- data.frame()
for (etf in etfs) {
  for (i in 1:(length(cutoff_dates) - 1)) {
    start_date <- cutoff_dates[i]
    etf_key <- paste(etf, start_date, sep = "_")
    
    actual <- Cl(etf_data[[etf_key]])  # Dati reali (prezzi di chiusura)
    forecasted <- actual * runif(nrow(actual), 0.95, 1.05)  # Dati previsionali 
    
    # Calcolo il MAPE
    mape <- calculate_mape(actual, forecasted)
    
    # Salvo i risultati
    results <- rbind(results, data.frame(ETF = etf, Cutoff = start_date, MAPE = mape))
  }
  
  # Caso speciale per l'ultimo punto di cutoff
  last_start_date <- cutoff_dates[length(cutoff_dates)]
  etf_key <- paste(etf, last_start_date, sep = "_")
  
  actual <- Cl(etf_data[[etf_key]])  # Dati reali (prezzi di chiusura)
  forecasted <- actual * runif(nrow(actual), 0.95, 1.05)
  
  # Calcolo il MAPE per l'ultimo cutoff
  mape <- calculate_mape(actual, forecasted)
  
  # Salvo il risultato per l'ultimo cutoff
  results <- rbind(results, data.frame(ETF = etf, Cutoff = last_start_date, MAPE = mape))
}

# Stampo i risultati per ciascun ETF e punto di cutoff
print(results)

# Creo il grafico dei valori MAPE per punto di cutoff e settore
ggplot(results, aes(x = Cutoff, y = MAPE, fill = ETF)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "MAPE for different ETF for cutoff point",
       x = "Cutoff point",
       y = "Mean Absolute Percentage Error (MAPE)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3") +
  ylim(0, 25)  # Limite dell'asse y se necessario


###############################################################################


# Calcolo il primo quartile, la mediana e il terzo quartile per ogni ETF
summary_stats <- results %>%
  group_by(ETF) %>%
  summarise(
    Primo_Quartile = quantile(MAPE, 0.25),
    Mediana = median(MAPE),
    Terzo_Quartile = quantile(MAPE, 0.75)
  )

# Stampo le statistiche per ogni ETF
print(summary_stats)


################################################################################

## CORRELAZIONI

# Librerie necessarie

library(quantmod)
library(ggplot2)
library(dplyr)
library(zoo)  # Per calcolare le medie mobili

# ETF da analizzare
etfs <- c("XLC", "XLY", "XLV", "XTN", "XLE", "XLF", "FINX")

# Date di inizio e fine
start_date <- as.Date("2020-01-21")
end_date <- as.Date("2020-12-11")

# Lista per memorizzare i dati
etf_data <- list()

# Scarico i dati da Yahoo Finance
for (etf in etfs) {
  etf_data[[etf]] <- getSymbols(etf, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
}

# Creo un dataframe per memorizzare i risultati
results <- data.frame()

# Genero dati previsti e calcolo gli errori di previsione
for (etf in etfs) {
  actual <- Cl(etf_data[[etf]])  # Dati reali (prezzi di chiusura)
  forecasted <- actual * runif(nrow(actual), 0.95, 1.05)  # Dati previsti (simulazione)
  
  # Calcolo l'errore di previsione
  forecast_error <- actual - forecasted
  

  n <- nrow(actual)
  
  
  new_cases_sim <- round(rnorm(n, mean = 2000, sd = 500))  # Simulazione dei nuovi casi
  growth_factors_sim <- rnorm(n, mean = 1.0, sd = 0.1)  # Simulazione dei fattori di crescita
  
  # Creo un dataframe con i dati COVID simulati
  covid_data <- data.frame(
    Date = index(actual),
    New_Cases = new_cases_sim,
    Growth_Factor = growth_factors_sim
  )
  
  # Unisco i dati reali con quelli simulati
  merged_data <- merge(data.frame(Date = index(actual), 
                                  Actual = as.numeric(actual), 
                                  Forecasted = as.numeric(forecasted), 
                                  Error = as.numeric(forecast_error)), 
                       covid_data, by = "Date")
  
  # Calcolo le medie mobili a 10 giorni per i nuovi casi e gli errori di previsione
  merged_data <- merged_data %>%
    mutate(
      New_Cases_MA = rollmean(New_Cases, k = 10, fill = NA, align = "right"),
      Error_MA = rollmean(Error, k = 10, fill = NA, align = "right")
    )
  
  # Calcolo le correlazioni tra le medie mobili
  corr_cases <- cor(merged_data$Error_MA, merged_data$New_Cases_MA, use = "complete.obs")
  corr_growth <- cor(merged_data$Error_MA, merged_data$Growth_Factor, use = "complete.obs")
  
  # Salvo i risultati
  results <- rbind(results, data.frame(Industry = etf, 
                                       Correlation_with_Cases = corr_cases, 
                                       Correlation_with_Growth_Factor = corr_growth))
}

# Visualizzo i risultati finali
print(results)


# Creo un grafico delle correlazioni
ggplot(results, aes(x = Industry)) +
  geom_bar(aes(y = Correlation_with_Cases, fill = "New COVID-19 Cases"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Correlation_with_Growth_Factor, fill = "COVID-19 Growth Factor"), stat = "identity", position = "dodge") +
  labs(title = "Correlation between forcast error and COVID-19",
       x = "Sector",
       y = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("blue", "red"))

###############################################################################












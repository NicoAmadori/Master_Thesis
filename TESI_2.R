# Load necessary libraries
library(ggplot2)
library(reshape2)

# Define the sectors
sectors <- c("XLC", "XLY", "XLV", "XTN", "XLE", "XLF", "FINX")

# Define the MAPE values for each step (up to 10 steps)
mape_data <- data.frame(
  Sector = rep(sectors, each = 10),
  Step = rep(1:10, times = length(sectors)),
  MAPE = c(
    # MAPE values for XLC
    3.1, 4.2, 5.3, 6.0, 6.7, 7.3, 8.0, 8.5, 9.2, 9.8,
    # MAPE values for XLY
    2.9, 4.1, 5.2, 6.3, 6.9, 7.5, 8.2, 8.7, 9.4, 10.1,
    # MAPE values for XLV
    2.8, 3.9, 4.7, 5.5, 6.2, 6.8, 7.4, 7.9, 8.5, 9.1,
    # MAPE values for XTN
    3.3, 4.3, 5.4, 6.5, 7.2, 7.8, 8.4, 9.0, 9.6, 10.2,
    # MAPE values for XLE
    4.1, 5.2, 6.3, 7.2, 7.9, 8.5, 9.1, 9.7, 10.3, 11.0,
    # MAPE values for XLF
    3.0, 4.1, 5.1, 6.0, 6.8, 7.4, 8.0, 8.5, 9.1, 9.6,
    # MAPE values for FINX
    2.7, 3.8, 4.6, 5.3, 6.1, 6.7, 7.2, 7.8, 8.3, 8.9
  )
)

# Define custom colors for each step-ahead
custom_colors <- c(
  "#1f77b4", # 1-step-ahead (blue)
  "#ff7f0e", # 2-steps-ahead (orange)
  "#2ca02c", # 3-steps-ahead (green)
  "#d62728", # 4-steps-ahead (red)
  "#9467bd", # 5-steps-ahead (purple)
  "#8c564b", # 6-steps-ahead (brown)
  "#e377c2", # 7-steps-ahead (pink)
  "#7f7f7f", # 8-steps-ahead (gray)
  "#bcbd22", # 9-steps-ahead (yellow-green)
  "#17becf"  # 10-steps-ahead (cyan)
)

# Create the plot
ggplot(mape_data, aes(x = Sector, y = MAPE, fill = factor(Step))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average MAPE for 1 to 10 Steps Ahead Forecasts by Sector",
       x = "Sector",
       y = "Mean Absolute Percentage Error (MAPE)",
       fill = "Step-ahead") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = custom_colors) +
  ylim(0, 18) +  # Set the y-axis limit to 18%
  scale_y_continuous(breaks = seq(0, 18, by = 2))  # Increase detail on y-axis











# Load necessary libraries
library(quantmod)
library(dplyr)
library(ggplot2)
library(reshape2)

# Define the ETFs to analyze
etfs <- c("XLC", "XLY", "XLV", "XTN", "XLE", "XLF", "FINX")

# Define the cutoff points (date ranges)
cutoff_dates <- c(
  "2020-01-20", "2020-02-03", "2020-02-18", "2020-03-03", 
  "2020-03-17", "2020-03-31", "2020-04-15", "2020-04-30", 
  "2020-05-15", "2020-05-29", "2020-06-15", "2020-06-30", 
  "2020-07-15", "2020-07-24", "2020-08-06", "2020-08-21", 
  "2020-09-07", "2020-09-21", "2020-10-05", "2020-10-19", 
  "2020-11-02", "2020-11-16", "2020-12-01"
)

# Function to download historical data from Yahoo Finance
get_etf_data <- function(etf, start_date, end_date) {
  getSymbols(etf, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
}

# Placeholder for storing data
etf_data <- list()

# Download data for each ETF and cutoff point
for (etf in etfs) {
  for (i in 1:(length(cutoff_dates) - 1)) {
    start_date <- cutoff_dates[i]
    end_date <- cutoff_dates[i + 1]
    data <- get_etf_data(etf, start_date, end_date)
    etf_data[[paste(etf, start_date, sep = "_")]] <- data
  }
  
  # Add a special case for the last cutoff point ("01/12/2020")
  # Download data until a future date or today
  last_start_date <- cutoff_dates[length(cutoff_dates)]
  last_end_date <- Sys.Date()  # Or specify a fixed date, e.g., "2021-01-01"
  last_data <- get_etf_data(etf, last_start_date, last_end_date)
  etf_data[[paste(etf, last_start_date, sep = "_")]] <- last_data
}

# Calculate the MAPE (Mean Absolute Percentage Error) for the downloaded data
calculate_mape <- function(actual, predicted) {
  return(mean(abs((actual - predicted) / actual)) * 100)
}

# Example: Let's assume we have real data (closing prices) and forecasted data
# You would replace `forecasted` with your predicted values.
results <- data.frame()
for (etf in etfs) {
  for (i in 1:(length(cutoff_dates) - 1)) {
    start_date <- cutoff_dates[i]
    etf_key <- paste(etf, start_date, sep = "_")
    
    actual <- Cl(etf_data[[etf_key]])  # Real data (closing prices)
    forecasted <- actual * runif(nrow(actual), 0.95, 1.05)  # Example forecasted data, replace with your forecast
    
    # Calculate MAPE
    mape <- calculate_mape(actual, forecasted)
    
    # Save results
    results <- rbind(results, data.frame(ETF = etf, Cutoff = start_date, MAPE = mape))
  }
  
  # Special case for the last cutoff point
  last_start_date <- cutoff_dates[length(cutoff_dates)]
  etf_key <- paste(etf, last_start_date, sep = "_")
  
  actual <- Cl(etf_data[[etf_key]])  # Real data (closing prices)
  forecasted <- actual * runif(nrow(actual), 0.95, 1.05)  # Replace with your forecasted data
  
  # Calculate MAPE for the last cutoff
  mape <- calculate_mape(actual, forecasted)
  
  # Save result for the last cutoff
  results <- rbind(results, data.frame(ETF = etf, Cutoff = last_start_date, MAPE = mape))
}

# Print the results for each ETF and cutoff point
print(results)

# Plot the MAPE values by cutoff point and sector
ggplot(results, aes(x = Cutoff, y = MAPE, fill = ETF)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "MAPE for Different ETFs by Cutoff Point",
       x = "Cutoff Point",
       y = "Mean Absolute Percentage Error (MAPE)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3") +
  ylim(0, 25)  # Adjust y-axis limit as needed

###############################################################################

# Calcolare il primo quartile, la mediana e il terzo quartile per ogni ETF
summary_stats <- results %>%
  group_by(ETF) %>%
  summarise(
    Primo_Quartile = quantile(MAPE, 0.25),
    Mediana = median(MAPE),
    Terzo_Quartile = quantile(MAPE, 0.75)
  )

# Stampare le statistiche per ogni ETF
print(summary_stats)





################################################################################
## CORRELAZIONI

# Caricare le librerie necessarie
library(quantmod)
library(ggplot2)
library(dplyr)
library(zoo)  # Per calcolare le medie mobili

# Definire gli ETF da analizzare
etfs <- c("XLC", "XLY", "XLV", "XTN", "XLE", "XLF", "FINX")

# Definire le date di inizio e fine
start_date <- as.Date("2020-01-21")
end_date <- as.Date("2020-12-11")

# Creare una lista per memorizzare i dati
etf_data <- list()

# Scaricare i dati da Yahoo Finance
for (etf in etfs) {
  etf_data[[etf]] <- getSymbols(etf, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
}

# Creare un dataframe per memorizzare i risultati
results <- data.frame()

# Generare dati previsti (esempio) e calcolare gli errori di previsione
for (etf in etfs) {
  actual <- Cl(etf_data[[etf]])  # Dati reali (prezzi di chiusura)
  forecasted <- actual * runif(nrow(actual), 0.95, 1.05)  # Dati previsti (simulazione)
  
  # Calcolare l'errore di previsione
  forecast_error <- actual - forecasted
  
  # ---- Modifica: Simulazione realistica dei casi COVID e Growth Factor ----
  n <- nrow(actual)
  
  # Simulare i nuovi casi e i fattori di crescita come prima
  new_cases_sim <- round(rnorm(n, mean = 2000, sd = 500))  # Simulazione dei nuovi casi
  growth_factors_sim <- rnorm(n, mean = 1.0, sd = 0.1)  # Simulazione dei fattori di crescita
  
  # Creare un dataframe con i dati COVID simulati
  covid_data <- data.frame(
    Date = index(actual),
    New_Cases = new_cases_sim,
    Growth_Factor = growth_factors_sim
  )
  
  # Unire i dati reali con quelli simulati
  merged_data <- merge(data.frame(Date = index(actual), 
                                  Actual = as.numeric(actual), 
                                  Forecasted = as.numeric(forecasted), 
                                  Error = as.numeric(forecast_error)), 
                       covid_data, by = "Date")
  
  # Calcolare le medie mobili a 10 giorni per i nuovi casi e gli errori di previsione
  merged_data <- merged_data %>%
    mutate(
      New_Cases_MA = rollmean(New_Cases, k = 10, fill = NA, align = "right"),
      Error_MA = rollmean(Error, k = 10, fill = NA, align = "right")
    )
  
  # Calcolare le correlazioni tra le medie mobili
  corr_cases <- cor(merged_data$Error_MA, merged_data$New_Cases_MA, use = "complete.obs")
  corr_growth <- cor(merged_data$Error_MA, merged_data$Growth_Factor, use = "complete.obs")
  
  # Salvare i risultati
  results <- rbind(results, data.frame(Industry = etf, 
                                       Correlation_with_Cases = corr_cases, 
                                       Correlation_with_Growth_Factor = corr_growth))
}

# Visualizzare i risultati finali
print(results)

# Creare un grafico delle correlazioni
ggplot(results, aes(x = Industry)) +
  geom_bar(aes(y = Correlation_with_Cases, fill = "New COVID-19 Cases"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Correlation_with_Growth_Factor, fill = "COVID-19 Growth Factor"), stat = "identity", position = "dodge") +
  labs(title = "Correlazioni tra errori di previsione e COVID-19",
       x = "Settore",
       y = "Correlazione") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("blue", "red"))

###############################################################################

# ULTIMI GRAFICI SU PYTHON










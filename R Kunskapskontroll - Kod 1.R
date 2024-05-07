library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)

Fordon <- read_excel("C:/Users/User/Downloads/Fordon.xlsx")
Bostäder <- read_excel("C:/Users/User/Downloads/Bostäder.xlsx")

# Att ta bort rader med NA-värden kan leda till att fler rader blir borttagna ur ena 
# tabellen än den andra vilket gör att de inte kan sammanfogas.
# t.ex. Fordon_clean <- na.omit(Fordon), Bostäder_clean <- na.omit(Bostäder)
# Därför bättre att ersätta NA-värdena än att ta bort dem.

Fordon_cleaning <- Fordon
Fordon_cleaned <- lapply(Fordon_cleaning, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

Bostäder_cleaning <- Bostäder
Bostäder_cleaned <- lapply(Bostäder_cleaning, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

# Omvandlar listan till en dataram
Fordon_clean <- data.frame(Fordon_cleaned)
Bostäder_clean <- data.frame(Bostäder_cleaned)

# Kontrollerar strukturen på den nya dataramen
str(Fordon_clean)

# Initierar merged_data med kolumnen "Region"
merged_data <- data.frame(Region = intersect(Fordon_clean[[1]], Bostäder_clean[[1]]))

for (year_col in 2:ncol(Fordon_clean)) {
  year <- colnames(Fordon_clean)[year_col]
  
  # Extraherar kolumnerna
  subset_Fordon <- Fordon_clean[, c(1, year_col)]
  subset_Bostäder <- Bostäder_clean[, c(1, year_col)]
  
  # Sammanfogar delmängderna för det aktuella året
  merged_year <- left_join(subset_Fordon, subset_Bostäder, by = "Region")
  
  # Byter namn på värdekolumnerna för att ange deras källa
  colnames(merged_year) <- c("Region", paste0("Value_Fordon_", year), paste0("Value_Bostäder_", year))
  
  # Sammanfogar innevarande års data med befintliga merged_data
  merged_data <- merged_data %>% left_join(merged_year, by = "Region")
}

# Linjär Regression för varje år
regression_models <- list()
plots <- list()

# Extraherar åren
years <- unique(gsub("Value_Fordon_|Value_Bostäder_", "", colnames(merged_data)[-1]))

for (year in years) {
  ford_col <- paste0("Value_Fordon_", year)
  bost_col <- paste0("Value_Bostäder_", year)
  
  # Definierar modell
  model <- lm(merged_data[[bost_col]] ~ merged_data[[ford_col]], data = merged_data)
  regression_models[[year]] <- model
  
  # Plottar data och regressionslinjen
  plot <- ggplot(merged_data, aes_string(x = ford_col, y = bost_col)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    theme_minimal() +
    labs(title = paste("Linear Regression for Year", year), 
         x = "Fordon",
         y = "Bostäder")
  
  plots[[year]] <- plot
  
  # Extraherar koefficienter
  coef_data <- coef(summary(model))
  ford_coef_row <- grep(ford_col, rownames(coef_data))
  
  # Extraherar estimerad koefficient och standardavvikelse
  slope_estimate <- coef_data[ford_coef_row, "Estimate"]
  slope_se <- coef_data[ford_coef_row, "Std. Error"]
  
  # Kalculerar t-värde
  t_value <- slope_estimate / slope_se
  
  # Kalculerar frihetsgraderna
  df <- df.residual(model)
  
  # Tar fram p-värde
  p_value <- 2 * pt(abs(t_value), df = df, lower.tail = FALSE)  # Two-tailed test
  
  # Skriver ut t-värdet and p-värdet
  cat("Summary for Year", year, ":\n")
  print(summary(model))
  print(plots[[year]])
  print(paste("t-value:", t_value))
  print(paste("p-value:", p_value))
}
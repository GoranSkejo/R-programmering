library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(leaps)

# Läser in data
Fordon <- read_excel("C:/Users/User/Downloads/Fordon.xlsx")
Bostäder <- read_excel("C:/Users/User/Downloads/Bostäder.xlsx")

# Ersätter NA-värden med medianen för varje kolumn i Fordon och Bostäder datasets
Fordon_cleaning <- Fordon
Fordon_cleaned <- lapply(Fordon_cleaning, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
Bostäder_cleaning <- Bostäder
Bostäder_cleaned <- lapply(Bostäder_cleaning, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

# Konverterar listan till en dataframe
Fordon_clean <- data.frame(Fordon_cleaned)
Bostäder_clean <- data.frame(Bostäder_cleaned)

# Exkluderar den första kolumnen från Fordon och Bostäder
Fordon_clean <- Fordon_clean[,-1]
Bostäder_clean <- Bostäder_clean[,-1]

# Ser till att kolumnnamnen är unika i Fordon_clean och Bostäder_clean
# genom att lägga till ett prefix till varje kolumnnamn i Bostäder_clean
names(Bostäder_clean) <- paste("Bostäder", names(Bostäder_clean), sep="_")

# Kombinerar 'Fordon_clean' och 'Bostäder' till en enda dataframe
combined_data <- cbind(Fordon_clean, Bostäder_clean)

# Bygger modell och väljer den bästa modellen för varje delmängdsstorlek
best_subsets <- regsubsets(År.2013 ~ ., data = combined_data, nvmax = ncol(combined_data) - 1, method = "exhaustive")

# Val av den bästa modellen bland alla delmängder
subset_results <- summary(best_subsets)
best_model <- which.min(subset_results$bic)

# Visar den bästa modellen
print(subset_results$which[best_model, ])
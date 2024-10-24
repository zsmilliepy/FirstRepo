install.packages("readr") #install the readr package
library(readr) #loads the readr package
Gannel <- read_csv("/cloud/project/Gannel.csv") # reads the CSV file located at the specified path into a data frame named Gannel
df<-Gannel
df
str(df)
dim(df)
class(df)

df[5, 1]
df[2, ]
# Get all data from the 4th column
df[, 4]
# Get values from specific rows and a specific column
df[c(3, 10, 16, 17), 3]


# Access a column using its name
df$fe
# Install packages
install.packages("dplyr", dependencies = TRUE)
install.packages("tidyr", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("tidyverse")

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)

# Assign df to the dataset
df<-Gannel
# Access a column using its name
df$"Spergularia media"

# Rename columns for readability
df <- df %>%
  rename_with(~gsub(" ", "_", .)) %>%
  rename_with(~tolower(.)) #optional_convert all the names into lower case.

names(df)

# Access a column using its name
df$"spergularia_media"

mean(df$mg, na.rm = TRUE)
df$mg > 8500
# Calculate mean of Mg for rows where Mg is <= 8500
mean(df$mg[df$mg <= 8500])

mean(df$mg[df$mg <= 8500], na.rm = TRUE)

# Load necessary libraries
install.packages("tidyverse")  # Uncomment if not installed
install.packages("reshape2")    # Uncomment if not installed
install.packages("corrplot")

library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(reshape2)

# Load necessary libraries
library(dplyr)

# Select only numeric columns for correlation analysis
numeric_data <- df %>%
  select(where(is.numeric))

sapply(numeric_data, function(x) sum(is.na(x)))

# Remove rows with missing values
cleaned_data <- na.omit(numeric_data)

# Calculate the correlation matrix
correlation_matrix <- cor(cleaned_data)

# View the correlation matrix
print(correlation_matrix)

# Melt the correlation matrix for ggplot
melted_correlation <- melt(correlation_matrix)

# Plot the correlation heatmap
ggplot(melted_correlation, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap between Environmental and Biological Data")


# Load necessary libraries
install.packages("pheatmap")
library(pheatmap)

# Remove rows with NA values
numeric_data <- na.omit(numeric_data)

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Loop through column names and display them
for (i in 1:length(names(df))) {
  # Display original name
  cat("Column", i, "name:", names(df)[i], "\n")
}

# Display all column names at the end
print(names(df))


library(readr)
dataframe <- read_csv("ProgConstructionsMerged.csv")
View(dataframe)
dataframe$construction_clean <- as.factor(dataframe$construction_clean)
dataframe$clusters_olga <- as.factor(dataframe$clusters_olga)
dataframe$construction_type <- as.factor(dataframe$construction_type)
dataframe$`author school language` <- as.factor(dataframe$`author school language`)
dataframe$`class id` <- as.factor(dataframe$`class id`)
fit <- vglm(construction_clean ~ clusters_olga, data = dataframe, family=multinomial)
summary(fit)
library(nnet)
model <- multinom(construction_clean ~ clusters_olga, data = dataframe, family=multinomial)
summary(model)
library(stargazer)
stargazer(model, type="text", out = "model.htm")


# Install necessary packages if not already installed
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("vctrs", dependencies = TRUE)

# Load necessary libraries
library(FactoMineR)
library(factoextra)

# Load the data (assuming your CSV file is saved as "ProgConstructionsMerged.csv")
data <- read.csv("ProgConstructionsMerged.csv")


# Filter out the "ambiguous" category
filtered_data <- subset(dataframe, construction_clean != "ambiguous")

# Prepare the data for MCA
# Select relevant columns
mca_data <- filtered_data[, c("author school language", "class id", "clusters", "construction_clean")]

# Convert columns to factors
mca_data$`author school language` <- as.factor(mca_data$`author school language`)
mca_data$`class id` <- as.factor(mca_data$`class id`)
mca_data$clusters <- as.factor(mca_data$clusters)
mca_data$construction_clean <- as.factor(mca_data$construction_clean)

# Perform MCA
mca_result <- MCA(mca_data, quali.sup = 1:3, graph = FALSE)

# Visualize the MCA results
# Plot individuals
fviz_mca_ind(mca_result, label = "none", habillage = "clusters", palette = "jco", addEllipses = TRUE, ellipse.level = 0.95) +
  labs(title = "MCA of Leonide Authors Based on Constructional Schema Usage and Background Variables")

# Step 1: Update the cli package
install.packages("cli", dependencies = TRUE)

# Step 2: Install necessary packages
install.packages("FactoMineR", dependencies = TRUE)
install.packages("factoextra", dependencies = TRUE)

# install.packages("devtools")
library("devtools")
install_github("kassambara/factoextra")
library("factoextra")
library(ggplot2)


# Plot variables to see how different constructional schemas and categorical variables contribute to the dimensions
fviz_mca_var(mca_result, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")) +
  labs(title = "MCA Variable Plot")


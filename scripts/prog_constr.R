library(readxl)   # for reading Excel files
library(dplyr)     # for data manipulation
library(summarytools)  # for descriptive statistics
library(ggplot2)# for data visualization

#import data
data <- read_excel("ProgConstructions.xlsx")
str(data)


#let's try a network graph viz for meaning and construction clean
library(tidygraph)
library(ggraph)

graph <- tbl_graph(data, directed = TRUE) %>%
  mutate(construction_clean = as.factor(construction_clean),
         meaning = as.factor(meaning))

# Plot the graph
ggraph(graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(aes(color = construction_clean)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void() +
  theme(legend.position = "none")


# Load required packages
library(igraph)

# Prepare your data
construction_types <- unique(data$construction_clean)
meanings <- unique(data$meaning)

# Calculate the number of vertices
num_vertices <- length(construction_types) + length(meanings)

# Create an empty graph object
g <- graph.empty(n = num_vertices, directed = TRUE)


# Add construction types as nodes
V(g)[1:length(construction_types)]$name <- construction_types

# Add meanings as nodes
V(g)[(length(construction_types) + 1):num_vertices]$name <- meanings

# Set colors for construction types and meanings
V(g)[1:length(construction_types)]$color <- "red"  # Red for construction types
V(g)[(length(construction_types) + 1):num_vertices]$color <- "blue"  # Blue for meanings

# Add edges between construction types and meanings
for (i in 1:length(construction_types)) {
  for (j in 1:length(meanings)) {
    if (any(data$construction_clean == construction_types[i] & data$meaning == meanings[j])) {
      g <- add_edges(g, c(i, length(construction_types) + j))
    }
  }
}

# Plot the network graph with custom colors
plot(g, layout = layout.auto, vertex.label.dist = 0.5, vertex.label.cex = 0.7, edge.arrow.size = 0)


# Calculate degree centrality for nodes
degree_centrality <- degree(g)

# Print summary statistics
cat("Summary Statistics:\n")
cat("Number of vertices:", vcount(g), "\n")
cat("Number of edges:", ecount(g), "\n")
cat("Average degree:", mean(degree_centrality), "\n")
cat("Maximum degree:", max(degree_centrality), "\n")
cat("Minimum degree:", min(degree_centrality), "\n")


# Calculate degree centrality for nodes
degree_centrality <- degree(g)

# Visualize degree centrality using a bar plot
barplot(degree_centrality, names.arg = V(g)$name, col = V(g)$color, las = 2,
        main = "Degree Centrality of Nodes",
        xlab = "Nodes", ylab = "Degree Centrality")
legend("topright", legend = c("Construction Types", "Meanings"),
       fill = c("red", "blue"))

# Add numerical values beside the bars
text(x = 1:length(degree_centrality), y = degree_centrality, labels = degree_centrality, pos = 3, cex = 0.8)

# Add legend
legend("topright", legend = c("Construction Types", "Meanings"),
       fill = c("red", "blue"))

contingency_table <- table(data$construction_clean, data$meaning)
chi_squared_result <- chisq.test(contingency_table)
print(chi_squared_result)
# Calculate standardized residuals
residuals <- residuals(chi_squared_result)

# Visualize residuals
residual_matrix <- matrix(residuals, nrow = nrow(contingency_table))
rownames(residual_matrix) <- rownames(contingency_table)
colnames(residual_matrix) <- colnames(contingency_table)
print(residual_matrix)

#posthoc
pairwise_chi_squared_result <- pairwise.chisq.test(contingency_table, p.adjust.method = "bonferroni")
print(pairwise_chi_squared_result)


#perform MCA
# Load required packages
install.packages("FactoMineR")  
library(FactoMineR)

ggplot(data, aes(x = meaning, fill = construction_clean)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ task_type)+
  labs(title = "Constructions and meaning types by task",
       x = "Function",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Select only the categorical variables you want to analyze
categorical_data <- data[, c("meaning", "task_type", "construction_clean", "Biber_semantic_domain", "subject")]

variable_colors <- c("blue", "red", "green", "purple", "yellow")
# Modify variable labels to display only levels without variable names
variable_labels <- apply(categorical_data, 2, function(x) gsub(".*_", "", levels(x)))

# Set modified variable labels
colnames(mca_result$var$coord) <- variable_labels

# Perform Multiple Correspondence Analysis
mca_result <- MCA(categorical_data, graph=FALSE)

# Print summary of the MCA results
summary(mca_result)

# Plot the MCA results
plot(mca_result,invis="ind",title="MCA graph of the categories", cex = 0.8, cex.axis = 0.8, cex.lab = 0.8, cex.main = 1.2)
par(mar = c(5, 5, 4, 2) + 0.1)
options(repr.plot.width=8, repr.plot.height=6)
plot(mca_result,invis="ind",title="MCA graph", cex = 0.8, cex.axis = 0.8, cex.lab = 0.8, cex.main = 1.2)
dimdesc(mca_result)

plot.new()
# Add a legend
legend("topright", legend = colnames(categorical_data), fill = variable_colors, title = "Variables")
plot(mca_result,invis="ind",title="MCA graph of the categories", cex = 0.8, cex.axis = 0.8, cex.lab = 0.8, cex.main = 1.2)

#heatmap between adv and meanings
heatmap(table(data$adv, data$meaning))

#convert columns into factors
meaning <- as.factor(data$meaning)
semantics <- as.factor(data$Biber_semantic_domain)
subject <- as.factor(data$subject)
construction <- as.factor(data$construction_clean)
task <- as.factor(data$task_type)
lemma <- as.factor(data$Ving_lemma)
adverb <- as.factor(data$adv)

#let's see how these constructions develop across speakers and year
install.packages("tidyverse")
library(tidyverse)

construction_summary <- data %>%
  group_by(author_id, construction_clean) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

library(ggplot2)

ggplot(construction_summary, aes(x = author_id, y = count, fill = construction_clean)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Construction Usage by Author",
       x = "Author ID",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

yearly_summary <- data %>%
  group_by(task_year, construction_clean) %>%
  summarise(count = n()) %>%
  arrange(task_year, desc(count))

ggplot(yearly_summary, aes(x = task_year, y = count, color = construction_clean)) +
  geom_line() +
  labs(title = "Construction Usage Across Years",
       x = "Year",
       y = "Count") +
  theme_minimal()

ggplot(construction_summary, aes(x = construction_clean, y = count, fill = author_id)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Variation of Constructions Used by Authors",
       x = "Construction",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()  # Adjust color scale as needed


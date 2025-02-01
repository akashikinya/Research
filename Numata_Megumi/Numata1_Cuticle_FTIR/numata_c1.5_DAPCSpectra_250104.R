#For DAPC analysis(PCA-LDA) 

#Import necessary libraries
library(adegenet)
library(ggplot2)
library(dplyr)
library(caret)
library(igraph)

#Clean up the brain
rm(list=ls())

#Prepare output data folder
DesktopPath <- file.path("C:/users/", Sys.getenv("USERNAME"), "/desktop/", paste0(format(Sys.Date(), "%y%m%d"), "_DAPCSpectra/"))
if (!dir.exists(DesktopPath)) {
  dir.create(DesktopPath)
}
setwd(DesktopPath)

#Select csv
inputfile <- file.choose()
#Read CSV
data <- read.csv(inputfile)

#The second character of area is stored in a new column,
#and “o” is converted to “D” and “u” is converted to “B”.
data$side <- substr(data$area, 2, 2)
data$side <- ifelse(substr(data$area, 2, 2) == "o", "D",
                    ifelse(substr(data$area, 2, 2) == "u", "B", data$side))

#Move 'side' column before 'identifier'
data <- data %>%
  relocate(side, .before = identifier)

#The first letter of area is stored in a new column and converted to capital letters
data$area <- toupper(substr(data$area, 1, 1))

#Reverse order data frames
numeric_data <- data[, 6:ncol(data)]  #Extract only wavenumber data
numeric_data_reverse <- numeric_data[, ncol(numeric_data):1]  # Reverse the column order of wavenumbers
data_reverse <- cbind(data[, 1:5], numeric_data_reverse) #Combined with original data frame

#Use 400-3600 data
data_reverse <-  dplyr::select(data_reverse, (1:3206))
colnames(data_reverse)[3206]

#Perform either 1 or 2
#1 
#Create data frames for species and Wavenumber
#Extract lines where area is 'S'
s_data <- data_reverse %>% filter(area == "S")
df <- s_data[, c(2, 6:ncol(s_data))]

#2
#Create data frames for Ck and wavenumber
#Extract lines where species is 'Ck'
ck_data <- data_reverse %>% filter(species == "Ck")
df <- ck_data[, c(3, 6:ncol(ck_data))]


#Used as a group label
#1
group <- df$species
#2
group <- df$area

#Seed setting
set.seed(123)
k_folds <- 3
folds <- createFolds(group, k = k_folds, list = TRUE)

#Vector to store the result
accuracy_results <- vector("numeric", length = k_folds)


#1
#Execution of DAPC and evaluation of accuracy
for (i in seq_along(folds)) {
  test_indices <- folds[[i]]
  train_indices <- unlist(folds[-i])
  
  #Training and test data directly from df
  train_data <- df[train_indices, ]
  test_data <- df[test_indices, ]
  
  #Extract labels and wavenumber data from training data
  train_group <- train_data$species  
  train_wavenumber_data <- train_data[, -1]  #Exclude the first row because it is a label
  
  #Execution of DAPC
  dapc_result <- dapc(train_wavenumber_data, grp = train_group, n.pca = 10, n.da = 2, scale = TRUE)
  
  #Make predictions with test data
  test_wavenumber_data <- test_data[, -1]  #Exclude the first row because it is a label
  predicted_labels <- predict.dapc(dapc_result, newdata = test_wavenumber_data)$assign
  actual_labels <- as.factor(test_data$species)  
  
  #Create a mixing matrix
  confusion_matrix <- confusionMatrix(factor(predicted_labels, levels = levels(actual_labels)), actual_labels)
  
  #Get accuracy
  accuracy_results[i] <- confusion_matrix$overall['Accuracy']
}


#2
#Execution of DAPC and evaluation of accuracy
for (i in seq_along(folds)) {
  test_indices <- folds[[i]]
  train_indices <- unlist(folds[-i])
  
  #Training and test data directly from df
  train_data <- df[train_indices, ]
  test_data <- df[test_indices, ]
  
  #Extract labels and wavenumber data from training data
  train_group <- train_data$area  
  train_wavenumber_data <- train_data[, -1]  #Exclude the first row because it is a label.
  
  #Execution of DAPC
  dapc_result <- dapc(train_wavenumber_data, grp = train_group, n.pca = 10, n.da = 2, scale = TRUE)
  
  #Make predictions with test data
  test_wavenumber_data <- test_data[, -1]  #Exclude the first row because it is a label.
  predicted_labels <- predict.dapc(dapc_result, newdata = test_wavenumber_data)$assign
  actual_labels <- as.factor(test_data$area)  
  
  #Create a mixing matrix
  confusion_matrix <- confusionMatrix(factor(predicted_labels, levels = levels(actual_labels)), actual_labels)
  
  #Get accuracy
  accuracy_results[i] <- confusion_matrix$overall['Accuracy']
}


#Extract only Wavenumber data and run DAPC
wavenumber_data <- df[, -1]  #Exclude the first row

#Cross-validation to obtain appropriate number of PCA axes
xval_result <- xvalDapc(
  train_wavenumber_data,
  grp = train_group,
  n.pca.max = 100,
  n.rep = 3
)

#Obtain optimal number of PCA axes
optimal_n_pca <- xval_result$`Cross-Validation Results`$n.pca[which.max(xval_result$`Cross-Validation Results`$success)]
print(paste("Optimal number of PCA axes:", optimal_n_pca))

#Re-run with optimal number of PCA axes
dapc_result_2 <- dapc(
  train_wavenumber_data,
  grp = train_group,
  n.pca = optimal_n_pca,
  n.da = 2,
  scale = TRUE
)


#Get DAPC results
dapc_summary_2 <- summary(dapc_result_2)

#Convert required elements into data frames
summary_df <- data.frame(
  n.dim = dapc_summary_2$n.dim,
  n.pop = dapc_summary_2$n.pop,
  assign.prop = dapc_summary_2$assign.prop,
  assign.per.pop = dapc_summary_2$assign.per.pop,
  prior.grp.size = dapc_summary_2$prior.grp.size,
  post.grp.size = dapc_summary_2$post.grp.size
)

#Save as CSV file
write.csv(summary_df, "dapc2_summary.csv", row.names = FALSE)



#Scatter plot drawing
#Converts DAPC results to data frames
dapc_df_2 <- as.data.frame(dapc_result_2$ind.coord)
dapc_df_2$group <- dapc_result_2$grp  #Add group information
#Calculate center of gravity for each group
centroids_2 <- dapc_df_2 %>%
  group_by(group) %>%
  summarize(LD1 = mean(LD1), LD2 = mean(LD2))
#Create a data frame for a line connecting the center of gravity and the individual
lines_df_2 <- dapc_df_2 %>%
  left_join(centroids_2, by = "group", suffix = c("", "_centroid")) %>%
  rename(LD1_centroid = LD1_centroid, LD2_centroid = LD2_centroid)
#Obtain the coordinates of the individual DAPC results
coords_2 <- as.matrix(dapc_result_2$ind.coord[, 1:2])  #Use 1 and 2 axes

#Compute minimum generating tree
dist_matrix_2 <- dist(coords_2)  #Euclidean distance
mst_2 <- mst(graph_from_adjacency_matrix(as.matrix(dist_matrix_2), weighted = TRUE))

#Edges of the minimum generation tree into a data frame
edges_2 <- as_data_frame(mst_2, what = "edges") %>%
  mutate(x = coords_2[from, 1], y = coords_2[from, 2],
         xend = coords_2[to, 1], yend = coords_2[to, 2])

#1
#Scatter Plot with Axis Ticks and Labels
png("dapc_scatter_plot_with_axes.png", width = 2400, height = 1350, res = 400)

#Specify colors for each group
group_colors <- c("Ck" = "darkred", "Ic" = "darkblue", "Lj" = "darkgreen")

ggplot() +
  #Minimally Generated Tree Lines
  geom_segment(data = edges_2, aes(x = x, y = y, xend = xend, yend = yend), color = "gray", alpha = 0.7) +
  #Line connecting the individual and the center of gravity
  geom_segment(data = lines_df_2, aes(x = LD1_centroid, y = LD2_centroid, xend = LD1, yend = LD2, color = group), alpha = 0.5) +
  #Individual point
  geom_point(data = dapc_df_2, aes(x = LD1, y = LD2, color = group), size = 2) +
  #Manually add a line on the 0 axis
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +  # y = 0 の水平線
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed") +  # x = 0 の垂直線
  #Apply custom colors
  scale_color_manual(values = group_colors) +
  #Customize Theme
  theme_minimal() +
  theme(
    #Add a border lines
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    #Completely remove grid lines
    panel.grid = element_blank(),
    #Ticks set inward
    axis.ticks.length = unit(-0.2, "cm"),
    axis.ticks = element_line(color = "black"),
    #Axis Text Adjustment
    axis.text = element_text(margin = margin(t = 5, r = 5, b = 5, l = 5)),
    #Legend Adjustment
    legend.position = "none"
  ) +
  labs(x = "Discriminant Function 1", y = "Discriminant Function 2", color = "Group")

#Close the image device
dev.off()


#2
#Scatter Plot with Axis Ticks and Labels
png("dapc_scatter_plot_with_axes.png", width = 2400, height = 1350, res = 400)

#Specify colors for each group
group_colors <- c("P" = "lightpink2", "Q" = "coral", "R" = "red", "S" = "darkred")
ggplot() +
  #Minimally Generated Tree Lines
  geom_segment(data = edges_2, aes(x = x, y = y, xend = xend, yend = yend), color = "gray", alpha = 0.7) +
  #Line connecting the individual and the center of gravity
  geom_segment(data = lines_df_2, aes(x = LD1_centroid, y = LD2_centroid, xend = LD1, yend = LD2, color = group), alpha = 0.5) +
  #Individual point
  geom_point(data = dapc_df_2, aes(x = LD1, y = LD2, color = group), size = 2) +
  #Manually add a line on the 0 axis
  geom_hline(yintercept = 0, color = "gray50", linetype = "dashed") +  # y = 0 の水平線
  geom_vline(xintercept = 0, color = "gray50", linetype = "dashed") +  # x = 0 の垂直線
  #Apply custom colors
  scale_color_manual(values = group_colors) +
  #Customize Theme
  theme_minimal() +
  theme(
    #Add a border lines
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    #Completely remove grid lines
    panel.grid = element_blank(),
    #Ticks set inward
    axis.ticks.length = unit(-0.2, "cm"),
    axis.ticks = element_line(color = "black"),
    #Axis Text Adjustment
    axis.text = element_text(margin = margin(t = 5, r = 5, b = 5, l = 5)),
    #Legend Adjustment
    legend.position = "none"
  ) +
  labs(x = "Discriminant Function 1", y = "Discriminant Function 2", color = "Group")
#Close the image device
dev.off()



#Make predictions with test data
#1
actual_labels <- test_data$species
#2
actual_labels <- test_data$area

test_wavenumber_data <- test_data[, -1]

#Predicted labels when PCA axis is optimized
predicted_labels <- predict.dapc(dapc_result_2, newdata = test_wavenumber_data)$assign

#1
actual_labels <- as.factor(test_data$species)  #Converted to factor type
#2
actual_labels <- as.factor(test_data$area)  #Converted to factor type

predicted_labels <- factor(predicted_labels, levels = levels(actual_labels))

#Create a confusion matrix
confusion_matrix <- confusionMatrix(predicted_labels, actual_labels)

#Print a confusion matrix
print(confusion_matrix)

#Save as CSV
write.csv(as.data.frame(confusion_matrix$table), "confusion_matrix.csv", row.names = TRUE)


#True Positives (TP), False Positives (FP), True Negatives (TN), False Negatives (FN)
confusion_matrix_data <- confusion_matrix$table
TP <- diag(confusion_matrix_data)  # 対角成分がTP
FP <- colSums(confusion_matrix_data) - TP
FN <- rowSums(confusion_matrix_data) - TP
TN <- sum(confusion_matrix_data) - (TP + FP + FN)

#Accuracy, Sensitivity, Specificityの計算
Accuracy <- (TP + TN) / (TP + TN + FP + FN)
Sensitivity <- TP / (TP + FN)
Specificity <- TN / (TN + FP)
Precision <- TP / (TP + FP)

#Summarize results into a data frame
results <- data.frame(
  Class = rownames(confusion_matrix_data),
  TP = TP,
  FP = FP,
  FN = FN,
  TN = TN,
  Accuracy = round(Accuracy, 3),
  Sensitivity = round(Sensitivity, 3),
  Specificity = round(Specificity, 3),
  Precision = round(Precision, 3)
)
#View Results
print(results)

#Save as CSV
write.csv(results, "model_metrics.csv", row.names = FALSE)



#Obtain eigenvalues of discriminant axis
eig_values <- dapc_result_2$eig

#Calculate the contribution ratio
eig_contributions <- eig_values / sum(eig_values) 

#Create data frame
eig_df <- data.frame(
  Axis = paste0("LD", seq_along(eig_values)),  # 軸名（LD1, LD2, ...）
  Eigenvalue = eig_values,                    # 固有値
  Contribution = eig_contributions  
)
#Confirmation of data frame
print(eig_df)

#Save as CSV
write.csv(eig_df, "pdca_eigenvalues_contributions.csv", row.names = FALSE)

#Obtain DA1 and DA2 contribution data
da1_contributions <- dapc_result_2$var.contr[, 1]  # DA1
da2_contributions <- dapc_result_2$var.contr[, 2]  # DA2

#Summarize into a data frame
contributions_df <- data.frame(
  Variable = rownames(dapc_result_2$var.contr),
  da1_Contribution = da1_contributions,
  da2_Contribution = da2_contributions
)

#Save as CSV 
write.csv(contributions_df, "da1_da2_contributions.csv", row.names = FALSE)


#For fig. 3. PCA plot

#Import necessary libraries
library(conflicted)
library(dplyr)
library(ggplot2)
library(readr)
library(psych)
library(ellipse)
library(ggfortify)

#Clean up the brain
rm(list=ls())

#Prefer dplyr::filter as default filter function
conflicted::conflicts_prefer(dplyr::filter)

#Select csv
print("alldata_xxxxxx or xxxxxx_ABS(SD) should be selected")
inputfile <- file.choose()

#Prepare output folder and its path
DesktopPath <- file.path("C:/users/", Sys.getenv("USERNAME"), "/desktop/", paste0(format(Sys.Date(), "%y%m%d"), "_PCAloadingplot/"))
if (!dir.exists(DesktopPath)) {
  dir.create(DesktopPath)
}
setwd(DesktopPath)

#Read CSV
filename <-  basename(inputfile)
rowspecs <-  read.csv(inputfile, header = T)

#Split area into area and side
rowspecs$side <- ifelse(grepl("^[pqrs]o$", rowspecs$area), "o", "u")
rowspecs$area <- substr(rowspecs$area, 1, 1)

#Create a new column combining species and area
rowspecs$species_area <- paste(rowspecs$species, rowspecs$area, sep = "_")

#Move 'side', 'species_area' columns before 'identifier'
rowspecs <- rowspecs %>%
  relocate(side, .before = identifier)
rowspecs <- rowspecs %>%
  relocate(species_area, .before = identifier)

#Reverse order data frames
numeric_data <- rowspecs[, 7:ncol(rowspecs)]  #Extract only wavenumber data
numeric_data_reverse <- numeric_data[, ncol(numeric_data):1]  #Reverse the column order of wavenumbers
data_reverse <- cbind(rowspecs[, 1:6], numeric_data_reverse) #Combined with original data frame

#This is used when dealing with all data
rowspecs_selected <- rowspecs

#Use 400-3600 data
rowspecs_selected <-  dplyr::select(data_reverse, (1:3207))
colnames(rowspecs_selected)[3207]


#When using filters, run the following script
#Choose 1 or 2 (skip PCA for alldata)
#1. Select species and side (change attributes as needed)
rowspecs_selected <- rowspecs_selected %>% filter(species == "Ck") #& side == "u")
#2. Select area and side
rowspecs_selected <- rowspecs_selected %>% filter(area == "s") #& side == "o")


#Exclude numeric columns
rowspecs_numeric <- rowspecs_selected %>% select_if(is.numeric)

#Separate "identifier" column (category info)
id_1 <-  dplyr::select(rowspecs_selected, species_area, side)

#Perform pca analysis using prcomp
pc = prcomp(rowspecs_numeric, scale =T,)

#Display the summary
summary(pc)

#Preparation of score data output
pc1_score <-  pc$x[,1]

pc2_score <-  pc$x[,2]

scoreonly <-  as.data.frame(pc$x)
score <- cbind(id_1, scoreonly)

#Export the score data as csv
#Data is PC1-PC2 score
filename_PC12_score <- paste0(format(Sys.Date(), "%y%m%d"),"_PC12_score", ".csv")
filepath_PC12_score <- file.path(DesktopPath, filename_PC12_score)
write.csv(score, filepath_PC12_score, row.names=FALSE)

#Export rotation data as csv file
rotationonly <-  as.data.frame(pc$rotation)
filename_pca_rotation <- paste0(format(Sys.Date(), "%y%m%d"),"_PCA_rotation", ".csv")
filepath_pca_rotation <- file.path(DesktopPath, filename_pca_rotation)
write.csv(rotationonly,filepath_pca_rotation, row.names=FALSE)

#Export sdev data as csv file
sdevonly <- as.data.frame(pc$sdev)
filename_pca_sdev <- paste0(format(Sys.Date(), "%y%m%d"),"_PCA_sdev", ".csv")
filepath_pca_sdev <- file.path(DesktopPath, filename_pca_sdev)
write.csv(sdevonly, filepath_pca_sdev, row.names=FALSE)

#Calculate the loadings, and export as csv file
loadingdata <- sweep(pc$rotation, MARGIN=2, pc$sdev, FUN="*")
filename_pca_loading <- paste0(format(Sys.Date(), "%y%m%d"),"_PCA_loading", ".csv")
filepath_pca_loading <- file.path(DesktopPath, filename_pca_loading)
write.csv(loadingdata,filepath_pca_loading, row.names=FALSE)

#Draw the pc1_pc2 scoreplot
#size of the dots in the plot can be changed
#by modifying the location of "geom_point(size=)
dev.new()
color_scale <- c("Ck_p" = "lightpink2", "Ck_q" = "coral", "Ck_r" = "red", "Ck_s" = "darkred",
                 "Ic_p" = "skyblue", "Ic_s" = "darkblue",
                 "Lj_q" = "palegreen", "Lj_r" = "green", "Lj_s" = "darkgreen")

pca_scoreplot <-  ggplot(score, aes(x = PC1,y = PC2,shape = side)) +
  geom_point(aes(color = species_area), size = 3) +
  scale_color_manual(values=color_scale) +
  scale_shape_manual(values = c("o" = 1, "u" = 0)) +
  theme_bw()+
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect (fill = "transparent", color = "black", linewidth = 1), 
        text = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks.length = unit(-0.2, "cm"),
        legend.position = "none")#+
  
#Drawing an Ellipse
pca_scoreplot <- pca_scoreplot +
  stat_ellipse(aes(color = species_area, linetype = side), type = "norm", level = 0.95) +
  scale_linetype_manual(values = c("o" = "solid", "u" = "dashed"))

print(pca_scoreplot)

#Save the plot as png format
#you can change to .jpeg, .tiff, etc
#unit is in inch
filename_pca_scoreplot <- paste0(format(Sys.Date(), "%y%m%d"),"_PCA_scoreplot", ".png")
filepath_pca_scoreplot <- file.path(DesktopPath, filename_pca_scoreplot)
ggsave(filepath_pca_scoreplot, dpi=400, width=10, height=6)


#Basic plot settings
pca_scoreplot2 <- ggplot(score, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = species_area, shape = side)) +  #Plot the points of the principal component score
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  #Add a line with zero principal component contribution
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  #Add a line with zero principal component contribution
  coord_cartesian(xlim = c(-250, 250), ylim = c(-150, 150)) +  #Adjust coordinates
  theme_bw()

#Calculate absolute value of loading
loading_abs <- abs(pc$rotation)

#Extract the top 10 loadings
top_loading_pc1 <- head(loading_abs[order(loading_abs[, 1], decreasing = TRUE), ], 10)
top_loading_pc1 <- data.frame(Variable = rownames(top_loading_pc1), PC1 = pc$rotation[rownames(top_loading_pc1), "PC1"],
                              PC2 = pc$rotation[rownames(top_loading_pc1), "PC2"])
#Export the score data as csv
#Data is PC1-PC2 score
filename_toploading_score <- paste0(format(Sys.Date(), "%y%m%d"),"_toploading", ".csv")
filepath_toploading_score <- file.path(DesktopPath, filename_toploading_score)
write.csv(top_loading_pc1, filepath_toploading_score, row.names=FALSE)


#Scaling to adjust arrow length according to contribution
scale_factor <- 5000  #Set scaling factor
top_loading_pc1$PC1 <- top_loading_pc1$PC1 * scale_factor
top_loading_pc1$PC2 <- top_loading_pc1$PC2 * scale_factor

#Add loading as arrows
pca_scoreplot2 <- pca_scoreplot2 +
  geom_segment(data = top_loading_pc1,  #Use data frame of top 10 loading scores
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "inches")),
               color = "red", linewidth = 1) +
  geom_text(data = top_loading_pc1, aes(x = PC1, y = PC2, label = Variable), 
            vjust = -0.5, hjust = 1.5, color = "red")  #Add label

#Show Plot
print(pca_scoreplot2)

#Save the plot as png format
#you can change to .jpeg, .tiff, etc
#unit is in inch
filename_pca_scoreplot2 <- paste0(format(Sys.Date(), "%y%m%d"),"_PCA_scoreplot_loading", ".png")
filepath_pca_scoreplot2 <- file.path(DesktopPath, filename_pca_scoreplot2)
ggsave(filepath_pca_scoreplot2, dpi=400, width=6, height=6)


#Draw the pc1_pc3 scoreplot
#size of the dots in the plot can be changed
#by modifying the location of "geom_point(size=)
dev.new()
color_scale <- c("Ck_p" = "lightpink2", "Ck_q" = "coral", "Ck_r" = "red", "Ck_s" = "darkred",
                 "Ic_p" = "skyblue", "Ic_s" = "darkblue",
                 "Lj_q" = "palegreen", "Lj_r" = "green", "Lj_s" = "darkgreen")

pca_scoreplot3 <-  ggplot(score, aes(x = PC1,y = PC3,shape = side)) +
  geom_point(aes(color = species_area), size = 3) +
  scale_color_manual(values=color_scale) +
  scale_shape_manual(values = c("o" = 1, "u" = 0)) +
  theme_bw()+
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect (fill = "transparent", color = "black", linewidth = 1), 
        text = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.ticks.length = unit(-0.2, "cm"),
        legend.position = "none")

#Drawing an Ellipse
pca_scoreplot3 <- pca_scoreplot3 +
  stat_ellipse(aes(color = species_area, linetype = side), type = "norm", level = 0.95) +
  scale_linetype_manual(values = c("o" = "solid", "u" = "dashed"))

print(pca_scoreplot3)

#Save the plot as png format
#you can change to .jpeg, .tiff, etc
#unit is in inch
filename_pca_scoreplot3 <- paste0(format(Sys.Date(), "%y%m%d"),"_PCA_scoreplot_PC3", ".png")
filepath_pca_scoreplot3 <- file.path(DesktopPath, filename_pca_scoreplot3)
ggsave(filepath_pca_scoreplot3, dpi=400, width=6, height=6)


#Extract contribution data
contribution <- as.data.frame(t(summary(pc)$importance))
names(contribution) <- c("standard_deviation","proportion_of_variance","cumulative_proportion")
contri_rownames <- as.data.frame(rownames(contribution))
names(contri_rownames) <- "PC"
contribution <- cbind(contribution, contri_rownames)

#Save the contribution data as csv file
filename_pca_contribution <- paste0(format(Sys.Date(), "%y%m%d"),"_PCA_contribution", ".csv")
filepath_pca_contribution <- file.path(DesktopPath, filename_pca_contribution)
write.csv(contribution, filepath_pca_contribution, row.names=FALSE)

#Extract top 9 from the contribution data, and save as a png file
top9_contribution <-  dplyr::slice(contribution, 1:9)
contribution_plot <-  ggplot(top9_contribution, aes(x = PC, y = proportion_of_variance)) +
  geom_bar(stat="identity", fill="forestgreen") +
  theme_bw()+
  theme(axis.ticks.length = unit(-0.2, "cm"))
print(contribution_plot)
filename_pca_contribution_plot <- paste0(format(Sys.Date(), "%y%m%d"),"_PCA_contribution_plot", ".png")
filepath_pca_contribution_plot <- file.path(DesktopPath, filename_pca_contribution_plot)
ggsave(file = filepath_pca_contribution_plot,
             plot = contribution_plot, dpi=400,
             width=6, height=6)

#Prepare data for loading plot
pc_loading <- data.frame(t(cor(pc$x,rowspecs_numeric)))
pc_score <- data.frame(pc$x)

#draw the 2D-loading plot using ggplot
#color info can be seen in the following website
#http://sape.inf.usi.ch/quick-reference/ggplot2/colour

#Generate colors based on number of rows in pc_loading
colors <- rainbow(nrow(pc_loading))
#Displays a bar graph to check the generated colors
barplot(rep(1, length(colors)), col = colors, border = NA, main = "Generated Colors")


g0 <- ggplot()
g0 <- g0 + geom_segment(data=pc_loading,
                        aes(x=0,y=0,xend=(PC1*1),yend=(PC2*1)),
                        colour = colors,alpha=0.2,linewidth=0.5)

g0 <- g0 + xlab("PC1")
g0 <- g0 + ylab("PC2")
g0 <- g0 + theme_bw()
g0 <- g0 + theme(panel.background = element_rect(fill = "transparent"),
                 panel.border = element_rect (fill = "transparent", color = "black", linewidth =  1), 
                 axis.title.x = element_text(size = 14),
                 axis.title.y = element_text(size = 14),
                 axis.text.x = element_text(size = 14),
                 axis.text.y = element_text(size = 14),
                 axis.ticks.length = unit(-0.2, "cm"))

print(g0)

filename_pca_loading2d_plot <- paste0(format(Sys.Date(), "%y%m%d"),"_PCA_loading2Dplot", ".png")
filepath_pca_loading2D_plot <- file.path(DesktopPath, filename_pca_loading2d_plot)
ggsave(file = filepath_pca_loading2D_plot,
       plot = g0, dpi=400,width=6, height=6)

#draw the 1D-loading barplot
#prepare x-axis data
wn_x_axis <- as.data.frame(c(seq(400, 3600, by= 1)))
names(wn_x_axis) <- c("wavenumber")
pc_loading2 <- cbind(wn_x_axis, pc_loading)

#draw the PC1 loading barplot
g1 <- ggplot(data=pc_loading2, 
             aes(x=wavenumber, y=PC1))
g1 <- g1 + geom_bar(stat="identity", col=colors)
g1 <- g1 + theme_bw()+theme(panel.background = element_rect(fill = "transparent"),
                            panel.border = element_rect (fill = "transparent", color = "black", linewidth = 1), 
                            text = element_text(size = 14),
                            axis.title.x = element_text(size = 14),
                            axis.title.y = element_text(size = 14),
                            axis.text.x = element_text(size = 14),
                            axis.text.y = element_text(size = 14),
                            axis.ticks.length = unit(-0.2, "cm"),
                            legend.position = "none")
print(g1)

filename_pca_PC1_loadingplot <- paste0(format(Sys.Date(), "%y%m%d"),"_PCA_PC1_loadingplot", ".png")
filepath_pca_PC1_loadingplot <- file.path(DesktopPath, filename_pca_PC1_loadingplot)
ggsave(file = filepath_pca_PC1_loadingplot, plot = g1, dpi=400,
       width=6, height=1.5)

#draw the PC2 loading barplot
g2 <- ggplot(data=pc_loading2, 
             aes(x=wavenumber, y=PC2))
g2 <- g2 + geom_bar(stat="identity", col=colors)
g2 <- g2 + theme_bw()+theme(panel.background = element_rect(fill = "transparent"),
                            panel.border = element_rect (fill = "transparent", color = "black", linewidth = 1), 
                            text = element_text(size = 14),
                            axis.title.x = element_text(size = 14),
                            axis.title.y = element_text(size = 14),
                            axis.text.x = element_text(size = 14),
                            axis.text.y = element_text(size = 14),
                            axis.ticks.length = unit(-0.2, "cm"),
                            legend.position = "none")
print(g2)

filename_pca_PC2_loadingplot <- paste0(format(Sys.Date(), "%y%m%d"),"_PCA_PC2_loadingplot", ".png")
filepath_pca_PC2_loadingplot <- file.path(DesktopPath, filename_pca_PC2_loadingplot)
ggsave(file = filepath_pca_PC2_loadingplot, plot = g2, dpi=400,
       width=6, height=1.5)

#End of script


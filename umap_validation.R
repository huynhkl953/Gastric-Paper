library(readr)
Data_gastric_paper <- read_csv("C:/Users/User/Downloads/Data gastric paper.csv")
library(readxl)
Run <- read_excel("C:/Users/User/Downloads/Run.xlsx")


Data_gastric_paper=Data_gastric_paper
data_final=merge(Run,Data_gastric_paper,"SampleID")

library(dplyr)


control_data <- data_final %>% filter(Label.x == "0")
cancer_data <- data_final %>% filter(Label.x == "1")

feature_groups <- list(
  TMD = "starts_with('TMD')",
  GWMD = "starts_with('GWMD')",
  MOTIF_END = "starts_with('MOTIF_END')",
  flen = "starts_with('flen')",
  CNA = "starts_with('CNA')",
  GWFP_long = "starts_with('GWFP_long')",
  GWFP_short = "starts_with('GWFP_short')"
)


get_columns_starting_with <- function(df, prefix) {
  colnames(df)[startsWith(colnames(df), prefix)]
}
library(dplyr)
library(uwot)
library(ggplot2)

# Define your feature groups and their prefixes
feature_groups <- list(
  TMD = "TMD",
  GWMD = "GWMD",
  MOTIF_END = "MOTIF_END",
  flen = "flen",
  CNA = "CNA",
  GWFP_long = "GWFP_long",
  GWFP_short = "GWFP_short"
)

for (group_name in names(feature_groups)) {
  prefix <- feature_groups[[group_name]]
  
  # Get column names starting with the prefix for control
  control_cols <- get_columns_starting_with(control_data, prefix)
  # Get column names starting with the prefix for cancer
  cancer_cols <- get_columns_starting_with(cancer_data, prefix)
  
  # Select the columns
  control_feats <- control_data %>% select(all_of(control_cols))
  cancer_feats <- cancer_data %>% select(all_of(cancer_cols))
  
  # Perform UMAP
  umap_control <- umap(control_feats, scale = TRUE)
  umap_cancer <- umap(cancer_feats, scale = TRUE)
  
  # Create data frames for plotting
  umap_control_df <- data.frame(umap_control, group = "control")
  umap_cancer_df <- data.frame(umap_cancer, group = "cancer")
  
  # Combine
  umap_combined <- rbind(umap_control_df, umap_cancer_df)
  
  # Plot and save
  p <- ggplot(umap_combined, aes(x = X1, y = X2, color = group)) +
    geom_point() +
    theme_classic(base_size = 15) +
    labs(title = paste("UMAP of", group_name, "features"),
         x = "UMAP 1", y = "UMAP 2")
  
  ggsave(filename = paste0("UMAP_", group_name, ".png"), plot = p, width = 10, height = 6)
}





TMD_data <- data_final %>%
  select(starts_with("MOTIF_END"))
library(uwot)

umap_result <- umap(TMD_data,scale = T)
umap_df <- data.frame(umap_result)
umap_df$group <- data_final$Run_GW  # assuming 'group' is a column in data_final


library(ggplot2)

ggplot(umap_df, aes(x = X1, y = X2, color = group)) +
  geom_point() +
  theme_classic(base_size = 15) +
  labs(title = "UMAP plot of flen features", x = "UMAP 1", y = "UMAP 2")





for (group_name in names(feature_groups)) {
  prefix <- feature_groups[[group_name]]
  
  # Get column names for control and cancer
  control_cols <- get_columns_starting_with(control_data, prefix)
  cancer_cols <- get_columns_starting_with(cancer_data, prefix)
  
  # Select feature columns
  control_feats <- control_data %>% select(all_of(control_cols))
  cancer_feats <- cancer_data %>% select(all_of(cancer_cols))
  
  # Perform UMAP for control
  umap_control <- umap(control_feats, scale = TRUE)
  umap_control_df <- data.frame(umap_control)
  umap_control_df$Run_GW <- control_data$Run_GW  # add grouping variable
  
  # Plot for control
  p_control <- ggplot(umap_control_df, aes(x = X1, y = X2, color = Run_GW)) +
    geom_point() +
    theme_classic(base_size = 15) +
    labs(title = paste("Control -", group_name),
         x = "UMAP 1", y = "UMAP 2")
  ggsave(paste0("C:/Users/User/Downloads/UMAP_Control_", group_name, ".png"), plot = p_control, width = 10, height = 6)
  
  # Perform UMAP for cancer
  umap_cancer <- umap(cancer_feats, scale = TRUE)
  umap_cancer_df <- data.frame(umap_cancer)
  umap_cancer_df$Run_GW <- cancer_data$Run_GW  # add grouping variable
  
  # Plot for cancer
  p_cancer <- ggplot(umap_cancer_df, aes(x = X1, y = X2, color = Run_GW)) +
    geom_point() +
    theme_classic(base_size = 15) +
    labs(title = paste("Cancer -", group_name),
         x = "UMAP 1", y = "UMAP 2")
  ggsave(paste0("C:/Users/User/Downloads/UMAP_Cancer_", group_name, ".png"), plot = p_cancer, width = 10, height = 6)
}

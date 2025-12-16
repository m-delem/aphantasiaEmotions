
source("helperfunctions.R")
source("soma_helpers.R")

scale_names_dfc <- c("tas", "ias", "iats", "pcs", "phq9", "gad7", "stai", "vviq")

df  <- read_excel("psychosomatic_controls_2024_10_22.xlsx", sheet = 1)
dfa <- read_excel("psychosomatic_2024_10_22.xlsx", sheet = 1)

df  <- clean_soma_dataframe(df)
dfa <- clean_soma_dataframe(dfa)

print(nrow(df)) # 520
print(nrow(dfa)) # 335

df  <- df[df$time_used >= 350 & df$time_used <= 5000, ]
dfa <- dfa[dfa$time_used >= 300 & dfa$time_used <= 5000, ]

print(nrow(df)) # 514
print(nrow(dfa)) # 327

# quest names ----
tas_names<-paste0("tas_",1:20) #TAS(Toronto Alexithymia Scale)
ias_names<-paste0("ias_",1:21) #IAS(Interoceptive Accuracy Scale)
iats_names<-paste0("iats_",1:21) #IATS(Interoceptive Attention Scale)
sass_names<-paste0("sass_",1:10) #SASS(Sensory Amplification Scale)
sss8_names<-paste0("sss_8_",1:8) #SSS-8(Somatic Symptom Scale-8)
pcs_names<-paste0("pcs_",1:13) #PCS(Pain Catastrophizing Scale)
phq9_names<-paste0("phq_9_",1:9) #PHQ-9(Patient Health Questionnaire)
gad7_names<-paste0("gad_7_",1:7) #GAD-7(Generalized Anxiety Disorder Scale)
stai_names<-paste0("stai_",1:20) #STAI(State-Trait Anxiety Inventory)
vviq_names<-c(paste0("vviq_q2_1_",1:4),
              paste0("vviq_q2_2_",1:4),
              paste0("vviq_q2_3_",1:4),
              paste0("vviq_q2_4_",1:4)) #VVIQ(Visual Vividness of Imagery Questionnaire)

all_questionnaire_items<-c(tas_names,ias_names,iats_names,sass_names,sss8_names,pcs_names,phq9_names,gad7_names,stai_names,vviq_names) #All questionnaire items
all_questionnaire_items_dfa<-c(tas_names,ias_names,iats_names,sss8_names,pcs_names,phq9_names,gad7_names,stai_names,vviq_names) #All questionnaire items

df <- all_items_to_numeric(df,all_questionnaire_items)
dfa <- all_items_to_numeric(dfa,all_questionnaire_items_dfa)

# 5 was coded as 13 for some reason in ias
df[, ias_names] <- lapply(df[, ias_names], function(column) ifelse(column == 13, 5, column))
dfa[, ias_names] <- lapply(dfa[, ias_names], function(column) ifelse(column == 13, 5, column))

# Define factors (number of items per scale)
factors <- c(length(tas_names),
             length(ias_names),
             length(iats_names),
             length(sass_names),
             length(sss8_names),
             length(pcs_names),
             length(phq9_names),
             length(gad7_names),
             length(stai_names),
             length(vviq_names))

# Prepare the data: Select questionnaire items
questionnaire_data <- df[, all_questionnaire_items, drop = FALSE]
#
evenodd_scores <- careless::evenodd(questionnaire_data, factors = factors)
df$evenodd_score <- evenodd_scores

df <- subset(df,df$evenodd_score < 0.0 & !is.na(df$evenodd_score))

# now  same for dfa
# Define factors (number of items per scale)
factors <- c(length(tas_names),
             length(ias_names),
             length(iats_names),
             length(sss8_names),
             length(pcs_names),
             length(phq9_names),
             length(gad7_names),
             length(stai_names),
             length(vviq_names))

# Prepare the data: Select questionnaire items
questionnaire_data <- dfa[, all_questionnaire_items_dfa, drop = FALSE]

# Compute Even-Odd Inconsistency Index
evenodd_scores <- careless::evenodd(questionnaire_data, factors = factors)

# Add even-odd scores as a new column in the dataset
dfa$evenodd_score <- evenodd_scores
dfa <- subset(dfa,dfa$evenodd_score < 0.0 & !is.na(dfa$evenodd_score))

nrow(df) # 512
nrow(dfa) # 325

# Reverse-code TAS items (reverse = 6 - value)
tas_reverse_items <- c("tas_4", "tas_5", "tas_10", "tas_11", "tas_15", "tas_16", "tas_19")
df[, tas_reverse_items] <- 6 - df[, tas_reverse_items]

# Reverse-code STAI items
stai_state_reverse_items <- c("stai_1", "stai_6", "stai_7", "stai_10", "stai_13", "stai_16", "stai_19")
df[, stai_state_reverse_items] <- 5 - df[, stai_state_reverse_items]

dfa[, tas_reverse_items] <- 6 - dfa[, tas_reverse_items]
dfa[, stai_state_reverse_items] <- 5 - dfa[, stai_state_reverse_items]

# reverse all vviq items for df and dfa
df[, vviq_names] <- 6 - df[, vviq_names]
dfa[, vviq_names] <- 6 - dfa[, vviq_names]

all_questionnaire_items <- list(
  TAS = tas_names,
  IAS = ias_names,
  IATS = iats_names,
  SASS = sass_names,
  SSS8 = sss8_names,
  PCS = pcs_names,
  PHQ9 = phq9_names,
  GAD7 = gad7_names,
  STAI = stai_names,
  VVIQ = vviq_names
)

scale_names_df <- c("tas", "ias", "iats", "sass", "sss8", "pcs", "phq9", "gad7", "stai", "vviq")

df <- compute_summary_scales(df, all_questionnaire_items, scale_names_df)

all_questionnaire_items_dfa <- list(
  TAS = tas_names,
  IAS = ias_names,
  IATS = iats_names,
  SSS8 = sss8_names,
  PCS = pcs_names,
  PHQ9 = phq9_names,
  GAD7 = gad7_names,
  STAI = stai_names,
  VVIQ = vviq_names
)

scale_names_dfa <- c("tas", "ias", "iats", "sss8", "pcs", "phq9", "gad7", "stai", "vviq")

dfa <- compute_summary_scales(dfa, all_questionnaire_items_dfa, scale_names_dfa)
generate_correlation_heatmap(df, scale_names_df, title = "Correlation Matrix for df")
generate_correlation_heatmap(dfa, scale_names_dfc, title = "Correlation Matrix for df")
dfa$cohort <- "aphan"
df$cohort <- "control"

nrow(df) # 512
nrow(dfa) # 325

df <- subset(df, df$ias != 0 & df$iats != 0 & df$vviq != 0 & df$stai != 0) # does nothing now after careless response removal
dfa <- subset(dfa, dfa$ias != 0 & dfa$iats != 0 & dfa$vviq != 0 & dfa$stai != 0) # does nothing now after careless response removal

nrow(df) # 512
nrow(dfa) # 321

dfc <- bind_rows(df, dfa)

generate_correlation_heatmap(dfc, scale_names_df, title = "Correlation Matrix for dfc")
nrow(dfc)

table(dfc$gender)
mean(dfc$age)
max(dfc$age)
min(dfc$age)
sd(dfc$age)

cor.test(dfc$age,dfc$vviq)

write.csv(dfc,"dfc.csv")

# Load required libraries
library(factoextra)
library(NbClust)
library(dplyr)

# Scale VVIQ for clustering
vviq_scaled <- scale(dfc$vviq, center = TRUE, scale = TRUE)

# Step 1: Determine Optimal Clusters Using Elbow Method
fviz_nbclust(data.frame(vviq_scaled), kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Clusters",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Sum of Squares")

# Step 2: Determine Optimal Clusters Using NbClust
set.seed(123)
nbclust_results <- NbClust(data = data.frame(vviq_scaled),
                           distance = "euclidean",
                           min.nc = 2,
                           max.nc = 10,
                           method = "kmeans")

# Get the optimal number of clusters
optimal_k <- nbclust_results$Best.nc[1]
cat("Optimal number of clusters determined by NbClust:", optimal_k, "\n")

which(nbclust_results$Best.nc[1, ] == 4)
optimal_k <- 4
# Step 3: Perform K-Means Clustering
set.seed(123)
kmeans_result <- kmeans(vviq_scaled, centers = optimal_k, nstart = 20)

# Assign cluster labels
dfc$vviq_k <- kmeans_result$cluster

# Step 4: Order Clusters by VVIQ Values
# Calculate mean VVIQ for each cluster
cluster_means <- aggregate(dfc$vviq, by = list(Cluster = dfc$vviq_k), mean)
cluster_means <- cluster_means[order(cluster_means$x), ]

# Create mapping from original cluster labels to ordered labels
cluster_mapping <- setNames(seq_along(cluster_means$Cluster), cluster_means$Cluster)

# Rename clusters in the dataset
dfc$vviq_k <- factor(dfc$vviq_k, levels = names(cluster_mapping), labels = cluster_mapping)

# Step 5: Automatically Calculate Cutoff Points
# Determine minimum and maximum VVIQ for each cluster
cluster_ranges <- aggregate(dfc$vviq,
                            by = list(Cluster = dfc$vviq_k),
                            FUN = function(x) c(Min = min(x, na.rm = TRUE), Max = max(x, na.rm = TRUE)))

# Reshape the result to a readable format
cluster_ranges <- do.call(data.frame, cluster_ranges)

# Step 6: Create Quantile-Based Clusters (Optional)
dfc$vviq_q <- cut(dfc$vviq,
                         breaks = quantile(dfc$vviq, probs = seq(0, 1, 0.25), na.rm = TRUE),
                         include.lowest = TRUE,
                         labels = c(1, 2, 3, 4))

q_ranges <- aggregate(dfc$vviq,
                            by = list(Cluster = dfc$vviq_q),
                            FUN = function(x) c(Min = min(x, na.rm = TRUE), Max = max(x, na.rm = TRUE)))

# Reshape the result to a readable format
q_ranges <- do.call(data.frame, q_ranges)

# subcluster within k1 and k4, aphantasia and hyperphana
range_16_32 <- dfc$vviq[dfc$vviq >= 16 & dfc$vviq <= 32]
range_65_80 <- dfc$vviq[dfc$vviq >= 65 & dfc$vviq <= 80]

# Scale the VVIQ values within each range
range_16_32_scaled <- scale(range_16_32, center = TRUE, scale = TRUE)
range_65_80_scaled <- scale(range_65_80, center = TRUE, scale = TRUE)

# Elbow method for range 16–32
# Optimal clusters for range 16–32
set.seed(123)
nbclust_16_32 <- NbClust(data = data.frame(range_16_32_scaled),
                         distance = "euclidean",
                         min.nc = 2,
                         max.nc = 10,
                         method = "kmeans")

# Get the optimal number of clusters for range 16–32
optimal_k_16_32 <- nbclust_16_32$Best.nc[1]
cat("Optimal number of clusters for range 16–32:", optimal_k_16_32, "\n")

# Optimal clusters for range 65–80
set.seed(123)
nbclust_65_80 <- NbClust(data = data.frame(range_65_80_scaled),
                         distance = "euclidean",
                         min.nc = 2,
                         max.nc = 10,
                         method = "kmeans")

# Get the optimal number of clusters for range 65–80
optimal_k_65_80 <- nbclust_65_80$Best.nc[1]
cat("Optimal number of clusters for range 65–80:", optimal_k_65_80, "\n")

# Export results
write.csv(dfc, "dfc_vviq_q_k.csv", row.names = FALSE)



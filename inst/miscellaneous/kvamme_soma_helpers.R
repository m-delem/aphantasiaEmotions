clean_soma_dataframe <- function(df) {
  # Remove the first row
  df <- df[-1, ]

  # Convert column names to lowercase
  colnames(df) <- tolower(colnames(df))

  # Replace "." and "-" in column names with "_"
  colnames(df) <- gsub("\\.|-", "_", colnames(df))

  df$time_used <- as.numeric(df$`duration (in seconds)`)

  df <- subset(df,df$finished == "1.0")

  df$age <- as.numeric(df$q1_8)
  df$gender <- as.integer(df$q1_7)

  return(df)
}

clean_time_used <- function(df) {
  # Step 1: Remove subjects with time_used > 5000 seconds

  # Step 2: Calculate the mean and standard deviation of time_used
  time_mean <- mean(df$time_used, na.rm = TRUE)
  time_sd <- sd(df$time_used, na.rm = TRUE)

  # Step 3: Identify the lower threshold (mean - 2 standard deviations)
  lower_threshold <- time_mean - 2 * time_sd

  # Step 4: Remove subjects with time_used below the lower threshold
  df_cleaned <- df[df$time_used >= lower_threshold, ]

  return(df_cleaned)
}

all_items_to_numeric <- function(df,all_questionnaire_items) {
  # Convert all columns except "eid" to numeric
  df[, all_questionnaire_items] <- lapply(df[, all_questionnaire_items], function(x) {
    if (is.factor(x) || is.character(x)) {
      as.numeric(as.character(x)) # Convert factors/characters to numeric
    } else {
      x
    }
  })

  return(df)
}

compute_evenodd <- function(df, all_questionnaire_items) {

  # Define factors for grouping items
  factors <- c(rep("TAS", sum(grepl("^tas_", all_questionnaire_items))),
               rep("IAS", sum(grepl("^ias_", all_questionnaire_items))),
               rep("IATS", sum(grepl("^iats_", all_questionnaire_items))),
               rep("SASS", sum(grepl("^sass_", all_questionnaire_items))),
               rep("SSS8", sum(grepl("^sss_8_", all_questionnaire_items))),
               rep("PCS", sum(grepl("^pcs_", all_questionnaire_items))),
               rep("PHQ9", sum(grepl("^phq_9_", all_questionnaire_items))),
               rep("GAD7", sum(grepl("^gad_7_", all_questionnaire_items))),
               rep("STAI", sum(grepl("^stai_", all_questionnaire_items))),
               rep("VVIQ", sum(grepl("^vviq_", all_questionnaire_items))))

  # Convert factors to a factor object
  factors <- factor(factors)

  # Compute even-odd consistency
  evenodd <- careless::evenodd(df[, all_questionnaire_items], factors = factors)

  return(evenodd)
}

compute_summary_scales <- function(df, all_questionnaire_items, scale_names) {
  # Loop through each scale
  for (scale in scale_names) {
    # Get the item names for the current scale
    scale_items <- all_questionnaire_items[[toupper(scale)]]

    # Check if items exist in the dataframe
    missing_items <- setdiff(scale_items, colnames(df))
    if (length(missing_items) > 0) {
      warning(paste("The following items are missing for scale", scale, ":",
                    paste(missing_items, collapse = ", ")))
      next
    }

    # Compute the summary scale (mean of non-NA values)
    scale_summary <- rowSums(df[, scale_items, drop = FALSE], na.rm = TRUE)

    # Add the summary scale to the dataframe
    df[[paste0(scale)]] <- scale_summary
  }

  return(df)
}

generate_correlation_heatmap <- function(data, scale_names, title = "Correlation Matrix") {
  # Ensure the corrplot package is installed
  if (!requireNamespace("corrplot", quietly = TRUE)) {
    install.packages("corrplot")
  }
  library(corrplot)

  # Select only the specified scale columns
  summary_data <- data[, scale_names, drop = FALSE]

  # Compute the correlation matrix
  correlation_matrix <- cor(summary_data, use = "pairwise.complete.obs")

  # Generate the heatmap
  corrplot::corrplot(correlation_matrix, method = "color", type = "upper",
                     tl.col = "black", tl.cex = 0.8, cl.cex = 0.8,
                     addCoef.col = "black", number.cex = 0.7,
                     title = title, mar = c(0, 0, 1, 0))
}

generate_correlation_dataframe <- function(data, scale_names, cohort_name) {
  # Select the relevant columns
  summary_data <- data[, scale_names, drop = FALSE]

  # Compute the correlation matrix
  correlation_matrix <- cor(summary_data, use = "pairwise.complete.obs")

  # Compute significance levels (p-values)
  n <- nrow(summary_data)
  p_matrix <- matrix(NA, ncol = length(scale_names), nrow = length(scale_names))
  colnames(p_matrix) <- scale_names
  rownames(p_matrix) <- scale_names
  for (i in 1:length(scale_names)) {
    for (j in i:length(scale_names)) {
      test <- cor.test(summary_data[, scale_names[i]], summary_data[, scale_names[j]])
      p_matrix[i, j] <- test$p.value
      p_matrix[j, i] <- test$p.value
    }
  }

  # Convert correlation matrix to long format
  correlation_df <- as.data.frame(as.table(correlation_matrix))
  colnames(correlation_df) <- c("Measure1", "Measure2", "Correlation")

  # Add p-values to the correlation dataframe
  p_df <- as.data.frame(as.table(p_matrix))
  colnames(p_df) <- c("Measure1", "Measure2", "P_value")

  # Merge correlations and p-values
  combined_df <- merge(correlation_df, p_df, by = c("Measure1", "Measure2"))
  combined_df$Cohort <- cohort_name

  # Remove self-correlations
  combined_df <- subset(combined_df, Measure1 != Measure2)

  # Add significance label
  combined_df$Significance <- with(combined_df, ifelse(
    P_value < 0.05 & Correlation > 0, "Significant Positive",
    ifelse(P_value < 0.05 & Correlation < 0, "Significant Negative", "Non-significant")
  ))

  return(combined_df)
}

perform_efa <- function(data, scale_names) {
  # Ensure psych package is installed
  if (!requireNamespace("psych", quietly = TRUE)) {
    install.packages("psych")
  }
  library(psych)

  # Select relevant columns
  efa_data <- data[, scale_names, drop = FALSE]

  # Check if there are missing values
  if (anyNA(efa_data)) {
    warning("Data contains missing values. Consider imputing missing data.")
  }

  # Perform Parallel Analysis to determine optimal number of factors
  parallel <- psych::fa.parallel(efa_data, fa = "fa", n.iter = 100, show.legend = TRUE)
  best_factors <- parallel$nfact  # Get suggested number of factors

  cat("\nOptimal number of factors based on parallel analysis: ", best_factors, "\n")

  # Perform EFA using the optimal number of factors
  efa_result <- psych::fa(efa_data, nfactors = best_factors, rotate = "varimax", fm = "ml")

  return(efa_result)
}

# Function to generate correlation annotations
generate_annotations <- function(data, x_var, y_var) {
  # Split data into groups
  aphantasiacs <- data %>% filter(Group == "Aphantasiacs (VVIQ = 16-32)")
  non_aphantasiacs <- data %>% filter(Group == "Non-Aphantasiacs (VVIQ = 33-80)")

  # Calculate correlations and p-values for each group
  corr_aph <- cor(aphantasiacs[[x_var]], aphantasiacs[[y_var]], use = "complete.obs", method = "pearson")
  p_aph <- cor.test(aphantasiacs[[x_var]], aphantasiacs[[y_var]], method = "pearson")$p.value
  p_aph_formatted <- ifelse(p_aph < 0.001, "p < 0.001", paste0("p = ", signif(p_aph, 2), " "))

  corr_non <- cor(non_aphantasiacs[[x_var]], non_aphantasiacs[[y_var]], use = "complete.obs", method = "pearson")
  p_non <- cor.test(non_aphantasiacs[[x_var]], non_aphantasiacs[[y_var]], method = "pearson")$p.value
  p_non_formatted <- ifelse(p_non < 0.001, "p < 0.001", paste0("p = ", signif(p_non, 2), " "))

  # Create annotations for the groups
  annotations <- data.frame(
    x = c(15, 45),  # X positions for annotations
    y = c(max(data[[y_var]], na.rm = TRUE) + 5, max(data[[y_var]], na.rm = TRUE) + 5),  # Y positions
    label = c(
      paste0("Aphantasiacs: R = ", format(round(corr_aph, 3), nsmall = 3), ", ", p_aph_formatted),
      paste0("Non-Aphantasiacs: R = ", format(round(corr_non, 3), nsmall = 3), ", ", p_non_formatted)
    )
  )

  return(annotations)
}

# Function to create scatterplots with formatted annotations
create_scatterplot <- function(data, x_var, y_var, title,y_annotate = NA) {
  # Generate annotations
  annotations <- generate_annotations(data, x_var, y_var)

  if (is.na(y_annotate)){
    y_annotate <- annotations$x[1]
  }

  ggplot(data, aes_string(x = x_var, y = y_var, color = "Group")) +
    geom_point(alpha = 0.7, size = 2) +
    geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 0.8) +
    annotate(
      "label", x = annotations$x[1], y = y_annotate, label = annotations$label[1],
      fill = "white", color = "blue", size = 5, hjust = 0
    ) +
    annotate(
      "label", x = annotations$x[2], y = y_annotate, label = annotations$label[2],
      fill = "white", color = "red", size = 5, hjust = 0
    ) +
    theme_minimal() +
    labs(
      title = title,
      x = toupper(x_var),
      y = toupper(y_var),
      color = "Group"
    ) +
    scale_color_manual(
      values = c(
        "Aphantasiacs (VVIQ = 16-32)" = "blue",
        "Non-Aphantasiacs (VVIQ = 33-80)" = "red"
      )
    ) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    )
}

calculate_correlation_aphantasia <- function(data, columns, group_var, threshold = 32) {
  results <- data.frame()

  # Split data into groups
  aphantasiacs <- data %>% filter(!!sym(group_var) <= threshold)
  non_aphantasiacs <- data %>% filter(!!sym(group_var) > threshold)

  # Function to format correlation results with stars for significance
  format_result <- function(corr, p) {
    if (is.na(corr)) return(NA)
    stars <- ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
    paste0(round(corr, 3), stars)
  }

  # Loop through all pairs of columns
  for (i in 1:(length(columns) - 1)) {
    for (j in (i + 1):length(columns)) {
      col1 <- columns[i]
      col2 <- columns[j]

      # Calculate correlation and p-value for aphantasiacs
      if (nrow(aphantasiacs) > 0) {
        corr_aph <- cor(aphantasiacs[[col1]], aphantasiacs[[col2]], use = "complete.obs", method = "pearson")
        p_aph <- cor.test(aphantasiacs[[col1]], aphantasiacs[[col2]], method = "pearson")$p.value
      } else {
        corr_aph <- NA
        p_aph <- NA
      }

      # Calculate correlation and p-value for non-aphantasiacs
      if (nrow(non_aphantasiacs) > 0) {
        corr_non <- cor(non_aphantasiacs[[col1]], non_aphantasiacs[[col2]], use = "complete.obs", method = "pearson")
        p_non <- cor.test(non_aphantasiacs[[col1]], non_aphantasiacs[[col2]], method = "pearson")$p.value
      } else {
        corr_non <- NA
        p_non <- NA
      }

      # Calculate correlation and p-value for the full dataset
      if (nrow(data) > 0) {
        corr_full <- cor(data[[col1]], data[[col2]], use = "complete.obs", method = "pearson")
        p_full <- cor.test(data[[col1]], data[[col2]], method = "pearson")$p.value
      } else {
        corr_full <- NA
        p_full <- NA
      }

      # Format correlation results
      aph_res <- format_result(corr_aph, p_aph)
      non_res <- format_result(corr_non, p_non)
      full_res <- format_result(corr_full, p_full)

      # Append results
      results <- rbind(results, data.frame(
        Measure1 = col1,
        Measure2 = col2,
        Aphantasiacs_Cor_Res = aph_res,
        Non_Aphantasiacs_Cor_Res = non_res,
        Full_Cor_Res = full_res,
        Correlation_Aphantasiacs = corr_aph,
        P_Value_Aphantasiacs = p_aph,
        Correlation_Non_Aphantasiacs = corr_non,
        P_Value_Non_Aphantasiacs = p_non,
        Correlation_Full = corr_full,
        P_Value_Full = p_full
      ))
    }
  }

  return(results)
}

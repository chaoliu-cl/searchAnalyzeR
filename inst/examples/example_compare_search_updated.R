# Enhanced PubMed Search Strategy Comparison using searchAnalyzeR Package
# Comparing Two Semantically Related Search Term Sets for COVID-19 Long-term Effects
#
# This example demonstrates comparing different search strategies and analyzing
# their relative performance using searchAnalyzeR functions.

# Load required packages
library(searchAnalyzeR)
library(ggplot2)
library(patchwork)  # For combining plots

# Set up the comparative analysis
cat("=== searchAnalyzeR: Search Strategy Comparison Example ===\n")
cat("Topic: Long-term effects of COVID-19 (Long COVID)\n")
cat("Objective: Compare two semantically related search strategies\n\n")

# Define two different search strategies for comparison
strategy_A <- list(
  name = "Clinical Terminology Strategy",
  terms = c(
    "post-covid syndrome",
    "covid-19 sequelae",
    "post-acute covid-19",
    "long haul covid",
    "covid long haulers"
  ),
  description = "Uses formal clinical terminology and established medical terms",
  databases = c("PubMed"),
  date_range = as.Date(c("2020-01-01", "2024-12-31")),
  filters = list(
    language = "English",
    article_types = c("Journal Article", "Review", "Clinical Trial")
  ),
  search_date = Sys.time()
)

strategy_B <- list(
  name = "Patient/Symptom-Focused Strategy",
  terms = c(
    "long covid",
    "persistent covid symptoms",
    "chronic covid symptoms",
    "post covid fatigue",
    "covid recovery complications"
  ),
  description = "Uses patient-centered language and symptom-based terminology",
  databases = c("PubMed"),
  date_range = as.Date(c("2020-01-01", "2024-12-31")),
  filters = list(
    language = "English",
    article_types = c("Journal Article", "Review", "Clinical Trial")
  ),
  search_date = Sys.time()
)

cat("Strategy A (Clinical Terminology):\n")
cat("Terms:", paste(strategy_A$terms, collapse = " OR "), "\n")
cat("Description:", strategy_A$description, "\n\n")

cat("Strategy B (Patient/Symptom-Focused):\n")
cat("Terms:", paste(strategy_B$terms, collapse = " OR "), "\n")
cat("Description:", strategy_B$description, "\n\n")

# Execute searches for both strategies
cat("=== EXECUTING STRATEGY A: Clinical Terminology ===\n")
results_A <- search_pubmed(
  search_terms = strategy_A$terms,
  max_results = 150,
  date_range = strategy_A$date_range
)

cat("\nStrategy A completed. Retrieved", nrow(results_A), "articles.\n\n")

cat("=== EXECUTING STRATEGY B: Patient/Symptom-Focused ===\n")
results_B <- search_pubmed(
  search_terms = strategy_B$terms,
  max_results = 150,
  date_range = strategy_B$date_range
)

cat("\nStrategy B completed. Retrieved", nrow(results_B), "articles.\n\n")

# Standardize both result sets
cat("Standardizing search results...\n")
standardized_A <- std_search_results(results_A, source_format = "pubmed")
standardized_B <- std_search_results(results_B, source_format = "pubmed")

# Add strategy identifiers
standardized_A$strategy <- "Clinical_Terminology"
standardized_B$strategy <- "Patient_Symptom_Focused"

# Detect duplicates within each strategy
dedup_A <- detect_dupes(standardized_A, method = "exact")
dedup_B <- detect_dupes(standardized_B, method = "exact")

cat("Strategy A - Total:", nrow(dedup_A), "Unique:", sum(!dedup_A$duplicate), "Duplicates:", sum(dedup_A$duplicate), "\n")
cat("Strategy B - Total:", nrow(dedup_B), "Unique:", sum(!dedup_B$duplicate), "Duplicates:", sum(dedup_B$duplicate), "\n\n")

# Create combined gold standard for comparison
# In practice, this would be your known relevant articles
# Here we'll create a more sophisticated gold standard based on high-confidence matches
cat("Creating enhanced gold standard...\n")

# High-confidence terms that indicate long COVID relevance
high_confidence_patterns <- c(
  "long covid", "post-covid", "post-acute covid", "persistent covid",
  "covid sequelae", "long haul", "chronic covid", "post covid syndrome"
)

# Filter out duplicates - using base R instead of dplyr
unique_A <- dedup_A[!dedup_A$duplicate, ]
unique_B <- dedup_B[!dedup_B$duplicate, ]

# Articles that appear in both strategies (high confidence)
overlap_ids <- intersect(unique_A$id, unique_B$id)

# Articles with multiple high-confidence patterns in title - using base R
# Function to count pattern matches in title
count_patterns <- function(titles, patterns) {
  sapply(titles, function(title) {
    sum(sapply(patterns, function(pattern) grepl(pattern, tolower(title))))
  })
}

pattern_counts_A <- count_patterns(unique_A$title, high_confidence_patterns)
pattern_counts_B <- count_patterns(unique_B$title, high_confidence_patterns)

multi_pattern_A <- unique_A$id[pattern_counts_A >= 2]
multi_pattern_B <- unique_B$id[pattern_counts_B >= 2]

# Combine for gold standard
gold_standard_ids <- unique(c(overlap_ids, multi_pattern_A, multi_pattern_B))

cat("Gold standard created with", length(gold_standard_ids), "high-confidence relevant articles\n")
cat("- Overlap between strategies:", length(overlap_ids), "articles\n")
cat("- Strategy A multi-pattern matches:", length(multi_pattern_A), "articles\n")
cat("- Strategy B multi-pattern matches:", length(multi_pattern_B), "articles\n\n")

# Initialize analyzers for both strategies
cat("Initializing SearchAnalyzers for comparison...\n")

# For non-duplicate entries
unique_A_ids <- unique_A$id
unique_B_ids <- unique_B$id

analyzer_A <- SearchAnalyzer$new(
  search_results = unique_A,
  gold_standard = gold_standard_ids,
  search_strategy = strategy_A
)

analyzer_B <- SearchAnalyzer$new(
  search_results = unique_B,
  gold_standard = gold_standard_ids,
  search_strategy = strategy_B
)

# Calculate comprehensive metrics for both strategies
cat("Calculating performance metrics...\n")
metrics_A <- analyzer_A$calculate_metrics()
metrics_B <- analyzer_B$calculate_metrics()

# Compare strategies using the comparison framework
cat("Performing statistical comparison...\n")
comparison_result <- compare_strategies(
  strategy1_results = unique_A_ids,
  strategy2_results = unique_B_ids,
  gold_standard = gold_standard_ids,
  test_type = "mcnemar"
)

# Additional performance metrics using the enhanced functions
cat("Calculating additional performance metrics...\n")

# Calculate strategy comparison metrics
strategy_comparison <- calc_strategy_comparison(
  strategy1_results = unique_A_ids,
  strategy2_results = unique_B_ids,
  gold_standard = gold_standard_ids
)

# Calculate temporal coverage for both strategies
temporal_A <- calc_temporal_coverage(unique_A, target_date_range = strategy_A$date_range)
temporal_B <- calc_temporal_coverage(unique_B, target_date_range = strategy_B$date_range)

# Display comprehensive comparison results
cat("\n=== COMPREHENSIVE STRATEGY COMPARISON RESULTS ===\n\n")

cat("STRATEGY A (Clinical Terminology) PERFORMANCE:\n")
cat("Total Articles Retrieved:", nrow(unique_A), "\n")
if (!is.null(metrics_A$precision_recall$precision)) {
  cat("Precision:", round(metrics_A$precision_recall$precision, 3), "\n")
  cat("Recall:", round(metrics_A$precision_recall$recall, 3), "\n")
  cat("F1 Score:", round(metrics_A$precision_recall$f1_score, 3), "\n")
  cat("Number Needed to Read:", round(metrics_A$precision_recall$number_needed_to_read, 1), "\n")
}

cat("\nSTRATEGY B (Patient/Symptom-Focused) PERFORMANCE:\n")
cat("Total Articles Retrieved:", nrow(unique_B), "\n")
if (!is.null(metrics_B$precision_recall$precision)) {
  cat("Precision:", round(metrics_B$precision_recall$precision, 3), "\n")
  cat("Recall:", round(metrics_B$precision_recall$recall, 3), "\n")
  cat("F1 Score:", round(metrics_B$precision_recall$f1_score, 3), "\n")
  cat("Number Needed to Read:", round(metrics_B$precision_recall$number_needed_to_read, 1), "\n")
}

cat("\nSTATISTICAL COMPARISON RESULTS:\n")
cat("Test Used:", comparison_result$test, "\n")
cat("P-value:", round(comparison_result$p_value, 4), "\n")
cat("Statistically Significant:", comparison_result$significant, "\n")

if (!is.null(comparison_result$difference)) {
  cat("\nPERFORMANCE DIFFERENCES (B - A):\n")
  cat("Precision Difference:", round(comparison_result$difference$precision_diff, 3), "\n")
  cat("Recall Difference:", round(comparison_result$difference$recall_diff, 3), "\n")
  cat("F1 Score Difference:", round(comparison_result$difference$f1_diff, 3), "\n")
}

# Display enhanced overlap analysis
cat("\nENHANCED OVERLAP ANALYSIS:\n")
cat("Total Unique Articles (Combined):", strategy_comparison$overlap_analysis$total_unique, "\n")
cat("Overlap Between Strategies:", strategy_comparison$overlap_analysis$overlap_count, "\n")
cat("Unique to Strategy A:", strategy_comparison$overlap_analysis$unique_to_strategy1, "\n")
cat("Unique to Strategy B:", strategy_comparison$overlap_analysis$unique_to_strategy2, "\n")
cat("Overlap Percentage:", round(strategy_comparison$overlap_analysis$overlap_percentage, 1), "%\n")

# Display complementarity analysis
cat("\nCOMPLEMENTARITY ANALYSIS:\n")
cat("Added Recall by Strategy A:", round(strategy_comparison$complementarity$added_recall_by_strategy1, 3), "\n")
cat("Added Recall by Strategy B:", round(strategy_comparison$complementarity$added_recall_by_strategy2, 3), "\n")
cat("Synergy Score:", round(strategy_comparison$complementarity$synergy_score, 3), "\n")

# Display temporal coverage results
cat("\nTEMPORAL COVERAGE ANALYSIS:\n")
cat("Strategy A - Target Period Coverage:", round(temporal_A$target_period_coverage * 100, 1), "%\n")
cat("Strategy B - Target Period Coverage:", round(temporal_B$target_period_coverage * 100, 1), "%\n")

if (length(temporal_A$peak_years) > 0) {
  cat("Strategy A - Peak Publication Years:", paste(temporal_A$peak_years, collapse = ", "), "\n")
}
if (length(temporal_B$peak_years) > 0) {
  cat("Strategy B - Peak Publication Years:", paste(temporal_B$peak_years, collapse = ", "), "\n")
}

# Create enhanced visualizations for comparison
cat("\nGenerating comparative visualizations...\n")

# 1. Side-by-side performance overview
overview_A <- analyzer_A$visualize_performance("overview") +
  ggtitle("Strategy A: Clinical Terminology") +
  theme(plot.title = element_text(size = 12))

overview_B <- analyzer_B$visualize_performance("overview") +
  ggtitle("Strategy B: Patient/Symptom-Focused") +
  theme(plot.title = element_text(size = 12))

combined_overview <- overview_A + overview_B +
  plot_annotation(title = "Search Strategy Performance Comparison",
                  subtitle = "Long COVID Search Strategies")

print(combined_overview)

# 2. Temporal comparison
temporal_A_plot <- analyzer_A$visualize_performance("temporal") +
  ggtitle("Strategy A: Temporal Distribution") +
  theme(plot.title = element_text(size = 12))

temporal_B_plot <- analyzer_B$visualize_performance("temporal") +
  ggtitle("Strategy B: Temporal Distribution") +
  theme(plot.title = element_text(size = 12))

combined_temporal <- temporal_A_plot + temporal_B_plot +
  plot_annotation(title = "Temporal Distribution Comparison")

print(combined_temporal)

# 3. Create a custom comparison summary plot - using base R instead of tidyr
comparison_data <- data.frame(
  Strategy = c("Clinical Terminology", "Patient/Symptom-Focused"),
  Precision = c(metrics_A$precision_recall$precision, metrics_B$precision_recall$precision),
  Recall = c(metrics_A$precision_recall$recall, metrics_B$precision_recall$recall),
  F1_Score = c(metrics_A$precision_recall$f1_score, metrics_B$precision_recall$f1_score),
  Articles_Retrieved = c(nrow(unique_A), nrow(unique_B)),
  stringsAsFactors = FALSE
)

# Reshape for plotting - using base R instead of tidyr::pivot_longer
precision_data <- data.frame(
  Strategy = comparison_data$Strategy,
  Metric = "Precision",
  Value = comparison_data$Precision,
  stringsAsFactors = FALSE
)

recall_data <- data.frame(
  Strategy = comparison_data$Strategy,
  Metric = "Recall",
  Value = comparison_data$Recall,
  stringsAsFactors = FALSE
)

f1_data <- data.frame(
  Strategy = comparison_data$Strategy,
  Metric = "F1_Score",
  Value = comparison_data$F1_Score,
  stringsAsFactors = FALSE
)

comparison_long <- rbind(precision_data, recall_data, f1_data)

comparison_plot <- ggplot(comparison_long, aes(x = Metric, y = Value, fill = Strategy)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = round(Value, 3)), position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_manual(values = c("Clinical Terminology" = "#2E86AB", "Patient/Symptom-Focused" = "#A23B72")) +
  labs(title = "Direct Performance Metric Comparison",
       subtitle = "Higher values indicate better performance",
       y = "Score", x = "Performance Metric") +
  theme_minimal() +
  ylim(0, 1)

print(comparison_plot)

# 4. Enhanced overlap analysis visualization
cat("\nCreating enhanced overlap analysis visualization...\n")

# Create data for enhanced overlap plot
overlap_data <- data.frame(
  Category = c("Strategy A Only", "Overlap", "Strategy B Only"),
  Count = c(strategy_comparison$overlap_analysis$unique_to_strategy1,
            strategy_comparison$overlap_analysis$overlap_count,
            strategy_comparison$overlap_analysis$unique_to_strategy2),
  Percentage = c(
    strategy_comparison$overlap_analysis$unique_to_strategy1 / strategy_comparison$overlap_analysis$total_unique * 100,
    strategy_comparison$overlap_analysis$overlap_count / strategy_comparison$overlap_analysis$total_unique * 100,
    strategy_comparison$overlap_analysis$unique_to_strategy2 / strategy_comparison$overlap_analysis$total_unique * 100
  ),
  stringsAsFactors = FALSE
)

overlap_plot <- ggplot(overlap_data, aes(x = Category, y = Count, fill = Category)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")), vjust = 0.5) +
  scale_fill_manual(values = c("Strategy A Only" = "#2E86AB",
                               "Overlap" = "#F18F01",
                               "Strategy B Only" = "#A23B72")) +
  labs(title = "Article Retrieval Overlap Analysis",
       subtitle = "Distribution of articles between search strategies",
       y = "Number of Articles", x = "Category") +
  theme_minimal() +
  theme(legend.position = "none")

print(overlap_plot)

# 5. Strategy comparison performance plot
performance_comparison_plot <- ggplot(strategy_comparison$performance_comparison,
                                      aes(x = strategy, y = f1_score, fill = strategy)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = round(f1_score, 3)), vjust = -0.5) +
  scale_fill_manual(values = c("Strategy 1" = "#2E86AB", "Strategy 2" = "#A23B72", "Combined" = "#F18F01")) +
  labs(title = "F1 Score Comparison Across Strategies",
       subtitle = "Higher F1 scores indicate better balanced performance",
       y = "F1 Score", x = "Strategy") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 1)

print(performance_comparison_plot)

# Analysis of individual term effectiveness using the enhanced term_effectiveness function
cat("\nAnalyzing individual term effectiveness...\n")

# Analyze terms from Strategy A
term_analysis_A <- term_effectiveness(
  terms = strategy_A$terms,
  search_results = unique_A,
  gold_standard = gold_standard_ids,
  text_fields = c("title", "abstract")
)

# Analyze terms from Strategy B
term_analysis_B <- term_effectiveness(
  terms = strategy_B$terms,
  search_results = unique_B,
  gold_standard = gold_standard_ids,
  text_fields = c("title", "abstract")
)

# Display term effectiveness results
cat("\nTerm Effectiveness for Strategy A (Clinical Terminology):\n")
print(term_analysis_A)

cat("\nTerm Effectiveness for Strategy B (Patient/Symptom-Focused):\n")
print(term_analysis_B)

# Add strategy information to the term analysis results
term_analysis_A$strategy <- "Clinical_Terminology"
term_analysis_B$strategy <- "Patient_Symptom_Focused"

# === ENHANCED INDIVIDUAL TERM EFFECTIVENESS PLOTS ===
cat("\n=== CREATING INDIVIDUAL TERM EFFECTIVENESS VISUALIZATIONS ===\n")

# 1. Strategy A - Individual Plots
cat("\nGenerating individual plots for Strategy A (Clinical Terminology)...\n")

# Precision plot for Strategy A
precision_plot_A <- plot_term_effectiveness(
  term_analysis_A,
  plot_type = "precision_only",
  title_override = "Strategy A: Term Precision Analysis",
  show_values = TRUE
)
print(precision_plot_A)

# Coverage plot for Strategy A
coverage_plot_A <- plot_term_effectiveness(
  term_analysis_A,
  plot_type = "coverage_only",
  title_override = "Strategy A: Term Coverage Analysis",
  show_values = TRUE
)
print(coverage_plot_A)

# Count plot for Strategy A
counts_plot_A <- plot_term_effectiveness(
  term_analysis_A,
  plot_type = "counts",
  title_override = "Strategy A: Article Retrieval Counts",
  show_values = TRUE
)
print(counts_plot_A)

# Precision vs Coverage bubble plot for Strategy A
bubble_plot_A <- plot_term_effectiveness(
  term_analysis_A,
  plot_type = "precision_coverage",
  title_override = "Strategy A: Term Effectiveness (Precision vs Coverage)",
  show_values = FALSE  # Labels would be too cluttered with values
)
print(bubble_plot_A)

# 2. Strategy B - Individual Plots
cat("\nGenerating individual plots for Strategy B (Patient/Symptom-Focused)...\n")

# Precision plot for Strategy B
precision_plot_B <- plot_term_effectiveness(
  term_analysis_B,
  plot_type = "precision_only",
  title_override = "Strategy B: Term Precision Analysis",
  show_values = TRUE
)
print(precision_plot_B)

# Coverage plot for Strategy B
coverage_plot_B <- plot_term_effectiveness(
  term_analysis_B,
  plot_type = "coverage_only",
  title_override = "Strategy B: Term Coverage Analysis",
  show_values = TRUE
)
print(coverage_plot_B)

# Count plot for Strategy B
counts_plot_B <- plot_term_effectiveness(
  term_analysis_B,
  plot_type = "counts",
  title_override = "Strategy B: Article Retrieval Counts",
  show_values = TRUE
)
print(counts_plot_B)

# Precision vs Coverage bubble plot for Strategy B
bubble_plot_B <- plot_term_effectiveness(
  term_analysis_B,
  plot_type = "precision_coverage",
  title_override = "Strategy B: Term Effectiveness (Precision vs Coverage)",
  show_values = FALSE
)
print(bubble_plot_B)

# 3. Side-by-side Comparison Plots (Optional Combined Views)
cat("\nCreating optional side-by-side comparison plots...\n")

if (requireNamespace("patchwork", quietly = TRUE)) {
  # Combined precision comparison
  precision_comparison <- precision_plot_A + precision_plot_B +
    plot_annotation(
      title = "Term Precision Comparison Across Strategies",
      subtitle = "Clinical Terminology vs Patient/Symptom-Focused approaches"
    )
  print(precision_comparison)

  # Combined coverage comparison
  coverage_comparison <- coverage_plot_A + coverage_plot_B +
    plot_annotation(
      title = "Term Coverage Comparison Across Strategies",
      subtitle = "Clinical Terminology vs Patient/Symptom-Focused approaches"
    )
  print(coverage_comparison)

  # Combined bubble plot comparison
  bubble_comparison <- bubble_plot_A + bubble_plot_B +
    plot_annotation(
      title = "Term Effectiveness Landscape Comparison",
      subtitle = "Precision vs Coverage analysis for both strategies"
    )
  print(bubble_comparison)
}

# 4. Highlight top-performing terms across strategies
cat("\nIdentifying and highlighting top-performing terms...\n")

# Find top terms by TES (Term Effectiveness Score) for Strategy A
if (nrow(term_analysis_A) > 0 && !all(is.na(term_analysis_A$precision))) {
  # Calculate TES and find top terms
  term_analysis_A <- calc_tes(term_analysis_A)
  top_results_A <- find_top_terms(term_analysis_A, n = 2, plot = FALSE)

  cat("Top performing terms in Strategy A:", paste(top_results_A$terms, collapse = ", "), "\n")
  cat("TES scores:", paste(round(top_results_A$data$tes, 3), collapse = ", "), "\n")

  # Create highlighted precision plot for Strategy A
  precision_highlight_A <- plot_term_effectiveness(
    term_analysis_A,
    plot_type = "precision_only",
    highlight_terms = top_results_A$terms,
    title_override = "Strategy A: Top-Performing Terms Highlighted (Precision)",
    show_values = TRUE
  )
  print(precision_highlight_A)
}

if (nrow(term_analysis_B) > 0 && !all(is.na(term_analysis_B$precision))) {
  # Calculate TES and find top terms
  term_analysis_B <- calc_tes(term_analysis_B)
  top_results_B <- find_top_terms(term_analysis_B, n = 2, plot = FALSE)

  cat("Top performing terms in Strategy B:", paste(top_results_B$terms, collapse = ", "), "\n")
  cat("TES scores:", paste(round(top_results_B$data$tes, 3), collapse = ", "), "\n")

  # Create highlighted precision plot for Strategy B
  precision_highlight_B <- plot_term_effectiveness(
    term_analysis_B,
    plot_type = "precision_only",
    highlight_terms = top_results_B$terms,
    title_override = "Strategy B: Top-Performing Terms Highlighted (Precision)",
    show_values = TRUE
  )
  print(precision_highlight_B)
}

# Cross-strategy term comparison
cat("\nPerforming cross-strategy term comparison...\n")
if (exists("top_results_A") && exists("top_results_B")) {
  term_comparison <- compare_terms(
    list(
      "Clinical" = term_analysis_A,
      "Patient" = term_analysis_B
    ),
    top_n = 3
  )

  cat("\nCross-Strategy Term Effectiveness Comparison:\n")
  print(term_comparison)

  # Create comparison visualization
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    # Get top terms from each strategy for visualization
    top_terms_combined <- unique(c(top_results_A$terms, top_results_B$terms))

    # Create TES comparison plot
    tes_comparison_data <- term_comparison[term_comparison$term %in% top_terms_combined, ]

    tes_plot <- ggplot(tes_comparison_data, aes(x = term, y = tes, fill = strategy)) +
      geom_col(position = "dodge", alpha = 0.8) +
      geom_text(aes(label = round(tes, 3)),
                position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
      scale_fill_manual(values = c("Clinical" = "#2E86AB", "Patient" = "#A23B72")) +
      labs(title = "Term Effectiveness Score (TES) Comparison",
           subtitle = "Top-performing terms across strategies",
           x = "Search Terms", y = "TES Score", fill = "Strategy") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(0, 1)

    print(tes_plot)
  }
}

# Generate comprehensive reports and exports
cat("\nExporting comprehensive analysis results...\n")
output_dir <- tempdir()

# Export individual strategy results
export_files_A <- export_results(
  search_results = unique_A,
  file_path = file.path(output_dir, "strategy_A_clinical"),
  formats = c("csv", "xlsx"),
  include_metadata = TRUE
)

export_files_B <- export_results(
  search_results = unique_B,
  file_path = file.path(output_dir, "strategy_B_patient"),
  formats = c("csv", "xlsx"),
  include_metadata = TRUE
)

# Export enhanced comparison results
enhanced_comparison_summary <- data.frame(
  Metric = c("Total_Articles_A", "Total_Articles_B", "Precision_A", "Precision_B",
             "Recall_A", "Recall_B", "F1_Score_A", "F1_Score_B", "Statistical_Significance",
             "Overlap_Count", "Unique_to_A", "Unique_to_B", "Synergy_Score",
             "Temporal_Coverage_A", "Temporal_Coverage_B"),
  Value = c(nrow(unique_A), nrow(unique_B),
            metrics_A$precision_recall$precision, metrics_B$precision_recall$precision,
            metrics_A$precision_recall$recall, metrics_B$precision_recall$recall,
            metrics_A$precision_recall$f1_score, metrics_B$precision_recall$f1_score,
            comparison_result$significant, strategy_comparison$overlap_analysis$overlap_count,
            strategy_comparison$overlap_analysis$unique_to_strategy1,
            strategy_comparison$overlap_analysis$unique_to_strategy2,
            strategy_comparison$complementarity$synergy_score,
            temporal_A$target_period_coverage, temporal_B$target_period_coverage),
  stringsAsFactors = FALSE
)

write.csv(enhanced_comparison_summary, file.path(output_dir, "enhanced_strategy_comparison_summary.csv"), row.names = FALSE)

# Export term effectiveness results with TES scores
write.csv(term_analysis_A, file.path(output_dir, "term_effectiveness_strategy_A.csv"), row.names = FALSE)
write.csv(term_analysis_B, file.path(output_dir, "term_effectiveness_strategy_B.csv"), row.names = FALSE)

# Export cross-strategy term comparison if available
if (exists("term_comparison")) {
  write.csv(term_comparison, file.path(output_dir, "cross_strategy_term_comparison.csv"), row.names = FALSE)
}

# Export strategy comparison results
write.csv(strategy_comparison$performance_comparison, file.path(output_dir, "strategy_performance_comparison.csv"), row.names = FALSE)
write.csv(strategy_comparison$overlap_analysis, file.path(output_dir, "strategy_overlap_analysis.csv"), row.names = FALSE)

# Export temporal coverage results
write.csv(temporal_A$coverage_by_year, file.path(output_dir, "temporal_coverage_strategy_A.csv"), row.names = FALSE)
write.csv(temporal_B$coverage_by_year, file.path(output_dir, "temporal_coverage_strategy_B.csv"), row.names = FALSE)

# Combined dataset with strategy labels - using base R instead of bind_rows and mutate
combined_A <- unique_A
combined_A$search_strategy <- "Clinical_Terminology"
combined_A$in_gold_standard <- combined_A$id %in% gold_standard_ids
combined_A$found_by_both <- combined_A$id %in% intersect(unique_A_ids, unique_B_ids)

combined_B <- unique_B
combined_B$search_strategy <- "Patient_Symptom_Focused"
combined_B$in_gold_standard <- combined_B$id %in% gold_standard_ids
combined_B$found_by_both <- combined_B$id %in% intersect(unique_A_ids, unique_B_ids)

# Combine and remove duplicates
combined_results <- rbind(combined_A, combined_B)
combined_results <- combined_results[!duplicated(combined_results$id), ]

write.csv(combined_results, file.path(output_dir, "combined_strategy_results.csv"), row.names = FALSE)

# Create comprehensive data package with enhanced results
enhanced_analysis_results <- list(
  metrics_A = metrics_A,
  metrics_B = metrics_B,
  comparison = comparison_result,
  strategy_comparison = strategy_comparison,
  temporal_A = temporal_A,
  temporal_B = temporal_B,
  term_effectiveness_A = term_analysis_A,
  term_effectiveness_B = term_analysis_B
)

package_dir <- create_data_package(
  search_results = combined_results,
  analysis_results = enhanced_analysis_results,
  output_dir = output_dir,
  package_name = "covid_search_strategy_comparison_enhanced"
)

# Final enhanced summary and recommendations
cat("\n=== ENHANCED ANALYSIS SUMMARY AND RECOMMENDATIONS ===\n\n")

# Determine the better strategy
better_strategy <- ifelse(metrics_A$precision_recall$f1_score > metrics_B$precision_recall$f1_score,
                          "Clinical Terminology (A)", "Patient/Symptom-Focused (B)")

cat("OVERALL PERFORMANCE WINNER:", better_strategy, "\n\n")

cat("ENHANCED KEY FINDINGS:\n")
cat("1. Strategy A (Clinical Terminology):\n")
cat("   - Uses formal medical terminology\n")
cat("   - Retrieved", nrow(unique_A), "unique articles\n")
cat("   - F1 Score:", round(metrics_A$precision_recall$f1_score, 3), "\n")
cat("   - Precision:", round(metrics_A$precision_recall$precision, 3), "\n")
cat("   - Recall:", round(metrics_A$precision_recall$recall, 3), "\n")
cat("   - Temporal Coverage:", round(temporal_A$target_period_coverage * 100, 1), "%\n\n")

cat("2. Strategy B (Patient/Symptom-Focused):\n")
cat("   - Uses patient-centered and symptom-based language\n")
cat("   - Retrieved", nrow(unique_B), "unique articles\n")
cat("   - F1 Score:", round(metrics_B$precision_recall$f1_score, 3), "\n")
cat("   - Precision:", round(metrics_B$precision_recall$precision, 3), "\n")
cat("   - Recall:", round(metrics_B$precision_recall$recall, 3), "\n")
cat("   - Temporal Coverage:", round(temporal_B$target_period_coverage * 100, 1), "%\n\n")

cat("3. Enhanced Complementarity Analysis:\n")
cat("   - Total unique articles when combined:", strategy_comparison$overlap_analysis$total_unique, "\n")
cat("   - Overlap between strategies:", strategy_comparison$overlap_analysis$overlap_count,
    "(", round(strategy_comparison$overlap_analysis$overlap_percentage, 1), "%)\n")
cat("   - Strategy A contributed", strategy_comparison$overlap_analysis$unique_to_strategy1, "unique articles\n")
cat("   - Strategy B contributed", strategy_comparison$overlap_analysis$unique_to_strategy2, "unique articles\n")
cat("   - Synergy Score:", round(strategy_comparison$complementarity$synergy_score, 3), "\n")
cat("   - Added Recall by Strategy A:", round(strategy_comparison$complementarity$added_recall_by_strategy1, 3), "\n")
cat("   - Added Recall by Strategy B:", round(strategy_comparison$complementarity$added_recall_by_strategy2, 3), "\n\n")

# Top performing terms
if (exists("top_results_A") && exists("top_results_B")) {
  cat("4. Most Effective Search Terms (by TES):\n")
  cat("   Strategy A top terms:", paste(top_results_A$terms, collapse = ", "), "\n")
  cat("   Strategy A TES scores:", paste(round(top_results_A$data$tes, 3), collapse = ", "), "\n")
  cat("   Strategy B top terms:", paste(top_results_B$terms, collapse = ", "), "\n")
  cat("   Strategy B TES scores:", paste(round(top_results_B$data$tes, 3), collapse = ", "), "\n\n")

  # Cross-strategy comparison insights
  if (exists("term_comparison")) {
    best_overall_term <- term_comparison[which.max(term_comparison$tes), ]
    cat("   Best overall term: '", best_overall_term$term, "' (", best_overall_term$strategy,
        " strategy, TES: ", round(best_overall_term$tes, 3), ")\n\n", sep = "")
  }
}

# Enhanced strategic recommendations
cat("ENHANCED STRATEGIC RECOMMENDATIONS:\n\n")

if (comparison_result$significant) {
  cat("✓ STATISTICAL SIGNIFICANCE: The difference between strategies IS statistically significant (p < 0.05)\n")
} else {
  cat("○ STATISTICAL SIGNIFICANCE: The difference between strategies is NOT statistically significant (p ≥ 0.05)\n")
}

if (strategy_comparison$overlap_analysis$overlap_percentage < 70) {
  cat("✓ COMPLEMENTARITY: Strategies show good complementarity - consider combining both approaches\n")
} else {
  cat("○ REDUNDANCY: High overlap suggests strategies may be redundant\n")
}

if (strategy_comparison$complementarity$synergy_score > 0.1) {
  cat("✓ SYNERGY: Combining strategies shows positive synergy effects\n")
} else {
  cat("○ LIMITED SYNERGY: Minimal benefit from combining strategies\n")
}

if (abs(metrics_A$precision_recall$recall - metrics_B$precision_recall$recall) > 0.1) {
  high_recall_strategy <- ifelse(metrics_A$precision_recall$recall > metrics_B$precision_recall$recall, "A", "B")
  cat("✓ RECALL DIFFERENCE: Strategy", high_recall_strategy, "shows substantially higher recall\n")
}

if (abs(metrics_A$precision_recall$precision - metrics_B$precision_recall$precision) > 0.1) {
  high_precision_strategy <- ifelse(metrics_A$precision_recall$precision > metrics_B$precision_recall$precision, "A", "B")
  cat("✓ PRECISION DIFFERENCE: Strategy", high_precision_strategy, "shows substantially higher precision\n")
}

# Temporal recommendations
if (abs(temporal_A$target_period_coverage - temporal_B$target_period_coverage) > 0.1) {
  high_temporal_strategy <- ifelse(temporal_A$target_period_coverage > temporal_B$target_period_coverage, "A", "B")
  cat("✓ TEMPORAL COVERAGE: Strategy", high_temporal_strategy, "shows better temporal coverage\n")
}

cat("\nOUTPUT FILES:\n")
cat("All enhanced analysis results have been exported to:", output_dir, "\n")
cat("- Individual strategy results (CSV, Excel)\n")
cat("- Enhanced comparison summary with synergy metrics\n")
cat("- Term effectiveness analysis with TES scores\n")
cat("- Cross-strategy term comparison matrix\n")
cat("- Strategy performance comparison matrix\n")
cat("- Temporal coverage analysis\n")
cat("- Overlap and complementarity analysis\n")
cat("- Complete enhanced data package for reproducibility\n")
cat("- Statistical comparison results\n")

cat("\n=== ENHANCED COMPARATIVE ANALYSIS COMPLETE ===\n")
cat("This enhanced analysis demonstrated:\n")
cat("1. Quantitative comparison of two semantically related search strategies\n")
cat("2. Statistical significance testing of performance differences\n")
cat("3. Enhanced overlap and complementarity analysis with synergy scoring\n")
cat("4. Individual search term effectiveness analysis with enhanced visualizations\n")
cat("5. Temporal coverage analysis across target date ranges\n")
cat("6. Evidence-based recommendations for strategy optimization\n")
cat("7. Comprehensive visualization and reporting with enhanced metrics\n")
cat("8. Performance comparison matrix analysis\n")
cat("9. Term Effectiveness Score (TES) analysis for individual keywords\n")
cat("10. Cross-strategy term performance comparison\n")

# Display sample articles from each strategy for qualitative review
cat("\n=== SAMPLE ARTICLES FOR QUALITATIVE REVIEW ===\n")

# Get unique articles that are in gold standard
gold_articles_A <- unique_A[unique_A$id %in% gold_standard_ids, ]
gold_articles_B <- unique_B[unique_B$id %in% gold_standard_ids, ]

# Sort by date (most recent first) - using base R
if ("date" %in% names(gold_articles_A)) {
  gold_articles_A <- gold_articles_A[order(gold_articles_A$date, decreasing = TRUE), ]
  gold_articles_B <- gold_articles_B[order(gold_articles_B$date, decreasing = TRUE), ]
}

# Take top 3 articles
sample_A <- head(gold_articles_A, 3)
sample_B <- head(gold_articles_B, 3)

cat("\nTop articles from Strategy A (Clinical Terminology):\n")
for (i in 1:nrow(sample_A)) {
  article <- sample_A[i, ]
  cat("\n", i, ". ", article$title, "\n", sep = "")
  cat("   Journal:", article$source, "\n")
  cat("   Date:", as.character(article$date), "\n")
  cat("   PMID:", gsub("PMID:", "", article$id), "\n")
}

cat("\nTop articles from Strategy B (Patient/Symptom-Focused):\n")
for (i in 1:nrow(sample_B)) {
  article <- sample_B[i, ]
  cat("\n", i, ". ", article$title, "\n", sep = "")
  cat("   Journal:", article$source, "\n")
  cat("   Date:", as.character(article$date), "\n")
  cat("   PMID:", gsub("PMID:", "", article$id), "\n")
}

# Clean up and provide final file locations
final_files <- list.files(output_dir, pattern = "covid|strategy|term_effectiveness|enhanced", full.names = TRUE, recursive = TRUE)
cat("\n=== FINAL OUTPUT LOCATIONS ===\n")
for (file in final_files) {
  cat(file, "\n")
}

cat("\n=== ENHANCED TERM EFFECTIVENESS VISUALIZATION SUMMARY ===\n")
cat("The following individual plots were generated for enhanced term effectiveness analysis:\n")
cat("1. Precision-only plots for each strategy (showing accuracy of each term)\n")
cat("2. Coverage-only plots for each strategy (showing completeness of each term)\n")
cat("3. Article count plots for each strategy (showing retrieval volume)\n")
cat("4. Precision vs Coverage bubble plots (showing balanced effectiveness)\n")
cat("5. Highlighted plots emphasizing top-performing terms\n")
cat("6. Optional side-by-side comparisons using patchwork\n")
cat("7. Enhanced performance comparison matrix visualization\n")
cat("8. Strategy overlap and complementarity analysis plots\n")
cat("9. Term Effectiveness Score (TES) comparison plots\n")
cat("10. Cross-strategy term performance visualizations\n\n")
cat("These enhanced individual plots provide comprehensive, focused analysis\n")
cat("with detailed synergy, complementarity, and TES metrics, making it easier to\n")
cat("identify the most effective search terms and optimal strategy combinations.\n")
cat("The TES metric provides a balanced view of term-level performance that\n")
cat("complements traditional strategy-level F1 scores.\n")

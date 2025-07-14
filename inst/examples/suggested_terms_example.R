# Enhanced PubMed Search Strategy Comparison using searchAnalyzeR Package
# Comparing Two Semantically Related Search Term Sets for COVID-19 Long-term Effects
#
# This example demonstrates comparing different search strategies and analyzing
# their relative performance using searchAnalyzeR functions.

# Load required packages
library(searchAnalyzeR)
library(rentrez)  # For PubMed API access
library(xml2)     # For XML parsing
library(dplyr)
library(ggplot2)
library(lubridate)
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

# Articles that appear in both strategies (high confidence)
overlap_ids <- intersect(dedup_A$id[!dedup_A$duplicate], dedup_B$id[!dedup_B$duplicate])

# Articles with multiple high-confidence patterns in title
multi_pattern_A <- dedup_A[!dedup_A$duplicate, ] %>%
  filter(rowSums(sapply(high_confidence_patterns, function(p) grepl(p, tolower(title)))) >= 2) %>%
  pull(id)

multi_pattern_B <- dedup_B[!dedup_B$duplicate, ] %>%
  filter(rowSums(sapply(high_confidence_patterns, function(p) grepl(p, tolower(title)))) >= 2) %>%
  pull(id)

# Combine for gold standard
gold_standard_ids <- unique(c(overlap_ids, multi_pattern_A, multi_pattern_B))

cat("Gold standard created with", length(gold_standard_ids), "high-confidence relevant articles\n")
cat("- Overlap between strategies:", length(overlap_ids), "articles\n")
cat("- Strategy A multi-pattern matches:", length(multi_pattern_A), "articles\n")
cat("- Strategy B multi-pattern matches:", length(multi_pattern_B), "articles\n\n")

# Initialize analyzers for both strategies
cat("Initializing SearchAnalyzers for comparison...\n")

analyzer_A <- SearchAnalyzer$new(
  search_results = filter(dedup_A, !duplicate),
  gold_standard = gold_standard_ids,
  search_strategy = strategy_A
)

analyzer_B <- SearchAnalyzer$new(
  search_results = filter(dedup_B, !duplicate),
  gold_standard = gold_standard_ids,
  search_strategy = strategy_B
)

# Calculate comprehensive metrics for both strategies
cat("Calculating performance metrics...\n")
metrics_A <- analyzer_A$calculate_metrics()
metrics_B <- analyzer_B$calculate_metrics()

# Compare strategies using the comparison framework
cat("Performing statistical comparison...\n")
unique_A_ids <- filter(dedup_A, !duplicate)$id
unique_B_ids <- filter(dedup_B, !duplicate)$id

comparison_result <- compare_strategies(
  strategy1_results = unique_A_ids,
  strategy2_results = unique_B_ids,
  gold_standard = gold_standard_ids,
  test_type = "mcnemar"
)

# Display comprehensive comparison results
cat("\n=== COMPREHENSIVE STRATEGY COMPARISON RESULTS ===\n\n")

cat("STRATEGY A (Clinical Terminology) PERFORMANCE:\n")
cat("Total Articles Retrieved:", nrow(filter(dedup_A, !duplicate)), "\n")
if (!is.null(metrics_A$precision_recall$precision)) {
  cat("Precision:", round(metrics_A$precision_recall$precision, 3), "\n")
  cat("Recall:", round(metrics_A$precision_recall$recall, 3), "\n")
  cat("F1 Score:", round(metrics_A$precision_recall$f1_score, 3), "\n")
  cat("Number Needed to Read:", round(metrics_A$precision_recall$number_needed_to_read, 1), "\n")
}

cat("\nSTRATEGY B (Patient/Symptom-Focused) PERFORMANCE:\n")
cat("Total Articles Retrieved:", nrow(filter(dedup_B, !duplicate)), "\n")
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

# Analyze overlap and unique contributions
cat("\nOVERLAP ANALYSIS:\n")
total_unique_combined <- length(union(unique_A_ids, unique_B_ids))
overlap_count <- length(intersect(unique_A_ids, unique_B_ids))
unique_to_A <- length(setdiff(unique_A_ids, unique_B_ids))
unique_to_B <- length(setdiff(unique_B_ids, unique_A_ids))

cat("Total Unique Articles (Combined):", total_unique_combined, "\n")
cat("Overlap Between Strategies:", overlap_count, "\n")
cat("Unique to Strategy A:", unique_to_A, "\n")
cat("Unique to Strategy B:", unique_to_B, "\n")
cat("Overlap Percentage:", round((overlap_count / total_unique_combined) * 100, 1), "%\n")

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
temporal_A <- analyzer_A$visualize_performance("temporal") +
  ggtitle("Strategy A: Temporal Distribution") +
  theme(plot.title = element_text(size = 12))

temporal_B <- analyzer_B$visualize_performance("temporal") +
  ggtitle("Strategy B: Temporal Distribution") +
  theme(plot.title = element_text(size = 12))

combined_temporal <- temporal_A + temporal_B +
  plot_annotation(title = "Temporal Distribution Comparison")

print(combined_temporal)

# 3. Create a custom comparison summary plot
comparison_data <- data.frame(
  Strategy = c("Clinical Terminology", "Patient/Symptom-Focused"),
  Precision = c(metrics_A$precision_recall$precision, metrics_B$precision_recall$precision),
  Recall = c(metrics_A$precision_recall$recall, metrics_B$precision_recall$recall),
  F1_Score = c(metrics_A$precision_recall$f1_score, metrics_B$precision_recall$f1_score),
  Articles_Retrieved = c(nrow(filter(dedup_A, !duplicate)), nrow(filter(dedup_B, !duplicate)))
)

# Reshape for plotting
library(tidyr)
comparison_long <- comparison_data %>%
  select(Strategy, Precision, Recall, F1_Score) %>%
  pivot_longer(cols = c(Precision, Recall, F1_Score), names_to = "Metric", values_to = "Value")

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

# 4. Venn diagram-style analysis
cat("\nCreating overlap analysis visualization...\n")

venn_data <- data.frame(
  Category = c("Strategy A Only", "Overlap", "Strategy B Only", "Gold Standard"),
  Count = c(unique_to_A, overlap_count, unique_to_B, length(gold_standard_ids)),
  Percentage = c(unique_to_A/total_unique_combined*100,
                 overlap_count/total_unique_combined*100,
                 unique_to_B/total_unique_combined*100,
                 length(gold_standard_ids)/total_unique_combined*100)
)

overlap_plot <- ggplot(venn_data[1:3,], aes(x = Category, y = Count, fill = Category)) +
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

# Generate comprehensive reports and exports
cat("\nExporting comprehensive analysis results...\n")
output_dir <- tempdir()

# Export individual strategy results
export_files_A <- export_results(
  search_results = filter(dedup_A, !duplicate),
  file_path = file.path(output_dir, "strategy_A_clinical"),
  formats = c("csv", "xlsx"),
  include_metadata = TRUE
)

export_files_B <- export_results(
  search_results = filter(dedup_B, !duplicate),
  file_path = file.path(output_dir, "strategy_B_patient"),
  formats = c("csv", "xlsx"),
  include_metadata = TRUE
)

# Export comparison results
comparison_summary <- data.frame(
  Metric = c("Total_Articles_A", "Total_Articles_B", "Precision_A", "Precision_B",
             "Recall_A", "Recall_B", "F1_Score_A", "F1_Score_B", "Statistical_Significance",
             "Overlap_Count", "Unique_to_A", "Unique_to_B"),
  Value = c(nrow(filter(dedup_A, !duplicate)), nrow(filter(dedup_B, !duplicate)),
            metrics_A$precision_recall$precision, metrics_B$precision_recall$precision,
            metrics_A$precision_recall$recall, metrics_B$precision_recall$recall,
            metrics_A$precision_recall$f1_score, metrics_B$precision_recall$f1_score,
            comparison_result$significant, overlap_count, unique_to_A, unique_to_B)
)

write.csv(comparison_summary, file.path(output_dir, "strategy_comparison_summary.csv"), row.names = FALSE)

# Combined dataset with strategy labels
combined_results <- rbind(
  filter(dedup_A, !duplicate) %>% mutate(search_strategy = "Clinical_Terminology"),
  filter(dedup_B, !duplicate) %>% mutate(search_strategy = "Patient_Symptom_Focused")
) %>%
  distinct(id, .keep_all = TRUE) %>%  # Remove any duplicates between strategies
  mutate(in_gold_standard = id %in% gold_standard_ids,
         found_by_both = id %in% intersect(unique_A_ids, unique_B_ids))

write.csv(combined_results, file.path(output_dir, "combined_strategy_results.csv"), row.names = FALSE)

# Create comprehensive data package
package_dir <- create_data_package(
  search_results = combined_results,
  analysis_results = list(
    metrics_A = metrics_A,
    metrics_B = metrics_B,
    comparison = comparison_result,
    overlap_analysis = venn_data
  ),
  output_dir = output_dir,
  package_name = "covid_search_strategy_comparison"
)

# Analysis of term effectiveness within each strategy
cat("\nAnalyzing individual term effectiveness...\n")

term_analysis_A <- data.frame(
  term = strategy_A$terms,
  strategy = "Clinical_Terminology"
) %>%
  rowwise() %>%
  mutate(
    articles_with_term = sum(grepl(tolower(term), tolower(paste(filter(dedup_A, !duplicate)$title,
                                                                filter(dedup_A, !duplicate)$abstract)))),
    relevant_with_term = sum(grepl(tolower(term), tolower(paste(filter(dedup_A, !duplicate & id %in% gold_standard_ids)$title,
                                                                filter(dedup_A, !duplicate & id %in% gold_standard_ids)$abstract))))
  ) %>%
  ungroup() %>%
  mutate(precision_by_term = ifelse(articles_with_term > 0, relevant_with_term / articles_with_term, 0))

term_analysis_B <- data.frame(
  term = strategy_B$terms,
  strategy = "Patient_Symptom_Focused"
) %>%
  rowwise() %>%
  mutate(
    articles_with_term = sum(grepl(tolower(term), tolower(paste(filter(dedup_B, !duplicate)$title,
                                                                filter(dedup_B, !duplicate)$abstract)))),
    relevant_with_term = sum(grepl(tolower(term), tolower(paste(filter(dedup_B, !duplicate & id %in% gold_standard_ids)$title,
                                                                filter(dedup_B, !duplicate & id %in% gold_standard_ids)$abstract))))
  ) %>%
  ungroup() %>%
  mutate(precision_by_term = ifelse(articles_with_term > 0, relevant_with_term / articles_with_term, 0))

term_analysis_combined <- rbind(term_analysis_A, term_analysis_B)

# Plot term effectiveness
term_plot <- ggplot(term_analysis_combined, aes(x = reorder(term, precision_by_term),
                                                y = precision_by_term, fill = strategy)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0(articles_with_term, " articles")), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("Clinical_Terminology" = "#2E86AB", "Patient_Symptom_Focused" = "#A23B72")) +
  labs(title = "Individual Search Term Effectiveness",
       subtitle = "Precision score and article count by search term",
       x = "Search Terms", y = "Precision Score", fill = "Strategy") +
  theme_minimal()

print(term_plot)

# Final summary and recommendations
cat("\n=== FINAL ANALYSIS SUMMARY AND RECOMMENDATIONS ===\n\n")

# Determine the better strategy
better_strategy <- ifelse(metrics_A$precision_recall$f1_score > metrics_B$precision_recall$f1_score,
                          "Clinical Terminology (A)", "Patient/Symptom-Focused (B)")

cat("OVERALL PERFORMANCE WINNER:", better_strategy, "\n\n")

cat("KEY FINDINGS:\n")
cat("1. Strategy A (Clinical Terminology):\n")
cat("   - Uses formal medical terminology\n")
cat("   - Retrieved", nrow(filter(dedup_A, !duplicate)), "unique articles\n")
cat("   - F1 Score:", round(metrics_A$precision_recall$f1_score, 3), "\n")
cat("   - Precision:", round(metrics_A$precision_recall$precision, 3), "\n")
cat("   - Recall:", round(metrics_A$precision_recall$recall, 3), "\n\n")

cat("2. Strategy B (Patient/Symptom-Focused):\n")
cat("   - Uses patient-centered and symptom-based language\n")
cat("   - Retrieved", nrow(filter(dedup_B, !duplicate)), "unique articles\n")
cat("   - F1 Score:", round(metrics_B$precision_recall$f1_score, 3), "\n")
cat("   - Precision:", round(metrics_B$precision_recall$precision, 3), "\n")
cat("   - Recall:", round(metrics_B$precision_recall$recall, 3), "\n\n")

cat("3. Complementarity Analysis:\n")
cat("   - Total unique articles when combined:", total_unique_combined, "\n")
cat("   - Overlap between strategies:", overlap_count, "(", round((overlap_count/total_unique_combined)*100, 1), "%)\n")
cat("   - Strategy A contributed", unique_to_A, "unique articles\n")
cat("   - Strategy B contributed", unique_to_B, "unique articles\n\n")

# Provide strategic recommendations
cat("STRATEGIC RECOMMENDATIONS:\n\n")

if (comparison_result$significant) {
  cat("✓ STATISTICAL SIGNIFICANCE: The difference between strategies IS statistically significant (p < 0.05)\n")
} else {
  cat("○ STATISTICAL SIGNIFICANCE: The difference between strategies is NOT statistically significant (p ≥ 0.05)\n")
}

if (overlap_count / total_unique_combined < 0.7) {
  cat("✓ COMPLEMENTARITY: Strategies show good complementarity - consider combining both approaches\n")
} else {
  cat("○ REDUNDANCY: High overlap suggests strategies may be redundant\n")
}

if (abs(metrics_A$precision_recall$recall - metrics_B$precision_recall$recall) > 0.1) {
  high_recall_strategy <- ifelse(metrics_A$precision_recall$recall > metrics_B$precision_recall$recall, "A", "B")
  cat("✓ RECALL DIFFERENCE: Strategy", high_recall_strategy, "shows substantially higher recall\n")
}

if (abs(metrics_A$precision_recall$precision - metrics_B$precision_recall$precision) > 0.1) {
  high_precision_strategy <- ifelse(metrics_A$precision_recall$precision > metrics_B$precision_recall$precision, "A", "B")
  cat("✓ PRECISION DIFFERENCE: Strategy", high_precision_strategy, "shows substantially higher precision\n")
}

cat("\nIMPLEMENTATION RECOMMENDATIONS:\n")

if (metrics_A$precision_recall$f1_score > metrics_B$precision_recall$f1_score) {
  if (metrics_A$precision_recall$f1_score - metrics_B$precision_recall$f1_score > 0.05) {
    cat("→ Primary recommendation: Use Clinical Terminology Strategy (A) as the main approach\n")
    cat("→ Secondary recommendation: Consider adding top-performing terms from Strategy B\n")
  } else {
    cat("→ Both strategies perform similarly - consider combining for maximum recall\n")
  }
} else {
  if (metrics_B$precision_recall$f1_score - metrics_A$precision_recall$f1_score > 0.05) {
    cat("→ Primary recommendation: Use Patient/Symptom-Focused Strategy (B) as the main approach\n")
    cat("→ Secondary recommendation: Consider adding top-performing terms from Strategy A\n")
  } else {
    cat("→ Both strategies perform similarly - consider combining for maximum recall\n")
  }
}

if (overlap_count / total_unique_combined < 0.6) {
  cat("→ Strong recommendation: Combine both strategies to maximize recall\n")
  cat("→ The low overlap suggests each strategy captures different relevant articles\n")
}

cat("\nOUTPUT FILES:\n")
cat("All analysis results have been exported to:", output_dir, "\n")
cat("- Individual strategy results (CSV, Excel)\n")
cat("- Combined comparison summary\n")
cat("- Complete data package for reproducibility\n")
cat("- Statistical comparison results\n")

cat("\n=== COMPARATIVE ANALYSIS COMPLETE ===\n")
cat("This analysis demonstrated:\n")
cat("1. Quantitative comparison of two semantically related search strategies\n")
cat("2. Statistical significance testing of performance differences\n")
cat("3. Overlap and complementarity analysis\n")
cat("4. Individual search term effectiveness analysis\n")
cat("5. Evidence-based recommendations for strategy optimization\n")
cat("6. Comprehensive visualization and reporting\n")

# Display sample articles from each strategy for qualitative review
cat("\n=== SAMPLE ARTICLES FOR QUALITATIVE REVIEW ===\n")

cat("\nTop articles from Strategy A (Clinical Terminology):\n")
sample_A <- filter(dedup_A, !duplicate, id %in% gold_standard_ids) %>%
  arrange(desc(date)) %>%
  head(3)

for (i in 1:nrow(sample_A)) {
  article <- sample_A[i, ]
  cat("\n", i, ". ", article$title, "\n", sep = "")
  cat("   Journal:", article$source, "\n")
  cat("   Date:", as.character(article$date), "\n")
  cat("   PMID:", gsub("PMID:", "", article$id), "\n")
}

cat("\nTop articles from Strategy B (Patient/Symptom-Focused):\n")
sample_B <- filter(dedup_B, !duplicate, id %in% gold_standard_ids) %>%
  arrange(desc(date)) %>%
  head(3)

for (i in 1:nrow(sample_B)) {
  article <- sample_B[i, ]
  cat("\n", i, ". ", article$title, "\n", sep = "")
  cat("   Journal:", article$source, "\n")
  cat("   Date:", as.character(article$date), "\n")
  cat("   PMID:", gsub("PMID:", "", article$id), "\n")
}

# Clean up and provide final file locations
final_files <- list.files(output_dir, pattern = "covid|strategy", full.names = TRUE, recursive = TRUE)
cat("\n=== FINAL OUTPUT LOCATIONS ===\n")
for (file in final_files) {
  cat(file, "\n")
}

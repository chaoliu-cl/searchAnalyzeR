# searchAnalyzeR Package Demonstration
# A comprehensive example showing key features of the package

# Load required libraries
library(searchAnalyzeR)
library(tidyverse)
library(lubridate)

# ==============================================================================
# 1. CREATE SAMPLE SEARCH STRATEGY AND RESULTS
# ==============================================================================

# Define search strategy
diabetes_strategy <- create_strategy(
  terms = c("diabetes", "type 2 diabetes", "insulin resistance", "glycemic control"),
  databases = c("PubMed", "Embase", "Cochrane"),
  date_range = as.Date(c("2020-01-01", "2023-12-31")),
  filters = list(
    language = "English",
    study_type = c("randomized controlled trial", "systematic review")
  )
)

print("Search Strategy Created:")
print(diabetes_strategy)

# Create realistic sample search results
set.seed(42)  # For reproducibility

# Generate 150 sample articles
search_results <- data.frame(
  id = paste0("PMID:", 30000000 + 1:150),
  title = c(
    # Highly relevant diabetes articles
    paste("Efficacy of metformin in type 2 diabetes:", sample(c("randomized trial", "systematic review", "meta-analysis"), 25, replace = TRUE)),
    paste("Insulin therapy for glycemic control:", sample(c("clinical study", "comparative trial", "longitudinal study"), 25, replace = TRUE)),
    paste("Dietary interventions in diabetes management:", sample(c("RCT", "cohort study", "intervention study"), 25, replace = TRUE)),
    # Moderately relevant
    paste("Cardiovascular outcomes in diabetes:", sample(c("observational study", "registry analysis", "follow-up study"), 25, replace = TRUE)),
    paste("Complications of diabetes:", sample(c("case series", "cross-sectional study", "review"), 25, replace = TRUE)),
    # Less relevant
    paste("General metabolic studies:", sample(c("basic research", "animal study", "case report"), 25, replace = TRUE))
  ),
  abstract = c(
    # Highly relevant abstracts
    rep("This randomized controlled trial evaluated the effectiveness of metformin in patients with type 2 diabetes mellitus. Primary outcomes included HbA1c reduction and glycemic control.", 25),
    rep("A systematic review examining insulin therapy protocols for optimal glycemic control in type 2 diabetes patients. Meta-analysis of 15 RCTs with 3,500 participants.", 25),
    rep("Dietary intervention study comparing low-carbohydrate vs. Mediterranean diets in diabetes management. Outcomes measured over 12 months follow-up period.", 25),
    # Moderately relevant
    rep("Observational study of cardiovascular outcomes in diabetes patients. Long-term follow-up of complications and risk factors in diabetes cohort.", 25),
    rep("Analysis of diabetic complications including neuropathy, retinopathy, and nephropathy. Cross-sectional study of prevalence and risk factors.", 25),
    # Less relevant
    rep("Basic metabolic research examining glucose metabolism pathways. Laboratory study using cell culture models to investigate insulin signaling.", 25)
  ),
  source = sample(c("Diabetes Care", "New England Journal of Medicine", "Lancet Diabetes & Endocrinology",
                    "Journal of Clinical Endocrinology", "Diabetologia", "American Journal of Medicine"), 150, replace = TRUE),
  date = as.Date("2020-01-01") + sample(0:1460, 150, replace = TRUE), # Random dates in range
  authors = paste("Author", sample(1:50, 150, replace = TRUE), "et al."),
  doi = paste0("10.1000/example.", 1:150),
  search_source = sample(c("PubMed", "Embase", "Cochrane"), 150, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
  stringsAsFactors = FALSE
)

# Add some duplicates to demonstrate deduplication
duplicate_indices <- sample(1:150, 15)
for (i in duplicate_indices) {
  # Create near-duplicate
  dup_row <- search_results[i, ]
  dup_row$id <- paste0("EMBASE:", 400000 + i)
  dup_row$title <- paste(dup_row$title, "- duplicate entry")
  dup_row$search_source <- "Embase"
  search_results <- rbind(search_results, dup_row)
}

# Define gold standard (known relevant articles) - first 50 are most relevant
gold_standard <- search_results$id[1:50]

cat("\nSample data created:")
cat("\nTotal search results:", nrow(search_results))
cat("\nGold standard size:", length(gold_standard))
cat("\nDate range:", paste(range(search_results$date), collapse = " to "))

# ==============================================================================
# 2. DATA PROCESSING AND STANDARDIZATION
# ==============================================================================

cat("\n\n=== DATA PROCESSING ===\n")

# Standardize search results
standardized_results <- std_search_results(search_results, source_format = "generic")

# Detect and remove duplicates
deduplicated_results <- detect_dupes(standardized_results, method = "exact")

cat("Duplicates detected:", sum(deduplicated_results$duplicate))
cat("\nUnique records:", sum(!deduplicated_results$duplicate))

# Calculate basic search statistics
search_stats <- calc_search_stats(deduplicated_results)
print(search_stats)

# ==============================================================================
# 3. SEARCH STRATEGY ANALYSIS
# ==============================================================================

cat("\n\n=== SEARCH STRATEGY ANALYSIS ===\n")

# Initialize the search analyzer
analyzer <- SearchAnalyzer$new(
  search_results = deduplicated_results[!deduplicated_results$duplicate, ],
  gold_standard = gold_standard,
  search_strategy = diabetes_strategy
)

# Calculate comprehensive performance metrics
metrics <- analyzer$calculate_metrics()

cat("Performance Metrics:")
cat("\nPrecision:", round(metrics$precision_recall$precision, 3))
cat("\nRecall:", round(metrics$precision_recall$recall, 3))
cat("\nF1 Score:", round(metrics$precision_recall$f1_score, 3))
cat("\nNumber Needed to Read:", round(metrics$precision_recall$number_needed_to_read, 1))

# ==============================================================================
# 4. VISUALIZATION GENERATION
# ==============================================================================

cat("\n\n=== GENERATING VISUALIZATIONS ===\n")

# Create performance overview plot
overview_plot <- analyzer$visualize_performance("overview")
print(overview_plot)

# Create temporal coverage plot
temporal_plot <- analyzer$visualize_performance("temporal")
print(temporal_plot)

# Create precision-recall curve
pr_plot <- analyzer$visualize_performance("precision_recall")
print(pr_plot)

# ==============================================================================
# 5. BENCHMARK VALIDATION
# ==============================================================================

cat("\n\n=== BENCHMARK VALIDATION ===\n")

# Initialize benchmark validator
validator <- BenchmarkValidator$new()

# Add custom benchmark (using our diabetes data as benchmark)
validator$add_benchmark(
  name = "diabetes_benchmark",
  corpus = deduplicated_results[!deduplicated_results$duplicate, ],
  relevant_ids = gold_standard
)

# Validate the search strategy
validation_results <- validator$validate_strategy(
  search_strategy = diabetes_strategy,
  benchmark_name = "diabetes_benchmark"
)

cat("Benchmark Validation Results:")
cat("\nPrecision:", round(validation_results$precision, 3))
cat("\nRecall:", round(validation_results$recall, 3))
cat("\nF1 Score:", round(validation_results$f1_score, 3))

# Cross-domain validation (if multiple benchmarks available)
if (length(validator$benchmarks) > 1) {
  cross_validation <- validator$cross_domain_validation(diabetes_strategy)
  print(cross_validation)
}

# ==============================================================================
# 6. SENSITIVITY ANALYSIS
# ==============================================================================

cat("\n\n=== SENSITIVITY ANALYSIS ===\n")

# Define parameter ranges for sensitivity analysis
parameter_ranges <- list(
  max_results = c(50, 100, 200),
  date_range_years = c(2, 3, 5)
)

# Run sensitivity analysis
sensitivity_results <- validator$sensitivity_analysis(
  base_strategy = diabetes_strategy,
  parameter_ranges = parameter_ranges
)

print("Sensitivity Analysis Results:")
print(sensitivity_results)

# ==============================================================================
# 7. STATISTICAL COMPARISON
# ==============================================================================

cat("\n\n=== STATISTICAL COMPARISON ===\n")

# Create a second strategy for comparison
alternative_strategy <- create_strategy(
  terms = c("diabetes mellitus", "hyperglycemia", "antidiabetic agents"),
  databases = c("PubMed", "Embase"),
  date_range = as.Date(c("2020-01-01", "2023-12-31"))
)

# Simulate results for alternative strategy (subset of original results)
alternative_results <- sample(search_results$id[1:120], 80)

# Compare strategies statistically
# Note: McNemar's test requires both strategies to have some overlapping and non-overlapping results
# Let's create more realistic strategy results for comparison

# Strategy 1: First 80 results (simulating a more restrictive search)
strategy1_results <- search_results$id[1:80]

# Strategy 2: Mix of first 60 + some from middle range (simulating different search terms)
strategy2_results <- c(search_results$id[1:60], search_results$id[90:120])

cat("Strategy comparison setup:")
cat("\nStrategy 1 results:", length(strategy1_results))
cat("\nStrategy 2 results:", length(strategy2_results))
cat("\nOverlap between strategies:", length(intersect(strategy1_results, strategy2_results)))
cat("\nGold standard articles found by Strategy 1:", length(intersect(strategy1_results, gold_standard)))
cat("\nGold standard articles found by Strategy 2:", length(intersect(strategy2_results, gold_standard)))

# Only proceed with comparison if we have valid data for McNemar's test
gold_found_by_1 <- intersect(strategy1_results, gold_standard)
gold_found_by_2 <- intersect(strategy2_results, gold_standard)
gold_found_by_both <- intersect(gold_found_by_1, gold_found_by_2)
gold_found_by_1_only <- setdiff(gold_found_by_1, gold_found_by_2)
gold_found_by_2_only <- setdiff(gold_found_by_2, gold_found_by_1)

cat("\nGold standard articles found by both strategies:", length(gold_found_by_both))
cat("\nGold standard articles found by Strategy 1 only:", length(gold_found_by_1_only))
cat("\nGold standard articles found by Strategy 2 only:", length(gold_found_by_2_only))

# Check if we have enough discordant pairs for McNemar's test
if (length(gold_found_by_1_only) > 0 || length(gold_found_by_2_only) > 0) {
  comparison_results <- compare_strategies(
    strategy1_results = strategy1_results,
    strategy2_results = strategy2_results,
    gold_standard = gold_standard,
    test_type = "mcnemar"
  )

  cat("\n\nStrategy Comparison Results:")
  cat("\nTest:", comparison_results$test)
  cat("\nP-value:", round(comparison_results$p_value, 4))
  cat("\nSignificant difference:", comparison_results$significant)

  # Show the contingency table
  cat("\nContingency Table:")
  print(comparison_results$contingency_table)

} else {
  cat("\n\nWarning: Strategies have identical performance on gold standard.")
  cat("\nUsing alternative statistical test (paired t-test with bootstrap)...")

  comparison_results <- compare_strategies(
    strategy1_results = strategy1_results,
    strategy2_results = strategy2_results,
    gold_standard = gold_standard,
    test_type = "paired_t"
  )

  cat("\nStrategy Comparison Results:")
  cat("\nTest:", comparison_results$test)
  cat("\nP-value:", round(comparison_results$p_value, 4))
  cat("\nSignificant difference:", comparison_results$significant)
}

cat("Strategy Comparison:")
cat("\nTest:", comparison_results$test)
cat("\nP-value:", round(comparison_results$p_value, 4))
cat("\nSignificant difference:", comparison_results$significant)

# ==============================================================================
# 8. PRISMA REPORTING
# ==============================================================================

cat("\n\n=== PRISMA REPORTING ===\n")

# Create PRISMA reporter
reporter <- PRISMAReporter$new()

# Create sample screening data for PRISMA flow
screening_data <- extract_screening(deduplicated_results[!deduplicated_results$duplicate, ])

# Add realistic screening decisions
screening_data$title_abstract_screened <- TRUE
screening_data$excluded_title_abstract <- sample(c(TRUE, FALSE), nrow(screening_data),
                                                 replace = TRUE, prob = c(0.7, 0.3))
screening_data$full_text_eligible <- !screening_data$excluded_title_abstract &
  sample(c(TRUE, FALSE), nrow(screening_data),
         replace = TRUE, prob = c(0.4, 0.6))
screening_data$included <- screening_data$full_text_eligible &
  sample(c(TRUE, FALSE), nrow(screening_data),
         replace = TRUE, prob = c(0.3, 0.7))

# Generate PRISMA flow diagram
flow_data <- list(
  identified = nrow(deduplicated_results),
  after_duplicates = sum(!deduplicated_results$duplicate),
  title_abstract_screened = sum(screening_data$title_abstract_screened),
  full_text_eligible = sum(screening_data$full_text_eligible),
  included = sum(screening_data$included),
  excluded_title_abstract = sum(screening_data$excluded_title_abstract),
  excluded_full_text = sum(screening_data$full_text_eligible & !screening_data$included)
)

prisma_plot <- reporter$generate_prisma_diagram(screening_data)
print(prisma_plot)

# Generate search strategy documentation
strategy_docs <- reporter$document_search_strategy(diabetes_strategy)
print("Search Strategy Documentation:")
print(strategy_docs)

# ==============================================================================
# 9. REPRODUCIBILITY MANAGEMENT
# ==============================================================================

cat("\n\n=== REPRODUCIBILITY MANAGEMENT ===\n")

# Initialize reproducibility manager
repro_manager <- ReproducibilityManager$new()

# Create analysis configuration
analysis_config <- list(
  gold_standard = gold_standard,
  method = "precision_recall",
  parameters = list(
    threshold = 0.8,
    deduplication_method = "exact"
  )
)

# Create reproducible package
package_path <- repro_manager$create_repro_package(
  search_strategy = diabetes_strategy,
  results = deduplicated_results[!deduplicated_results$duplicate, ],
  analysis_config = analysis_config
)

cat("Reproducible package created at:", package_path)

# Generate audit trail
audit_trail <- repro_manager$gen_audit_trail(analyzer)
cat("\nAudit trail generated with", length(audit_trail), "components")

# ==============================================================================
# 10. EXPORT AND REPORTING
# ==============================================================================

cat("\n\n=== EXPORT AND REPORTING ===\n")

# Export search results in multiple formats
output_files <- export_results(
  search_results = deduplicated_results[!deduplicated_results$duplicate, ],
  file_path = file.path(tempdir(), "diabetes_search_results"),
  formats = c("csv", "xlsx", "ris", "bibtex"),
  include_metadata = TRUE
)

cat("Search results exported to:")
for (file in output_files) {
  cat("\n -", file)
}

# Export analysis metrics
metrics_file <- export_metrics(
  metrics = metrics,
  file_path = file.path(tempdir(), "analysis_metrics.xlsx"),
  format = "xlsx"
)

cat("\nAnalysis metrics exported to:", metrics_file)

# Create comprehensive data package
data_package_dir <- create_data_package(
  search_results = deduplicated_results[!deduplicated_results$duplicate, ],
  analysis_results = list(
    metrics = metrics,
    plots = list(
      overview = overview_plot,
      temporal = temporal_plot,
      precision_recall = pr_plot
    ),
    validation = validation_results
  ),
  output_dir = tempdir(),
  package_name = "diabetes_search_analysis"
)

cat("\nComprehensive data package created at:", data_package_dir)

# ==============================================================================
# 11. COMPLETE WORKFLOW EXAMPLE
# ==============================================================================

cat("\n\n=== COMPLETE WORKFLOW EXAMPLE ===\n")

# Demonstrate the complete workflow function
workflow_results <- complete_search_workflow(
  search_terms = "type 2 diabetes AND (metformin OR insulin)",
  databases = "pubmed",
  gold_standard = gold_standard[1:20],  # Subset for demo
  max_results = 50,
  date_range = c("2022-01-01", "2023-12-31"),
  output_dir = tempdir()
)

cat("Complete workflow summary:")
cat("\nTotal articles found:", workflow_results$summary$total_found)
cat("\nAfter deduplication:", workflow_results$summary$after_deduplication)
cat("\nPrecision:", round(workflow_results$summary$precision, 3))
cat("\nRecall:", round(workflow_results$summary$recall, 3))
cat("\nF1 Score:", round(workflow_results$summary$f1_score, 3))

# ==============================================================================
# 12. SUMMARY AND RECOMMENDATIONS
# ==============================================================================

cat("\n\n=== ANALYSIS SUMMARY ===\n")

cat("Search Strategy Performance Summary:")
cat("\n• Total retrieved articles:", nrow(deduplicated_results[!deduplicated_results$duplicate, ]))
cat("\n• Precision (% relevant):", paste0(round(metrics$precision_recall$precision * 100, 1), "%"))
cat("\n• Recall (% found):", paste0(round(metrics$precision_recall$recall * 100, 1), "%"))
cat("\n• F1 Score:", round(metrics$precision_recall$f1_score, 3))
cat("\n• Number needed to read:", round(metrics$precision_recall$number_needed_to_read, 1))

cat("\n\nData Quality:")
cat("\n• Duplicates detected:", sum(deduplicated_results$duplicate))
cat("\n• Missing abstracts:", sum(is.na(deduplicated_results$abstract)))
cat("\n• Date coverage:", paste(range(deduplicated_results$date, na.rm = TRUE), collapse = " to "))

cat("\n\nRecommendations:")
if (metrics$precision_recall$precision < 0.5) {
  cat("\n• Consider refining search terms to improve precision")
}
if (metrics$precision_recall$recall < 0.8) {
  cat("\n• Consider expanding search terms or adding databases to improve recall")
}
if (sum(deduplicated_results$duplicate) > nrow(deduplicated_results) * 0.1) {
  cat("\n• High duplicate rate suggests overlapping database coverage")
}

cat("\n\nFiles created during this analysis:")
cat("\n• Search results: Multiple formats (CSV, Excel, RIS, BibTeX)")
cat("\n• Analysis metrics: Excel workbook with detailed breakdowns")
cat("\n• Visualizations: Performance plots and PRISMA diagram")
cat("\n• Reproducibility package: Complete analysis reproduction materials")
cat("\n• Data package: Comprehensive dataset with documentation")

cat("\n\n=== DEMO COMPLETED SUCCESSFULLY ===")
cat("\nThe searchAnalyzeR package provides comprehensive tools for:")
cat("\n1. Search strategy validation and optimization")
cat("\n2. Performance metric calculation and visualization")
cat("\n3. Statistical comparison between strategies")
cat("\n4. PRISMA-compliant reporting")
cat("\n5. Reproducibility and audit trail management")
cat("\n6. Multi-format data export and sharing")
cat("\n\nFor more information, see package documentation and vignettes.")

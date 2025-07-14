# Real PubMed Search Example using searchAnalyzeR Package
# Systematic Review Search Strategy for COVID-19 Long-term Effects
#
# This example demonstrates searching PubMed for real articles and analyzing
# the search strategy performance using searchAnalyzeR functions.

# Load required packages
library(searchAnalyzeR)
library(rentrez)  # For PubMed API access
library(xml2)     # For XML parsing
library(dplyr)
library(ggplot2)
library(lubridate)

# Set up the analysis
cat("=== searchAnalyzeR: Real PubMed Search Example ===\n")
cat("Topic: Long-term effects of COVID-19 (Long COVID)\n")
cat("Objective: Demonstrate search strategy analysis with real data\n\n")

# Define our search strategy
search_strategy <- list(
  terms = c(
    "long covid",
    "post-covid syndrome",
    "covid-19 sequelae",
    "post-acute covid-19",
    "persistent covid symptoms"
  ),
  databases = c("PubMed"),
  date_range = as.Date(c("2020-01-01", "2024-12-31")),
  filters = list(
    language = "English",
    article_types = c("Journal Article", "Review", "Clinical Trial")
  ),
  search_date = Sys.time()
)

cat("Search Strategy:\n")
cat("Terms:", paste(search_strategy$terms, collapse = " OR "), "\n")
cat("Date range:", paste(search_strategy$date_range, collapse = " to "), "\n\n")

# Function to search PubMed and retrieve real articles
search_pubmed_real <- function(search_terms, max_results = 200, date_range = NULL) {

  # Build PubMed query
  query_terms <- paste(paste0('"', search_terms, '"[Title/Abstract]'), collapse = " OR ")

  if (!is.null(date_range)) {
    date_filter <- paste0('("', format(date_range[1], "%Y/%m/%d"), '"[Date - Publication] : "',
                          format(date_range[2], "%Y/%m/%d"), '"[Date - Publication])')
    full_query <- paste("(", query_terms, ") AND", date_filter, "AND English[Language]")
  } else {
    full_query <- paste("(", query_terms, ") AND English[Language]")
  }

  cat("PubMed Query:", full_query, "\n")

  # Search PubMed
  search_results <- entrez_search(
    db = "pubmed",
    term = full_query,
    retmax = max_results,
    use_history = TRUE
  )

  cat("Found", length(search_results$ids), "articles\n")

  if (length(search_results$ids) == 0) {
    return(data.frame())
  }

  # Retrieve detailed information in batches
  batch_size <- 50
  all_articles <- list()

  for (i in seq(1, length(search_results$ids), batch_size)) {
    end_idx <- min(i + batch_size - 1, length(search_results$ids))
    batch_ids <- search_results$ids[i:end_idx]

    cat("Retrieving batch", ceiling(i/batch_size), "of", ceiling(length(search_results$ids)/batch_size), "\n")

    # Get abstracts and metadata
    abstracts <- entrez_fetch(
      db = "pubmed",
      id = batch_ids,
      rettype = "abstract",
      retmode = "xml"
    )

    # Parse XML
    xml_doc <- read_xml(abstracts)
    articles <- xml_find_all(xml_doc, ".//PubmedArticle")

    batch_data <- lapply(articles, function(article) {
      # Extract PMID
      pmid_node <- xml_find_first(article, ".//PMID")
      pmid <- if (!is.na(pmid_node)) xml_text(pmid_node) else NA

      # Extract title
      title_node <- xml_find_first(article, ".//ArticleTitle")
      title <- if (!is.na(title_node)) xml_text(title_node) else "No title available"

      # Extract abstract
      abstract_nodes <- xml_find_all(article, ".//AbstractText")
      abstract <- if (length(abstract_nodes) > 0) {
        paste(xml_text(abstract_nodes), collapse = " ")
      } else {
        "No abstract available"
      }

      # Extract journal
      journal_node <- xml_find_first(article, ".//Journal/Title")
      journal <- if (!is.na(journal_node)) xml_text(journal_node) else "Unknown journal"

      # Extract publication date
      year_node <- xml_find_first(article, ".//PubDate/Year")
      month_node <- xml_find_first(article, ".//PubDate/Month")
      day_node <- xml_find_first(article, ".//PubDate/Day")

      year <- if (!is.na(year_node)) xml_text(year_node) else format(Sys.Date(), "%Y")
      month <- if (!is.na(month_node)) xml_text(month_node) else "01"
      day <- if (!is.na(day_node)) xml_text(day_node) else "01"

      # Convert month name to number if necessary
      if (nchar(month) > 2) {
        month <- match(month, month.name)
        if (is.na(month)) month <- "01"
      }

      # Create date
      pub_date <- tryCatch({
        as.Date(paste(year, sprintf("%02d", as.numeric(month)), sprintf("%02d", as.numeric(day)), sep = "-"))
      }, error = function(e) {
        as.Date(paste(year, "01", "01", sep = "-"))
      })

      # Extract authors
      author_nodes <- xml_find_all(article, ".//Author/LastName")
      authors <- if (length(author_nodes) > 0) {
        paste(xml_text(author_nodes), collapse = ", ")
      } else {
        "No authors listed"
      }

      # Extract DOI
      doi_node <- xml_find_first(article, ".//ELocationID[@EIdType='doi']")
      doi <- if (!is.na(doi_node)) xml_text(doi_node) else NA

      data.frame(
        id = paste0("PMID:", pmid),
        title = title,
        abstract = abstract,
        source = journal,
        date = pub_date,
        authors = authors,
        doi = doi,
        pmid = pmid,
        search_source = "PubMed_API",
        stringsAsFactors = FALSE
      )
    })

    all_articles <- c(all_articles, batch_data)

    # Be respectful to NCBI servers
    Sys.sleep(0.5)
  }

  # Combine all results
  do.call(rbind, all_articles)
}

# Execute the search
cat("Searching PubMed for real articles...\n")
raw_results <- search_pubmed_real(
  search_terms = search_strategy$terms,
  max_results = 150,
  date_range = search_strategy$date_range
)

cat("\nRaw search completed. Retrieved", nrow(raw_results), "articles.\n")

# Standardize the results using searchAnalyzeR functions
cat("\nStandardizing search results...\n")
standardized_results <- std_search_results(raw_results, source_format = "pubmed")

# Detect and remove duplicates
cat("Detecting duplicates...\n")
dedup_results <- detect_dupes(standardized_results, method = "exact")

cat("Duplicate detection complete:\n")
cat("- Total articles:", nrow(dedup_results), "\n")
cat("- Unique articles:", sum(!dedup_results$duplicate), "\n")
cat("- Duplicates found:", sum(dedup_results$duplicate), "\n\n")

# Calculate search statistics
search_stats <- calc_search_stats(dedup_results)
cat("Search Statistics:\n")
cat("- Date range:", paste(search_stats$date_range, collapse = " to "), "\n")
cat("- Missing abstracts:", search_stats$missing_abstracts, "\n")
cat("- Missing dates:", search_stats$missing_dates, "\n\n")

# Create a gold standard for demonstration
# In a real systematic review, this would be your known relevant articles
# For this example, we'll identify articles that contain key terms in titles
cat("Creating demonstration gold standard...\n")
long_covid_terms <- c("long covid", "post-covid", "post-acute covid", "persistent covid", "covid sequelae")
pattern <- paste(long_covid_terms, collapse = "|")

gold_standard_ids <- dedup_results %>%
  filter(!duplicate) %>%
  filter(grepl(pattern, tolower(title))) %>%
  pull(id)

cat("Gold standard created with", length(gold_standard_ids), "highly relevant articles\n\n")

# Initialize SearchAnalyzer with real data
cat("Initializing SearchAnalyzer...\n")
analyzer <- SearchAnalyzer$new(
  search_results = filter(dedup_results, !duplicate),
  gold_standard = gold_standard_ids,
  search_strategy = search_strategy
)

# Calculate comprehensive metrics
cat("Calculating performance metrics...\n")
metrics <- analyzer$calculate_metrics()

# Display key metrics
cat("\n=== SEARCH PERFORMANCE METRICS ===\n")
if (!is.null(metrics$precision_recall$precision)) {
  cat("Precision:", round(metrics$precision_recall$precision, 3), "\n")
  cat("Recall:", round(metrics$precision_recall$recall, 3), "\n")
  cat("F1 Score:", round(metrics$precision_recall$f1_score, 3), "\n")
  cat("Number Needed to Read:", round(metrics$precision_recall$number_needed_to_read, 1), "\n")
}

cat("\n=== BASIC METRICS ===\n")
cat("Total Records:", metrics$basic$total_records, "\n")
cat("Unique Records:", metrics$basic$unique_records, "\n")
cat("Duplicates:", metrics$basic$duplicates, "\n")
cat("Sources:", metrics$basic$sources, "\n")

# Generate visualizations
cat("\nGenerating visualizations...\n")

# Overview plot
overview_plot <- analyzer$visualize_performance("overview")
print(overview_plot)

# Temporal distribution plot
temporal_plot <- analyzer$visualize_performance("temporal")
print(temporal_plot)

# Precision-recall curve (if gold standard available)
if (length(gold_standard_ids) > 0) {
  pr_plot <- analyzer$visualize_performance("precision_recall")
  print(pr_plot)
}

# Generate PRISMA flow diagram data
cat("\nCreating PRISMA flow data...\n")
screening_data <- data.frame(
  id = dedup_results$id[!dedup_results$duplicate],
  identified = TRUE,
  duplicate = FALSE,
  title_abstract_screened = TRUE,
  full_text_eligible = runif(sum(!dedup_results$duplicate)) > 0.7,  # Simulate screening
  included = runif(sum(!dedup_results$duplicate)) > 0.85,  # Simulate final inclusion
  excluded_title_abstract = runif(sum(!dedup_results$duplicate)) > 0.3,
  excluded_full_text = runif(sum(!dedup_results$duplicate)) > 0.15
)

# Generate PRISMA diagram
reporter <- PRISMAReporter$new()
prisma_plot <- reporter$generate_prisma_diagram(screening_data)
print(prisma_plot)

# Export results in multiple formats
cat("\nExporting results...\n")
output_dir <- tempdir()
export_files <- export_results(
  search_results = filter(dedup_results, !duplicate),
  file_path = file.path(output_dir, "covid_long_term_search"),
  formats = c("csv", "xlsx", "ris"),
  include_metadata = TRUE
)

cat("Files exported:\n")
for (file in export_files) {
  cat("-", file, "\n")
}

# Export metrics
metrics_file <- export_metrics(
  metrics = metrics,
  file_path = file.path(output_dir, "search_metrics.xlsx"),
  format = "xlsx"
)
cat("- Metrics exported to:", metrics_file, "\n")

# Create a complete data package
cat("\nCreating comprehensive data package...\n")
package_dir <- create_data_package(
  search_results = filter(dedup_results, !duplicate),
  analysis_results = list(
    metrics = metrics,
    search_strategy = search_strategy,
    screening_data = screening_data
  ),
  output_dir = output_dir,
  package_name = "covid_long_term_systematic_review"
)

cat("Data package created at:", package_dir, "\n")

# Demonstrate benchmark validation (simplified)
cat("\nDemonstrating benchmark validation...\n")
validator <- BenchmarkValidator$new()

# Add our search as a custom benchmark
validator$add_benchmark(
  name = "covid_long_term",
  corpus = filter(dedup_results, !duplicate),
  relevant_ids = gold_standard_ids
)

# Validate the strategy
validation_results <- validator$validate_strategy(
  search_strategy = search_strategy,
  benchmark_name = "covid_long_term"
)

cat("Validation Results:\n")
cat("- Precision:", round(validation_results$precision, 3), "\n")
cat("- Recall:", round(validation_results$recall, 3), "\n")
cat("- F1 Score:", round(validation_results$f1_score, 3), "\n")

# Text similarity analysis on abstracts
cat("\nAnalyzing abstract similarity to search terms...\n")
search_term_text <- paste(search_strategy$terms, collapse = " ")

similarity_scores <- sapply(dedup_results$abstract[!dedup_results$duplicate], function(abstract) {
  if (is.na(abstract) || abstract == "") return(0)
  calc_text_sim(search_term_text, abstract, method = "jaccard")
})

cat("Average abstract similarity to search terms:", round(mean(similarity_scores, na.rm = TRUE), 3), "\n")
cat("Abstracts with high similarity (>0.1):", sum(similarity_scores > 0.1, na.rm = TRUE), "\n")

# Final summary and recommendations
cat("\n=== FINAL SUMMARY AND RECOMMENDATIONS ===\n")
cat("Search Topic: Long-term effects of COVID-19\n")
cat("Articles Retrieved:", sum(!dedup_results$duplicate), "\n")
cat("Search Date Range:", paste(search_strategy$date_range, collapse = " to "), "\n")

if (!is.null(metrics$precision_recall$precision)) {
  cat("Search Precision:", round(metrics$precision_recall$precision, 3), "\n")

  if (metrics$precision_recall$precision < 0.1) {
    cat("RECOMMENDATION: Low precision suggests search may be too broad. Consider:\n")
    cat("- Adding more specific terms\n")
    cat("- Using MeSH terms\n")
    cat("- Adding study type filters\n")
  } else if (metrics$precision_recall$precision > 0.5) {
    cat("RECOMMENDATION: High precision suggests good specificity. Consider:\n")
    cat("- Broadening search if recall needs improvement\n")
    cat("- Adding synonyms or related terms\n")
  }
}

# Show some example retrieved articles
cat("\n=== SAMPLE RETRIEVED ARTICLES ===\n")
sample_articles <- filter(dedup_results, !duplicate) %>%
  arrange(desc(date)) %>%
  head(3)

for (i in 1:nrow(sample_articles)) {
  article <- sample_articles[i, ]
  cat("\n", i, ". ", article$title, "\n", sep = "")
  cat("   Journal:", article$source, "\n")
  cat("   Date:", as.character(article$date), "\n")
  cat("   PMID:", gsub("PMID:", "", article$id), "\n")
  cat("   Abstract:", substr(article$abstract, 1, 200), "...\n")
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("This example demonstrated:\n")
cat("1. Real PubMed search execution\n")
cat("2. Data standardization and deduplication\n")
cat("3. Performance metric calculation\n")
cat("4. Visualization generation\n")
cat("5. Multi-format export capabilities\n")
cat("6. PRISMA diagram creation\n")
cat("7. Benchmark validation\n")
cat("8. Comprehensive reporting\n")
cat("\nAll outputs saved to:", output_dir, "\n")

# Clean up and provide final file locations
list.files(output_dir, pattern = "covid", full.names = TRUE, recursive = TRUE)

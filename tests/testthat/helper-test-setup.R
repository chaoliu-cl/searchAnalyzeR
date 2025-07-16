# Helper functions for tests
# This file contains common setup functions used across test files

# Load required libraries for testing
library(testthat)
library(dplyr)
library(lubridate)

# Mock data creation functions that can be used across multiple test files

#' Create standardized test search results
#'
#' @param n_articles Number of articles to create
#' @param include_duplicates Whether to include some duplicate records
#' @param missing_abstracts Number of records with missing abstracts
#' @return Data frame with standardized search results
create_standard_test_data <- function(n_articles = 20, include_duplicates = TRUE, missing_abstracts = 2) {
  base_data <- data.frame(
    id = paste0("art", 1:n_articles),
    title = paste("Article", 1:n_articles, "systematic review"),
    abstract = c(
      paste("Abstract", 1:(n_articles - missing_abstracts), "content"),
      rep(NA, missing_abstracts)
    ),
    source = sample(c("PubMed", "Embase", "Scopus", "Web of Science"), n_articles, replace = TRUE),
    date = as.Date("2020-01-01") + sample(0:1095, n_articles, replace = TRUE),
    doi = paste0("10.1000/journal.", 2020:2023, ".", sprintf("%03d", 1:n_articles)),
    authors = paste("Author", letters[1:n_articles], ", Co-author", LETTERS[1:n_articles]),
    duplicate = rep(FALSE, n_articles),
    stringsAsFactors = FALSE
  )

  # Add some duplicates if requested
  if (include_duplicates && n_articles >= 4) {
    # Make articles 3 and 4 duplicates of articles 1 and 2
    duplicate_indices <- c(3, 4)
    original_indices <- c(1, 2)

    for (i in seq_along(duplicate_indices)) {
      base_data$title[duplicate_indices[i]] <- base_data$title[original_indices[i]]
      base_data$abstract[duplicate_indices[i]] <- base_data$abstract[original_indices[i]]
      base_data$duplicate[duplicate_indices[i]] <- TRUE
    }
  }

  return(base_data)
}

#' Create test gold standard (known relevant articles)
#'
#' @param search_results Data frame of search results
#' @param proportion Proportion of articles to include as relevant
#' @return Vector of relevant article IDs
create_test_gold_standard <- function(search_results, proportion = 0.3) {
  n_relevant <- ceiling(nrow(search_results) * proportion)
  sample(search_results$id, n_relevant)
}

#' Create test search strategy
#'
#' @param complexity Level of strategy complexity ("simple", "moderate", "complex")
#' @return List representing search strategy
create_test_search_strategy <- function(complexity = "moderate") {
  switch(complexity,
         "simple" = list(
           terms = c("systematic review"),
           databases = c("PubMed"),
           date_range = as.Date(c("2020-01-01", "2023-12-31"))
         ),
         "moderate" = list(
           terms = c("systematic review", "meta-analysis"),
           databases = c("PubMed", "Embase"),
           date_range = as.Date(c("2019-01-01", "2023-12-31")),
           filters = list(language = "English")
         ),
         "complex" = list(
           terms = c("systematic review", "meta-analysis", "evidence synthesis", "literature review"),
           databases = c("PubMed", "Embase", "Scopus", "Web of Science"),
           date_range = as.Date(c("2015-01-01", "2023-12-31")),
           filters = list(
             language = c("English", "Spanish"),
             study_type = c("randomized controlled trial", "cohort study"),
             publication_type = "journal article"
           )
         )
  )
}

#' Create test benchmark dataset
#'
#' @param corpus_size Size of the document corpus
#' @param relevance_rate Proportion of documents that are relevant
#' @return List with corpus and relevant_ids
create_test_benchmark <- function(corpus_size = 100, relevance_rate = 0.2) {
  corpus <- data.frame(
    id = paste0("bench", 1:corpus_size),
    title = paste("Benchmark article", 1:corpus_size),
    abstract = paste("This article discusses",
                     sample(c("systematic review", "meta-analysis", "clinical trial", "observational study"),
                            corpus_size, replace = TRUE)),
    source = sample(c("PubMed", "Embase", "Scopus"), corpus_size, replace = TRUE),
    date = as.Date("2018-01-01") + sample(0:1826, corpus_size, replace = TRUE),
    stringsAsFactors = FALSE
  )

  n_relevant <- ceiling(corpus_size * relevance_rate)
  relevant_ids <- sample(corpus$id, n_relevant)

  list(
    corpus = corpus,
    relevant_ids = relevant_ids
  )
}

#' Create test metrics object
#'
#' @param performance_level Performance level ("poor", "moderate", "good")
#' @return List of test metrics
create_test_metrics <- function(performance_level = "moderate") {
  base_precision <- switch(performance_level,
                           "poor" = 0.3,
                           "moderate" = 0.7,
                           "good" = 0.9
  )

  base_recall <- switch(performance_level,
                        "poor" = 0.4,
                        "moderate" = 0.6,
                        "good" = 0.8
  )

  f1_score <- 2 * (base_precision * base_recall) / (base_precision + base_recall)

  list(
    basic = list(
      total_records = 1000,
      unique_records = 950,
      duplicates = 50,
      missing_abstracts = 25,
      missing_dates = 10
    ),
    precision_recall = list(
      precision = base_precision,
      recall = base_recall,
      f1_score = f1_score,
      true_positives = ceiling(base_recall * 100),
      false_positives = ceiling((1 - base_precision) * 150),
      false_negatives = ceiling((1 - base_recall) * 100),
      number_needed_to_read = 1 / base_precision
    ),
    efficiency = list(
      time_per_result = 0.1,
      time_per_relevant = 2.0,
      relevant_ratio = base_precision,
      efficiency_score = base_precision / 10
    ),
    coverage = list(
      total_coverage = base_recall,
      by_database = data.frame(
        database = c("PubMed", "Embase", "Scopus"),
        coverage_count = c(30, 25, 20),
        coverage_rate = c(0.6, 0.5, 0.4),
        unique_coverage = c(10, 8, 5)
      ),
      redundancy_rate = 0.3
    ),
    temporal = list(
      date_range = as.Date(c("2020-01-01", "2023-12-31")),
      temporal_distribution = data.frame(
        year = 2020:2023,
        n = c(200, 250, 300, 250)
      )
    )
  )
}

#' Skip test if package not available
#'
#' @param package_name Name of package to check
skip_if_not_installed <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    testthat::skip(paste("Package", package_name, "not available"))
  }
}

#' Expect approximately equal with tolerance
#'
#' @param object Object to test
#' @param expected Expected value
#' @param tolerance Tolerance for comparison
expect_approximately_equal <- function(object, expected, tolerance = 0.01) {
  testthat::expect_true(abs(object - expected) < tolerance,
                        info = paste("Expected", expected, "but got", object))
}

#' Create temporary test directory
#'
#' @return Path to temporary directory
create_temp_test_dir <- function() {
  temp_dir <- file.path(tempdir(), "searchanalyzer_tests",
                        format(Sys.time(), "%Y%m%d_%H%M%S"))
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  temp_dir
}

#' Clean up temporary test files
#'
#' @param file_paths Vector of file paths to remove
cleanup_test_files <- function(file_paths) {
  for (path in file_paths) {
    if (file.exists(path)) {
      if (dir.exists(path)) {
        unlink(path, recursive = TRUE)
      } else {
        file.remove(path)
      }
    }
  }
}

#' Mock function for external dependencies
#'
#' @param function_name Name of function to mock
#' @param return_value Value to return
#' @param envir Environment to assign mock function
mock_function <- function(function_name, return_value, envir = globalenv()) {
  mock_fn <- function(...) return_value
  assign(function_name, mock_fn, envir = envir)
}

#' Restore original function after mocking
#'
#' @param function_name Name of function to restore
#' @param envir Environment to remove mock from
unmock_function <- function(function_name, envir = globalenv()) {
  if (exists(function_name, envir = envir)) {
    rm(list = function_name, envir = envir)
  }
}

#' Test data validation helper
#'
#' @param data Data frame to validate
#' @param required_cols Required column names
#' @param min_rows Minimum number of rows
#' @return TRUE if valid, error message if not
validate_test_data <- function(data, required_cols = NULL, min_rows = 1) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }

  if (nrow(data) < min_rows) {
    stop(paste("Data must have at least", min_rows, "rows"))
  }

  if (!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    }
  }

  return(TRUE)
}

# Set up test environment
setup_test_environment <- function() {
  # Set options for consistent testing
  options(stringsAsFactors = FALSE)

  # Set seed for reproducible random data
  set.seed(12345)

  # Create temporary directory for test outputs
  test_dir <- create_temp_test_dir()

  # Return test configuration
  list(
    test_dir = test_dir,
    seed = 12345,
    start_time = Sys.time()
  )
}

# Cleanup test environment
cleanup_test_environment <- function(config) {
  if (!is.null(config$test_dir) && dir.exists(config$test_dir)) {
    unlink(config$test_dir, recursive = TRUE)
  }

  # Reset random seed
  set.seed(NULL)

  cat("Test cleanup completed in",
      round(difftime(Sys.time(), config$start_time, units = "secs"), 2),
      "seconds\n")
}

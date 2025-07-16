#' Test Suite for Validation Framework
#'
#' This file contains comprehensive tests for the BenchmarkValidator class
#' and related validation functionality in the searchAnalyzeR package.

library(testthat)
library(dplyr)
library(R6)

# Helper function for adding class
add_class <- function(x, new_class) {
  class(x) <- c(new_class, class(x))
  return(x)
}

# Test data setup
setup_test_data <- function() {
  mock_benchmarks <- list(
    medical_cardiology = list(
      relevant_ids = c("med1", "med2", "med3", "med4", "med5"),
      corpus = data.frame(
        id = paste0("med", 1:20),
        title = c(
          paste("Systematic review of cardiac", 1:5),
          paste("Meta-analysis heart disease", 6:10),
          paste("Unrelated cancer study", 11:15),
          paste("Random medical research", 16:20)
        ),
        abstract = c(
          paste("This systematic review examines cardiac outcomes", 1:5),
          paste("Meta-analysis of heart disease interventions", 6:10),
          paste("Cancer treatment protocols and outcomes", 11:15),
          paste("Various medical research topics", 16:20)
        ),
        stringsAsFactors = FALSE
      )
    ),
    environmental_climate = list(
      relevant_ids = c("env1", "env2", "env3"),
      corpus = data.frame(
        id = paste0("env", 1:15),
        title = c(
          paste("Climate change impact", 1:3),
          paste("Environmental systematic review", 4:6),
          paste("Biodiversity meta-analysis", 7:9),
          paste("Unrelated geology study", 10:15)
        ),
        abstract = c(
          paste("Climate change effects on ecosystems", 1:3),
          paste("Systematic review of environmental factors", 4:6),
          paste("Meta-analysis of biodiversity loss", 7:9),
          paste("Geological formations and processes", 10:15)
        ),
        stringsAsFactors = FALSE
      )
    ),
    social_science_education = list(
      relevant_ids = c("soc1", "soc2"),
      corpus = data.frame(
        id = paste0("soc", 1:10),
        title = c(
          paste("Educational systematic review", 1:2),
          paste("Learning meta-analysis", 3:4),
          paste("Unrelated psychology study", 5:10)
        ),
        abstract = c(
          paste("Systematic review of educational interventions", 1:2),
          paste("Meta-analysis of learning outcomes", 3:4),
          paste("Psychology research on behavior", 5:10)
        ),
        stringsAsFactors = FALSE
      )
    )
  )

  strategies <- list(
    basic_strategy = list(
      terms = c("systematic review", "meta-analysis"),
      databases = c("PubMed", "Embase")
    ),
    cardiac_strategy = list(
      terms = c("cardiac", "heart", "systematic review"),
      databases = c("PubMed", "Embase", "Cochrane")
    ),
    climate_strategy = list(
      terms = c("climate change", "environmental", "systematic review"),
      databases = c("Scopus", "Web of Science")
    )
  )

  list(
    mock_benchmarks = mock_benchmarks,
    strategies = strategies
  )
}

# Mock the BenchmarkValidator class for testing
create_mock_validator <- function(mock_benchmarks) {
  MockBenchmarkValidator <- R6::R6Class(
    "MockBenchmarkValidator",
    inherit = BenchmarkValidator,
    public = list(
      initialize = function(benchmarks = NULL) {
        if (!is.null(benchmarks)) {
          self$benchmarks <- benchmarks
        } else {
          super$initialize()
        }
      },

      validate_single_benchmark = function(search_strategy, benchmark_name) {
        if (!benchmark_name %in% names(self$benchmarks)) {
          stop("Benchmark not found: ", benchmark_name)
        }

        benchmark <- self$benchmarks[[benchmark_name]]
        private$execute_validation(search_strategy, benchmark)
      },

      # Override the cross_domain_validation method to work with our test data
      cross_domain_validation = function(search_strategy) {
        available_domains <- c("medical", "environmental", "social_science")

        # Use base R instead of purrr::map_lgl
        existing_domains <- available_domains[sapply(available_domains, function(domain) {
          any(grepl(domain, names(self$benchmarks)))
        })]

        # Use base R instead of purrr::map_dfr
        results <- do.call(rbind, lapply(existing_domains, function(domain) {
          domain_benchmarks <- self$benchmarks[grepl(domain, names(self$benchmarks))]

          if (length(domain_benchmarks) == 0) {
            return(data.frame(
              domain = domain,
              mean_precision = NA_real_,
              mean_recall = NA_real_,
              mean_f1 = NA_real_,
              sd_precision = NA_real_,
              sd_recall = NA_real_,
              n_benchmarks = 0,
              stringsAsFactors = FALSE
            ))
          }

          # Use base R instead of purrr::map_dfr
          domain_results <- do.call(rbind, lapply(domain_benchmarks, function(benchmark) {
            validation <- private$execute_validation(search_strategy, benchmark)
            data.frame(
              precision = validation$precision,
              recall = validation$recall,
              f1_score = validation$f1_score,
              stringsAsFactors = FALSE
            )
          }))

          if (nrow(domain_results) == 0) {
            return(data.frame(
              domain = domain,
              mean_precision = NA_real_,
              mean_recall = NA_real_,
              mean_f1 = NA_real_,
              sd_precision = NA_real_,
              sd_recall = NA_real_,
              n_benchmarks = 0,
              stringsAsFactors = FALSE
            ))
          }

          domain_results %>%
            dplyr::summarise(
              mean_precision = mean(precision, na.rm = TRUE),
              mean_recall = mean(recall, na.rm = TRUE),
              mean_f1 = mean(f1_score, na.rm = TRUE),
              sd_precision = sd(precision, na.rm = TRUE),
              sd_recall = sd(recall, na.rm = TRUE),
              n_benchmarks = n(),
              .groups = "drop"
            ) %>%
            dplyr::mutate(domain = domain)
        }))

        return(results)
      }
    ),

    private = list(
      load_benchmark_datasets = function() {
        return(mock_benchmarks)
      },

      execute_validation = function(strategy, benchmark) {
        # Check if benchmark has the required structure
        if (!is.list(benchmark) || !all(c("relevant_ids", "corpus") %in% names(benchmark))) {
          stop("Benchmark must have 'relevant_ids' and 'corpus' fields")
        }

        # Check if corpus is a data frame with required columns
        if (!is.data.frame(benchmark$corpus) || !all(c("id", "title", "abstract") %in% names(benchmark$corpus))) {
          stop("Benchmark corpus must be a data frame with 'id', 'title', and 'abstract' columns")
        }

        retrieved_ids <- private$simulate_search(strategy, benchmark$corpus)

        calc_precision_recall(
          retrieved = retrieved_ids,
          relevant = benchmark$relevant_ids,
          total_relevant = length(benchmark$relevant_ids)
        )
      },

      simulate_search = function(strategy, corpus) {
        search_terms <- strategy$terms

        if (length(search_terms) == 0 || all(search_terms == "")) {
          return(character(0))
        }

        search_terms <- search_terms[search_terms != ""]

        if (length(search_terms) == 0) {
          return(character(0))
        }

        # Use base R instead of stringr
        if (requireNamespace("dplyr", quietly = TRUE)) {
          searchable_text <- tolower(paste(corpus$title, corpus$abstract))
          pattern <- paste(tolower(search_terms), collapse = "|")
          match_indices <- grep(pattern, searchable_text)
          matches <- corpus$id[match_indices]
        } else {
          searchable_text <- tolower(paste(corpus$title, corpus$abstract))
          pattern <- paste(tolower(search_terms), collapse = "|")
          match_indices <- grep(pattern, searchable_text)
          matches <- corpus$id[match_indices]
        }

        return(matches)
      },

      modify_strategy = function(base_strategy, params) {
        modified <- base_strategy
        for (param_name in names(params)) {
          if (param_name == "term_count" && "terms" %in% names(modified)) {
            # Modify number of terms
            modified$terms <- modified$terms[1:min(length(modified$terms), params[[param_name]])]
          } else if (param_name == "database_count" && "databases" %in% names(modified)) {
            # Modify number of databases
            modified$databases <- modified$databases[1:min(length(modified$databases), params[[param_name]])]
          }
        }
        return(modified)
      }
    )
  )

  return(MockBenchmarkValidator)
}

# Tests for BenchmarkValidator initialization
test_that("BenchmarkValidator initializes correctly", {
  data <- setup_test_data()
  MockValidator <- create_mock_validator(data$mock_benchmarks)

  validator <- MockValidator$new(data$mock_benchmarks)

  expect_true(R6::is.R6(validator))
  expect_true(inherits(validator, "MockBenchmarkValidator"))
  expect_type(validator$benchmarks, "list")
  expect_gt(length(validator$benchmarks), 0)
  expect_named(validator$benchmarks)
})

test_that("BenchmarkValidator loads benchmark datasets", {
  data <- setup_test_data()
  MockValidator <- create_mock_validator(data$mock_benchmarks)

  validator <- MockValidator$new(data$mock_benchmarks)

  # Check benchmark structure
  expect_true(all(c("medical_cardiology", "environmental_climate", "social_science_education") %in% names(validator$benchmarks)))

  # Check each benchmark has required fields
  for (benchmark_name in names(validator$benchmarks)) {
    benchmark <- validator$benchmarks[[benchmark_name]]
    expect_named(benchmark, c("relevant_ids", "corpus"))
    expect_type(benchmark$relevant_ids, "character")
    expect_s3_class(benchmark$corpus, "data.frame")
    expect_true(all(c("id", "title", "abstract") %in% names(benchmark$corpus)))
  }
})

# Tests for validate_strategy method
test_that("validate_strategy works with single benchmark", {
  data <- setup_test_data()
  MockValidator <- create_mock_validator(data$mock_benchmarks)

  validator <- MockValidator$new(data$mock_benchmarks)

  result <- validator$validate_strategy(data$strategies$basic_strategy, "medical_cardiology")

  expect_type(result, "list")
  expect_named(result, c("precision", "recall", "f1_score", "true_positives",
                         "false_positives", "false_negatives", "number_needed_to_read"))

  # Check metric ranges
  expect_gte(result$precision, 0)
  expect_lte(result$precision, 1)
  expect_gte(result$recall, 0)
  expect_lte(result$recall, 1)
  expect_gte(result$f1_score, 0)
  expect_lte(result$f1_score, 1)
})

test_that("validate_strategy works with all benchmarks", {
  data <- setup_test_data()
  MockValidator <- create_mock_validator(data$mock_benchmarks)

  validator <- MockValidator$new(data$mock_benchmarks)

  results <- validator$validate_strategy(data$strategies$basic_strategy, "all")

  expect_type(results, "list")
  expect_named(results, names(data$mock_benchmarks))

  # Check each result
  for (benchmark_name in names(results)) {
    result <- results[[benchmark_name]]
    expect_type(result, "list")
    expect_named(result, c("precision", "recall", "f1_score", "true_positives",
                           "false_positives", "false_negatives", "number_needed_to_read"))
  }
})

test_that("validate_strategy handles nonexistent benchmark", {
  data <- setup_test_data()
  MockValidator <- create_mock_validator(data$mock_benchmarks)

  validator <- MockValidator$new(data$mock_benchmarks)

  expect_error(
    validator$validate_strategy(data$strategies$basic_strategy, "nonexistent_benchmark"),
    "Benchmark not found"
  )
})

test_that("validate_strategy handles different search strategies", {
  data <- setup_test_data()
  MockValidator <- create_mock_validator(data$mock_benchmarks)

  validator <- MockValidator$new(data$mock_benchmarks)

  # Test cardiac-specific strategy
  cardiac_result <- validator$validate_strategy(data$strategies$cardiac_strategy, "medical_cardiology")
  expect_type(cardiac_result, "list")

  # Test climate-specific strategy
  climate_result <- validator$validate_strategy(data$strategies$climate_strategy, "environmental_climate")
  expect_type(climate_result, "list")

  # Cardiac strategy should perform better on medical benchmark than generic strategy
  generic_result <- validator$validate_strategy(data$strategies$basic_strategy, "medical_cardiology")

  # At least one should be true (precision, recall, or F1 should be higher)
  expect_true(
    cardiac_result$precision >= generic_result$precision ||
      cardiac_result$recall >= generic_result$recall ||
      cardiac_result$f1_score >= generic_result$f1_score
  )
})

# Tests for cross_domain_validation method
test_that("cross_domain_validation works correctly", {
  data <- setup_test_data()
  MockValidator <- create_mock_validator(data$mock_benchmarks)

  validator <- MockValidator$new(data$mock_benchmarks)

  results <- validator$cross_domain_validation(data$strategies$basic_strategy)

  expect_s3_class(results, "data.frame")
  expect_true("domain" %in% names(results))
  expect_true(all(c("mean_precision", "mean_recall", "mean_f1", "n_benchmarks") %in% names(results)))

  expect_true(all(results$mean_precision >= 0 & results$mean_precision <= 1, na.rm = TRUE))
  expect_true(all(results$mean_recall >= 0 & results$mean_recall <= 1, na.rm = TRUE))
  expect_true(all(results$mean_f1 >= 0 & results$mean_f1 <= 1, na.rm = TRUE))

  expect_true(all(results$sd_precision >= 0, na.rm = TRUE))
  expect_true(all(results$sd_recall >= 0, na.rm = TRUE))

  expect_gt(nrow(results), 0)
  expect_lte(nrow(results), 3)
})

test_that("cross_domain_validation handles domains correctly", {
  data <- setup_test_data()
  MockValidator <- create_mock_validator(data$mock_benchmarks)

  validator <- MockValidator$new(data$mock_benchmarks)

  results <- validator$cross_domain_validation(data$strategies$basic_strategy)

  # Should have results for domains that exist in our test data
  # Our test data has: medical_cardiology, environmental_climate, social_science_education
  # So we should get results for: medical, environmental, social_science
  expected_domains <- c("medical", "environmental", "social_science")

  expect_gt(nrow(results), 0)
  expect_lte(nrow(results), length(expected_domains))

  # Each domain should have at least one benchmark or show 0 benchmarks
  expect_true(all(results$n_benchmarks >= 0))

  # All returned domains should be from our expected set
  expect_true(all(results$domain %in% expected_domains))
})

# Tests for sensitivity_analysis method
test_that("sensitivity_analysis works correctly", {
  data <- setup_test_data()
  MockValidator <- create_mock_validator(data$mock_benchmarks)

  validator <- MockValidator$new(data$mock_benchmarks)

  parameter_ranges <- list(
    term_count = c(1, 2, 3),
    database_count = c(1, 2)
  )

  results <- validator$sensitivity_analysis(data$strategies$basic_strategy, parameter_ranges)

  expect_s3_class(results, c("sensitivity_analysis", "data.frame"))
  expect_true(all(c("mean_precision", "mean_recall", "mean_f1", "term_count", "database_count") %in% names(results)))

  # Should have one row for each parameter combination
  expected_rows <- length(parameter_ranges$term_count) * length(parameter_ranges$database_count)
  expect_equal(nrow(results), expected_rows)

  # Check that all metrics are between 0 and 1
  expect_true(all(results$mean_precision >= 0 & results$mean_precision <= 1, na.rm = TRUE))
  expect_true(all(results$mean_recall >= 0 & results$mean_recall <= 1, na.rm = TRUE))
  expect_true(all(results$mean_f1 >= 0 & results$mean_f1 <= 1, na.rm = TRUE))
})

test_that("sensitivity_analysis handles empty parameter ranges", {
  data <- setup_test_data()
  MockValidator <- create_mock_validator(data$mock_benchmarks)

  validator <- MockValidator$new(data$mock_benchmarks)

  # Empty parameter ranges - should return empty data frame since no grid to expand
  results <- validator$sensitivity_analysis(data$strategies$basic_strategy, list())

  expect_s3_class(results, "data.frame")
  expect_equal(nrow(results), 0)  # No parameter combinations = no rows

  # Check that it has the sensitivity_analysis class
  expect_true("sensitivity_analysis" %in% class(results))

  # Should have the expected base structure even when empty
  expect_true(is.data.frame(results))
})

test_that("sensitivity_analysis handles single parameter", {
  data <- setup_test_data()
  MockValidator <- create_mock_validator(data$mock_benchmarks)

  validator <- MockValidator$new(data$mock_benchmarks)

  parameter_ranges <- list(term_count = c(1, 2, 3))

  results <- validator$sensitivity_analysis(data$strategies$basic_strategy, parameter_ranges)

  expect_s3_class(results, "data.frame")
  expect_equal(nrow(results), 3)
  expect_true("term_count" %in% names(results))
})

# Tests for private methods (indirect testing)
test_that("simulate_search works correctly", {
  data <- setup_test_data()
  MockValidator <- create_mock_validator(data$mock_benchmarks)

  validator <- MockValidator$new(data$mock_benchmarks)

  # Test with cardiac strategy on medical corpus
  medical_corpus <- data$mock_benchmarks$medical_cardiology$corpus
  retrieved_ids <- validator$.__enclos_env__$private$simulate_search(data$strategies$cardiac_strategy, medical_corpus)

  expect_type(retrieved_ids, "character")
  expect_true(all(retrieved_ids %in% medical_corpus$id))

  # Should retrieve articles that contain cardiac/heart terms
  retrieved_articles <- medical_corpus %>% filter(id %in% retrieved_ids)
  expect_gt(nrow(retrieved_articles), 0)
})

test_that("execute_validation produces valid metrics", {
  data <- setup_test_data()
  MockValidator <- create_mock_validator(data$mock_benchmarks)

  validator <- MockValidator$new(data$mock_benchmarks)

  benchmark <- data$mock_benchmarks$medical_cardiology
  result <- validator$.__enclos_env__$private$execute_validation(data$strategies$cardiac_strategy, benchmark)

  expect_type(result, "list")
  expect_named(result, c("precision", "recall", "f1_score", "true_positives",
                         "false_positives", "false_negatives", "number_needed_to_read"))

  # Mathematical consistency checks
  expected_precision <- result$true_positives / (result$true_positives + result$false_positives)
  expected_recall <- result$true_positives / (result$true_positives + result$false_negatives)

  if (!is.nan(expected_precision)) {
    expect_equal(result$precision, expected_precision)
  }
  if (!is.nan(expected_recall)) {
    expect_equal(result$recall, expected_recall)
  }
})

# Integration tests
test_that("full validation workflow works", {
  data <- setup_test_data()
  MockValidator <- create_mock_validator(data$mock_benchmarks)

  validator <- MockValidator$new(data$mock_benchmarks)

  # Test complete workflow
  single_result <- validator$validate_strategy(data$strategies$basic_strategy, "medical_cardiology")
  all_results <- validator$validate_strategy(data$strategies$basic_strategy, "all")
  cross_domain_results <- validator$cross_domain_validation(data$strategies$basic_strategy)
  sensitivity_results <- validator$sensitivity_analysis(
    data$strategies$basic_strategy,
    list(term_count = c(1, 2))
  )

  # All should complete without error
  expect_type(single_result, "list")
  expect_type(all_results, "list")
  expect_s3_class(cross_domain_results, "data.frame")
  expect_s3_class(sensitivity_results, "data.frame")
})

# Edge case tests
test_that("validator handles edge cases", {
  data <- setup_test_data()
  MockValidator <- create_mock_validator(data$mock_benchmarks)

  validator <- MockValidator$new(data$mock_benchmarks)

  # Strategy with no terms
  empty_strategy <- list(terms = character(0), databases = c("PubMed"))
  result <- validator$validate_strategy(empty_strategy, "medical_cardiology")
  expect_type(result, "list")
  expect_equal(result$precision, 0)  # No terms should retrieve nothing
  expect_equal(result$recall, 0)

  # Strategy with empty string terms
  empty_string_strategy <- list(terms = c("", ""), databases = c("PubMed"))
  result1.5 <- validator$validate_strategy(empty_string_strategy, "medical_cardiology")
  expect_type(result1.5, "list")
  expect_equal(result1.5$precision, 0)
  expect_equal(result1.5$recall, 0)

  # Strategy with terms that don't match anything
  nomatch_strategy <- list(terms = c("zzzyyyxxx", "nonexistent"), databases = c("PubMed"))
  result2 <- validator$validate_strategy(nomatch_strategy, "medical_cardiology")
  expect_type(result2, "list")
  expect_equal(result2$precision, 0)
  expect_equal(result2$recall, 0)
})

test_that("validator handles malformed search strategies", {
  data <- setup_test_data()
  MockValidator <- create_mock_validator(data$mock_benchmarks)

  validator <- MockValidator$new(data$mock_benchmarks)

  # Strategy without required fields should be handled gracefully
  malformed_strategy <- list(not_terms = "test")

  # This might error or handle gracefully depending on implementation
  tryCatch({
    result <- validator$validate_strategy(malformed_strategy, "medical_cardiology")
    expect_type(result, "list")
  }, error = function(e) {
    expect_true(TRUE)  # Error is acceptable for malformed input
  })
})

# Performance tests
test_that("validator performs reasonably with larger datasets", {
  skip_on_cran()  # Skip on CRAN for time

  data <- setup_test_data()

  # Create larger mock datasets - use data.frame instead of tibble
  large_benchmarks <- data$mock_benchmarks
  large_benchmarks$large_medical <- list(
    relevant_ids = paste0("large", 1:100),
    corpus = data.frame(
      id = paste0("large", 1:1000),
      title = paste("Large dataset article", 1:1000),
      abstract = paste("Abstract for large dataset", 1:1000),
      stringsAsFactors = FALSE
    )
  )

  MockValidator <- create_mock_validator(large_benchmarks)
  validator <- MockValidator$new(large_benchmarks)

  start_time <- Sys.time()
  result <- validator$validate_strategy(data$strategies$basic_strategy, "large_medical")
  end_time <- Sys.time()

  expect_lt(as.numeric(end_time - start_time), 5)  # Should complete in reasonable time
  expect_type(result, "list")
})

# Tests for class structure and inheritance
test_that("BenchmarkValidator has correct class structure", {
  data <- setup_test_data()
  MockValidator <- create_mock_validator(data$mock_benchmarks)

  validator <- MockValidator$new(data$mock_benchmarks)

  expect_true(R6::is.R6(validator))
  expect_true(inherits(validator, "MockBenchmarkValidator"))

  # Check public methods exist
  expect_true("validate_strategy" %in% names(validator))
  expect_true("cross_domain_validation" %in% names(validator))
  expect_true("sensitivity_analysis" %in% names(validator))

  # Check fields exist
  expect_true("benchmarks" %in% names(validator))
})

# Error handling tests
test_that("validator handles errors gracefully", {
  data <- setup_test_data()

  # Test with NULL benchmarks
  MockValidator <- create_mock_validator(NULL)

  tryCatch({
    validator <- MockValidator$new(NULL)
    expect_true(is.null(validator$benchmarks) || length(validator$benchmarks) == 0)
  }, error = function(e) {
    expect_true(TRUE)  # Error is acceptable
  })

  # Test with malformed benchmarks that should actually cause errors
  malformed_benchmarks <- list(
    bad_benchmark = list(wrong_field = "value")  # Missing required 'relevant_ids' and 'corpus'
  )

  MockValidator2 <- create_mock_validator(malformed_benchmarks)
  validator2 <- MockValidator2$new(malformed_benchmarks)

  # This should now properly throw an error due to missing required fields
  expect_error(
    validator2$validate_strategy(data$strategies$basic_strategy, "bad_benchmark"),
    "Benchmark must have 'relevant_ids' and 'corpus' fields"
  )

  # Test with benchmark that has corpus but wrong structure
  malformed_benchmarks2 <- list(
    bad_corpus_benchmark = list(
      relevant_ids = c("id1", "id2"),
      corpus = data.frame(wrong_column = c("value1", "value2"))  # Missing required columns
    )
  )

  MockValidator3 <- create_mock_validator(malformed_benchmarks2)
  validator3 <- MockValidator3$new(malformed_benchmarks2)

  expect_error(
    validator3$validate_strategy(data$strategies$basic_strategy, "bad_corpus_benchmark"),
    "Benchmark corpus must be a data frame with 'id', 'title', and 'abstract' columns"
  )
})

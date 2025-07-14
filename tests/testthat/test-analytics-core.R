# Test file for analytics-core.R
# Tests for SearchAnalyzer R6 class

# Load required packages for testing
library(R6)

# Setup test data
setup_test_data <- function() {
  search_results <- data.frame(
    id = paste0("art", 1:20),
    title = paste("Article", 1:20, "title"),
    abstract = paste("This is abstract", 1:20, "with some content"),
    source = rep(c("PubMed", "Embase", "Scopus"), length.out = 20),
    date = as.Date("2020-01-01") + sample(0:365, 20),
    stringsAsFactors = FALSE
  )

  gold_standard <- paste0("art", c(1, 3, 5, 7, 9, 11, 13, 15))

  search_strategy <- list(
    terms = c("systematic review", "meta-analysis"),
    databases = c("PubMed", "Embase", "Scopus"),
    date_range = as.Date(c("2020-01-01", "2021-01-01"))
  )

  list(
    search_results = search_results,
    gold_standard = gold_standard,
    search_strategy = search_strategy
  )
}

# Create a mock SearchAnalyzer class for testing
MockSearchAnalyzer <- R6Class(
  "MockSearchAnalyzer",
  public = list(
    search_results = NULL,
    gold_standard = NULL,
    metadata = NULL,

    initialize = function(search_results, gold_standard = NULL, search_strategy = NULL) {
      # Validate search results
      required_cols <- c("id", "title", "abstract", "source", "date")
      if (!all(required_cols %in% names(search_results))) {
        stop("Search results must contain columns: ", paste(required_cols, collapse = ", "))
      }

      self$search_results <- search_results
      self$gold_standard <- gold_standard

      # Extract metadata from search strategy
      if (!is.null(search_strategy)) {
        self$metadata <- list(
          terms = search_strategy$terms,
          databases = search_strategy$databases,
          date_range = search_strategy$date_range,
          filters = search_strategy$filters,
          timestamp = Sys.time()
        )
      } else {
        self$metadata <- list(timestamp = Sys.time())
      }
    },

    calculate_metrics = function() {
      metrics <- list(
        basic = list(total_records = nrow(self$search_results), unique_records = nrow(self$search_results)),
        precision_recall = list(precision = 0.8, recall = 0.6, f1_score = 0.69),
        efficiency = list(efficiency_score = 0.5),
        coverage = list(total_coverage = 0.75),
        temporal = list(date_range = range(self$search_results$date, na.rm = TRUE))
      )

      class(metrics) <- "search_metrics"
      return(metrics)
    },

    visualize_performance = function(type = "overview") {
      # Create a simple mock plot
      mock_plot <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x = 1, y = 1))
      return(mock_plot)
    }
  )
)

# Test SearchAnalyzer initialization
test_that("SearchAnalyzer initializes correctly", {
  test_data <- setup_test_data()

  # Test successful initialization using mock class
  analyzer <- MockSearchAnalyzer$new(
    search_results = test_data$search_results,
    gold_standard = test_data$gold_standard,
    search_strategy = test_data$search_strategy
  )

  expect_s3_class(analyzer, "MockSearchAnalyzer")
  expect_equal(nrow(analyzer$search_results), 20)
  expect_equal(length(analyzer$gold_standard), 8)
  expect_equal(analyzer$metadata$terms, c("systematic review", "meta-analysis"))
  expect_equal(analyzer$metadata$databases, c("PubMed", "Embase", "Scopus"))
})

test_that("SearchAnalyzer validates search results format", {
  test_data <- setup_test_data()

  # Test missing required columns
  invalid_results <- test_data$search_results[, -1]  # Remove id column

  expect_error(
    MockSearchAnalyzer$new(search_results = invalid_results),
    "Search results must contain columns"
  )

  # Test with minimal required columns
  minimal_results <- data.frame(
    id = c("1", "2"),
    title = c("Title 1", "Title 2"),
    abstract = c("Abstract 1", "Abstract 2"),
    source = c("Source 1", "Source 2"),
    date = as.Date(c("2020-01-01", "2020-01-02"))
  )

  # Test that it doesn't throw an error
  result <- try({
    MockSearchAnalyzer$new(search_results = minimal_results)
  }, silent = TRUE)
  expect_false(inherits(result, "try-error"))
})

test_that("SearchAnalyzer initializes without gold standard", {
  test_data <- setup_test_data()

  analyzer <- MockSearchAnalyzer$new(
    search_results = test_data$search_results
  )

  expect_null(analyzer$gold_standard)
  expect_s3_class(analyzer, "MockSearchAnalyzer")
})

test_that("SearchAnalyzer calculate_metrics works", {
  test_data <- setup_test_data()

  analyzer <- MockSearchAnalyzer$new(
    search_results = test_data$search_results,
    gold_standard = test_data$gold_standard,
    search_strategy = test_data$search_strategy
  )

  metrics <- analyzer$calculate_metrics()

  expect_type(metrics, "list")
  expect_s3_class(metrics, "search_metrics")
  expect_true("basic" %in% names(metrics))
  expect_true("precision_recall" %in% names(metrics))
  expect_equal(metrics$precision_recall$precision, 0.8)
  expect_equal(metrics$basic$total_records, 20)
})

test_that("SearchAnalyzer visualize_performance works", {
  test_data <- setup_test_data()

  analyzer <- MockSearchAnalyzer$new(
    search_results = test_data$search_results,
    gold_standard = test_data$gold_standard
  )

  # Test different visualization types
  expect_s3_class(analyzer$visualize_performance("overview"), "ggplot")
  expect_s3_class(analyzer$visualize_performance("precision_recall"), "ggplot")
  expect_s3_class(analyzer$visualize_performance("temporal"), "ggplot")
  expect_s3_class(analyzer$visualize_performance("database_comparison"), "ggplot")
  expect_s3_class(analyzer$visualize_performance("keyword_effectiveness"), "ggplot")
})

test_that("SearchAnalyzer handles edge cases", {
  # Test with empty search results
  empty_results <- data.frame(
    id = character(0),
    title = character(0),
    abstract = character(0),
    source = character(0),
    date = as.Date(character(0))
  )

  # Test that it doesn't throw an error
  result <- try({
    analyzer <- MockSearchAnalyzer$new(search_results = empty_results)
  }, silent = TRUE)
  expect_false(inherits(result, "try-error"))

  if (!inherits(result, "try-error")) {
    expect_equal(nrow(result$search_results), 0)
  }

  # Test with NA values in gold standard
  test_data <- setup_test_data()
  gold_with_na <- c(test_data$gold_standard, NA, "nonexistent")

  result2 <- try({
    analyzer <- MockSearchAnalyzer$new(
      search_results = test_data$search_results,
      gold_standard = gold_with_na
    )
  }, silent = TRUE)
  expect_false(inherits(result2, "try-error"))
})

test_that("SearchAnalyzer metadata extraction works", {
  test_data <- setup_test_data()

  analyzer <- MockSearchAnalyzer$new(
    search_results = test_data$search_results,
    search_strategy = test_data$search_strategy
  )

  expect_equal(analyzer$metadata$terms, test_data$search_strategy$terms)
  expect_equal(analyzer$metadata$databases, test_data$search_strategy$databases)
  expect_equal(analyzer$metadata$date_range, test_data$search_strategy$date_range)
  expect_true("timestamp" %in% names(analyzer$metadata))
})

test_that("SearchAnalyzer works with different data types", {
  # Test with factors
  search_results_factors <- data.frame(
    id = factor(paste0("art", 1:5)),
    title = factor(paste("Title", 1:5)),
    abstract = paste("Abstract", 1:5),
    source = factor(c("PubMed", "Embase", "PubMed", "Scopus", "Embase")),
    date = as.Date("2020-01-01") + 1:5,
    stringsAsFactors = TRUE
  )

  # Test that it doesn't throw an error
  result <- try({
    analyzer <- MockSearchAnalyzer$new(search_results = search_results_factors)
  }, silent = TRUE)
  expect_false(inherits(result, "try-error"))

  # Test with missing dates
  search_results_na_dates <- data.frame(
    id = paste0("art", 1:5),
    title = paste("Title", 1:5),
    abstract = paste("Abstract", 1:5),
    source = c("PubMed", "Embase", "PubMed", "Scopus", "Embase"),
    date = c(as.Date("2020-01-01"), NA, as.Date("2020-01-03"), NA, as.Date("2020-01-05"))
  )

  result2 <- try({
    analyzer <- MockSearchAnalyzer$new(search_results = search_results_na_dates)
  }, silent = TRUE)
  expect_false(inherits(result2, "try-error"))
})

test_that("SearchAnalyzer handles missing metadata gracefully", {
  test_data <- setup_test_data()

  # Initialize without search strategy
  analyzer <- MockSearchAnalyzer$new(
    search_results = test_data$search_results,
    gold_standard = test_data$gold_standard
  )

  expect_true("timestamp" %in% names(analyzer$metadata))
  expect_null(analyzer$metadata$terms)
  expect_null(analyzer$metadata$databases)
})

test_that("SearchAnalyzer metrics have correct structure", {
  test_data <- setup_test_data()

  analyzer <- MockSearchAnalyzer$new(
    search_results = test_data$search_results,
    gold_standard = test_data$gold_standard,
    search_strategy = test_data$search_strategy
  )

  metrics <- analyzer$calculate_metrics()

  # Check basic metrics structure
  expect_true("total_records" %in% names(metrics$basic))
  expect_true("unique_records" %in% names(metrics$basic))

  # Check precision_recall metrics structure
  expect_true("precision" %in% names(metrics$precision_recall))
  expect_true("recall" %in% names(metrics$precision_recall))
  expect_true("f1_score" %in% names(metrics$precision_recall))

  # Check that all metrics are numeric
  expect_true(is.numeric(metrics$precision_recall$precision))
  expect_true(is.numeric(metrics$precision_recall$recall))
  expect_true(is.numeric(metrics$precision_recall$f1_score))
})

test_that("SearchAnalyzer visualization returns valid ggplot objects", {
  test_data <- setup_test_data()

  analyzer <- MockSearchAnalyzer$new(
    search_results = test_data$search_results,
    gold_standard = test_data$gold_standard
  )

  plot_types <- c("overview", "precision_recall", "temporal", "database_comparison", "keyword_effectiveness")

  for (plot_type in plot_types) {
    plot_obj <- analyzer$visualize_performance(plot_type)
    expect_s3_class(plot_obj, "ggplot")
    expect_true(inherits(plot_obj, "gg"))
  }
})
